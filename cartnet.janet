(import path)
(import uri)

(defn int [num] 
    (- num (mod num 1))
)

(def alphabet (string 
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789"))

(defn random-id [&opt len] 
    (default len 10)
    (string/from-bytes (splice (seq [x :range (0 len)]
        (alphabet (int (* 35 (math/random)) ))
    )))
)

(defn tempdir [] 
    (def osw (os/which))
    (or 
        (and (= osw :windows) (os/getenv "TMP"))
        (and (= osw :windows) (os/getenv "TEMP"))
        (and (= osw :windows) (os/getenv "USERPROFILE"))
        (and (= osw :linux)    "/tmp")
        (error "Could not find a temp dir!")
    )
)

(defn file-in-tempdir [& name] 
    (path/join (tempdir) (string (splice name)))
)

(defn open-tempfile [name]
    (def tpath (path/join (tempdir) name))
    (def f (file/open tpath :w))
    [tpath f]
)

(defn- finish-file [f path]
    (file/flush f)
    (slurp path)
)

(defn- cleanup [f path]
    (file/close f)
    (os/rm path)
)

(defn header-val [val] 
    (match (type val)
        :string val
        :buffer (string val)
        :keyword (string val)
        _ (error "Cannot write hader")
    )
)

(defn write-headers-file [path headers] 
    (def hfile (file/open path :w))
    (defer (do (file/close hfile))
        (eachp (k v) headers 
            (:write hfile (header-val k))
            (:write hfile ": ")
            (:write hfile (header-val v))
            (:write hfile "\n")
        )
    )
)

(defn cleanup [spec] 
    (each cl-spec spec 
        (when-let [f (cl-spec :file)] 
            (file/flush f)
            (file/close f)
        )
        (when-let [p (cl-spec :path)]
            (os/rm p)
        )
    )
)


(defn http-request [verb url &opt spec] 
    ```
    
    ```
    (def cmd-args @["curl"])
    (defn add-arg [& args] 
        (array/concat cmd-args args))
        
    # Make curl not show progress bars (-s) but still show errors in stderr (-S)
    (add-arg "-sS")
        
    # Set things up for the different HTTP verbs
    (match verb 
        "GET" (add-arg "-G")
        "POST" (add-arg "-X" "POST")
        "PUT" (add-arg "-X" "PUT")
        "PATCH" (add-arg "-X" "PATCH")
        "DELETE" (add-arg "-X" "DELETE")
        "OPTIONS" (add-arg "-X" "OPTIONS")
        # Tell CURL to do a head request only
        "HEAD" (add-arg "-I")
        _ (error (string "Unexpected verb " verb))
    )
    (add-arg url)
    
    (def tempfiles @[])
    (defn cleanup-temp [& tempfile]
        (array/concat tempfiles tempfile)
    )
        
    (def req-id (random-id 24))
    (when-let [spec spec] 
        (when-let [headers (spec :headers)]
            (def tpath (string req-id ".headers.txt"))
            (write-headers-file tpath headers)
            (cleanup-temp {:path tpath})
            (add-arg "-H" (string "@" tpath))
        )
        
        (when-let [postdata (spec :post-body)] 
            (def datapath (string req-id ".postdata.txt"))
            (spit datapath postdata)
            (cleanup-temp {:path datapath})
            (add-arg  "-d" (string "@" datapath))
        )
        
        (when-let [data-args (spec :data-args)] 
            (def kvbuf @"")
            (eachp (k v) data-args 
                (buffer/push kvbuf (uri/escape (string k)) "=" (uri/escape (string v)) "&")
            )
            # Cut off the last "&", it should only be a single byte
            (add-arg "-d" (string (buffer/popn kvbuf 1)))
        )
    )
    
    
    (def [outpath outfile] (open-tempfile (string req-id  ".out.txt")))
    (def [errpath errfile] (open-tempfile (string req-id ".err.txt")))
    (cleanup-temp {:path outpath :file outfile} {:path errpath :file errfile})
    
    (def retval (match (os/execute cmd-args :p { :out outfile :err errfile }) 
        0 [:ok (finish-file outfile outpath)]
        _ [:err (finish-file errfile errpath)]
    ))
    (cleanup tempfiles)
    retval
)

(defn http-get 
    [url &opt spec]
    "Issues a GET request with the given request spec"
    (http-request "GET" url spec))
    
(defn http-post [url &opt spec]
    (http-request "POST" url spec))

(defmacro- throwing-func [name wrapped doc] 
    ~(defn ,name ,doc [url &opt spec]
        (match (,wrapped url spec)
            [:ok response] response
            [:err err] (error err)
        )
    )
)

(throwing-func http-get! http-get "Like http-get, but errors on failure, instead of returning [:err err]")
(throwing-func http-post! http-post "Like http-post, but errors on failure, instead of returning [:err err]")

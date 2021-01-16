(import path)

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
    (or 
        (os/getenv "USERPROFILE")
        (os/getenv "TEMP")
        (os/getenv "TMP")
        (error "Could not find a temp dir!")
    )
)

(defn file-in-tempdir [& name] 
    (path/join (tempdir) (string ;name))
)

(defn open-tempfile [name]
    (def tpath (path/join (tempdir) name))
    (def file (file/open tpath :w))
    [tpath file]
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
    (defer (do (file/close hfile) (pp "EXIT"))
        (eachp (k v) headers 
            (:write hfile (header-val k))
            (:write hfile ": ")
            (:write hfile (header-val v))
            (:write hfile "\n")
        )
    )
)

(defn cleanup [spec] 
    (pp "CLEAN-UP: ")
    (each cl-spec spec 
        (when-let [p (cl-spec :path)]
            (pp p)
            (os/rm p))
        (when-let [f (cl-spec :file)] 
            (pp f)
            (file/flush f)
            (file/close f)
            (pp f)
        )
    )
)

# (defn http-get! [url &opt headers])

(defn http-get [url &opt headers] 
    (def cmd-args @["curl"])
    (defn add-args [& args] 
        (array/concat cmd-args args)
    )
    
    (def tempfiles @[])
    (defn cleanup-temp [& tempfile]
        (array/concat tempfiles tempfile)
    )
    
    (def req-id (random-id 36))
    (add-args "-sS")
    (when (not= nil headers) 
        (def tpath (string req-id ".headers.txt"))
        (write-headers-file tpath headers)
        (cleanup-temp {:path tpath})
        (add-args "-H" (string "@" tpath))
    )
    (add-args url)
    
    (def [outpath outfile] (open-tempfile (string req-id  ".out.txt")))
    (def [errpath errfile] (open-tempfile (string req-id ".err.txt")))
    (cleanup-temp {:path outpath :file outfile } {:path errpath :file errfile})
    
    (pp cmd-args)
    (def retval (match (os/execute cmd-args :p { :out outfile :err errfile }) 
        0 [:ok (finish-file outfile outpath)]
        _ [:err (finish-file errfile errpath)]
    ))
    
    (cleanup tempfiles)
    retval 
)

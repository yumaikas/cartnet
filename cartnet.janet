(import path)
(import uri)

(defn- s. [& args] (string ;args))

(defn- int [num] 
    (- num (mod num 1)))

(def- alphabet (string 
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789"))

(defn- random-id [&opt len] 
  (default len 10)
  (string/from-bytes (splice (seq [x :range (0 len)]
                               (alphabet (int (* 35 (math/random))))))))

(defn- tempdir [] 
    (def osw (os/which))
    (or 
        (and (= osw :windows) 
            (or 
                (os/getenv "TMP")
                (os/getenv "TEMP")
                (os/getenv "USERPROFILE")))
        (and (= osw :osx)  (os/getenv ""))
        (and (= osw :linux)  "/tmp")
        (error "Could not find a temp dir!")))

(defn- file-in-tempdir [& name] 
    (path/join (tempdir) (string (splice name))))

(defn- open-tempfile [name]
    (def tpath (path/join (tempdir) name))
    (def f (file/open tpath :w))
    [tpath f])

(defn- finish-file [f path]
    (file/flush f)
    (slurp path))

(defn- header-val [val] 
    (match (type val)
        :string val
        :buffer (string val)
        :keyword (string val)
        _ (error "Cannot write hader")))

(defn- headers-as-file [headers] 
    (def req-id (random-id 24))
    (def tpath (file-in-tempdir req-id ".headers.txt"))
    (def hfile (file/open tpath :w))
    ((dyn :register-cleanup) {:path tpath})

    (defer (do (file/close hfile))
        (eachp (k v) headers 
            (:write hfile (header-val k))
            (:write hfile ": ")
            (:write hfile (header-val v))
            (:write hfile "\n")))
    ["-H" (s. "@" tpath)])

(defn- headers-as-args [headers] 
  (def out @[])
  (eachp [k v] headers
    (array/concat out ["-H" (s. (header-val k) ": " (header-val v))]))
  out)

(defn- size-arr 
  "Size up how big an array of lengthable items would be"
  [arr] 
  (var size 0)
  (each el arr 
    (+= size (+ (length el) 1)))
  # Remove the size of the last space
  (- size 1))

# Leave some buffer room
(def MAX-FLAG-LEN 32000)

(defn- cleanup [spec] 
    (each cl-spec spec 
        (when-let [f (cl-spec :file)] 
            (file/flush f)
            (file/close f))
        (when-let [p (cl-spec :path)] (os/rm p))))

(defn- verb-arg [verb] 
  (match verb 
    "GET" ["-G"]
    "POST" ["-X" "POST"]
    "PUT" ["-X" "PUT"]
    "PATCH" ["-X" "PATCH"]
    "DELETE" ["-X" "DELETE"]
    "OPTIONS" ["-X" "OPTIONS"]
    # Tell CURL to do a head request only
    "HEAD" ["-I"]
    _ (error (string "Unexpected verb " verb))))

(defn http-request 
`Takes a verb, a url, and a request spec.
A spec is a kv that contains the following keys:

:headers 
  - A kv provided as headers to a given request. 
:post-body 
  - A buffer passed as the body for POST requests. 
:data-args 
  - A kv passed as the URL query string.`
[verb url &opt spec] 
    
    (def cmd-args @["curl"])
    (defn add-arg [& args] 
        (array/concat cmd-args ;args))
        
    # Make curl not show progress bars (-s) but still show errors in stderr (-S)
    # Also, disable globbing, so that URLs don't get globbed by accident
    (add-arg "-sSg")
    (add-arg (verb-arg verb))
        
    # Set things up for the different HTTP verbs
    (add-arg url)
    
    (def tempfiles @[])
    (defn cleanup-temp [& tempfile]
        (array/concat tempfiles tempfile))
    (setdyn :regsiter-cleanup cleanup-temp)
        
    (var post-body nil)
    (when-let [spec spec] 
        (when-let [headers (spec :headers)]
          (add-arg (headers-as-args headers)))
        
        (when-let [postdata (spec :post-body)] 
          (set post-body postdata)
          (add-arg "-d" "@-"))
        (when-let [{ :username un :password pw } (spec :credential)]
          (add-arg "-u" (s. un ":" pw)))
        
        (when-let [data-args (spec :data-args)] 
            (def kvbuf @"")
            (eachp (k v) data-args 
                (buffer/push kvbuf (uri/escape (string k)) "=" (uri/escape (string v)) "&"))
            # Cut off the last "&", it should only be a single byte
            (add-arg "-d" (string (buffer/popn kvbuf 1))))) 

    (def curl-proc (os/spawn cmd-args :p { :in :pipe :out :pipe :err :pipe }))
    (def { :out curl-out :in curl-in :err curl-err } curl-proc)
    (when post-body 
      (:write curl-in (string post-body)))
    (:close curl-in)

    (def req-bytes (:read curl-out :all))
    (def err-msg (:read curl-err :all))
    (def ret 
      (if err-msg
        [:err err-msg]
        [:ok req-bytes])))

(defn http-get 
    ```Issues a GET request with the given request spec. 
    See (doc http-request) for details about spec```
    [url &opt spec]
    (http-request "GET" url spec))
    
(defn http-post
    "Issues a POST request with the given request spec. See http-request for details about spec"
    [url &opt spec]
    (http-request "POST" url spec))

(defmacro- throwing-func [name wrapped doc] 
    ~(defn ,name ,doc [url &opt spec]
        (match (,wrapped url spec)
            [:ok response] response
            [:err err] (error err))))

(throwing-func http-get! http-get "Like http-get, but errors on failure, instead of returning [:err err]")
(throwing-func http-post! http-post "Like http-post, but errors on failure, instead of returning [:err err]")

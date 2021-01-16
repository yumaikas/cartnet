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
        (os/getenv "TMP")
        (os/getenv "TEMP")
        (os/getenv "USERPROFILE")
        (error "Could not find a temp dir!")
    )
)

(defn tempfile [prefix] 
    (path/join (tempdir) (string prefix (random-id)))
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

(defn http-get [url &opt headers] 
    (if (not= nil headers) 
        (do 
            (def tpath (tempfile "get-"))
            (write-headers-file tpath headers)
            (defer (os/rm tpath)
                (:read (file/popen (string "curl.exe -sS -H @" tpath " " url )) :all)
            )
        )
        (do 
            (pp url)
            (def curl-get-proc (os/spawn ["curl.exe" "-sS" url] :p {:out :pipe :err :pipe}))
            (pp curl-get-proc)
            (def buf @"")
            (try
                (do
                    (pp (:read (curl-get-proc :out) :all buf 5.0))
                )
                ([err] nil)
            )
            (pp buf)
            
            (break)
            (match (:wait curl-get-proc) 
                0 (:read (curl-get-proc :out) :all)
                _ (error (:read (curl-get-proc :err) :all))
            )
        )
    )
)

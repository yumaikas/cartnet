(use ../cartnet) 

(math/seedrandom (os/time))


(def srv (os/spawn ["janet" "test/server.janet" "--serve"] :p {:out :pipe :err :pipe}))
(ev/spawn 
  (:read (srv :out) :all)
  (def errs (:read (srv :err) :all))
  (if errs 
    (print errs)))

(defmacro should-succeed [http-func url buf &opt spec] 
    ~(match (,http-func ,url ,spec)
        [:ok resp1] 
        (do 
            (buffer/clear ,buf)
            (buffer/blit ,buf resp1)
            true)
        [:err errmsg] (error errmsg)))

(defmacro should-fail [http-func url buf &opt spec] 
    ~(match (,http-func ,url ,spec)
        [:ok resp1] (error (string "Should have failed, instead got: " resp1))
        [:err errmsg] 
        (do 
            (buffer/clear ,buf)
            (buffer/push ,buf errmsg)
            true)))

(defmacro test [name & body] 
    ~(try 
        (do ,;body
            (prin "."))
        ([err] 
            (do 
                (print "X")
                (print (string "Test \"" ,name "\" failed. "))
                (print (string "Details: " err))
                (error "Tests failed!")))))

(test "Reading from an existing domain does the expected thing"
    (def resp @"")
    (should-succeed http-get "https://janet-lang.org" resp)

    (assert (= (type resp) :buffer) (string "Should be a buffer instead of a " (type resp)))
    (assert (> (length resp) 0) "Response should have a positive length"))

(test "A non-existant domain doesn't resolve"
    (def errmsg @"")
    (should-fail http-get "https://error.junglecoder.com" errmsg))


(test "We can ping our local/test server, and the hard-coded response is what we expect"
    (def resp @"")
    (should-succeed http-get "http://localhost:9001/test1" resp)
    (assert (= "This is a response!" (string resp)) "We didn't get the expected response!"))

(test "If we send data-args, they get encoded to the URL as expected" 
    (def resp @"")
    (should-succeed http-get "http://localhost:9001/params-route" resp {
        :data-args {:arg1 "foo" :arg2 "bar" "XXXX" 23} })

    (assert (deep= @"arg1=foo\narg2=bar\nXXXX=23" resp) "Keys and value don't match!")
)

(test "Post works, and gets processed correctly" 
    (def resp @"")
    (should-succeed http-post "http://localhost:9001/cut-out-even-bytes" resp {
        :post-body @"1|2|3|4|5" :headers { "Content-Type" "text/plain" } })
    (assert (deep= @"12345" resp) (string "Resp was " resp " instead of 12345")))

# Clean up the local server process
(:kill srv)
(:wait srv)
(os/exit 0)


(use ../cartnet) 

(math/seedrandom (os/time))
(var resp nil)
(match (http-get "https://junglecoder.com")
    [:ok resp1] (set resp resp1)
    [:err errmsg] (error errmsg)
)

(assert (= (type resp) :buffer) (string "Should be a buffer instead of a " (type resp)))
(assert (> (length resp) 0) "Response should have a positive length")

(match (http-get "https://error.junglecoder.com")
    [:ok resp1] (error "This request should not have succeeded!")
    [:err errmsg] nil
)


(use ../cartnet) 


(def resp (http-get "https://junglecoder.com"))

(assert (= (type resp) :buffer) (string "Should be a string instead of a " (type resp)))
(assert (> (length resp) 0) "Response should have a positive length")

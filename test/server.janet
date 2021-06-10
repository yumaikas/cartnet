(use osprey)
(GET "/test1" "This is a response!" )
(GET "/params-route"
     (string/join (seq [(k v) :pairs params] (string k "=" v)) "\n"))

(POST "/cut-out-even-bytes"
      (var idx 0)
      (string/from-bytes ;(seq [b :in body 
                                :after (++ idx) 
                                :when (even? idx)]
                            b)))

# only run the server if we're passed a flag to do so.
# Otherwise, just exit.
(match (dyn :args)
  [scrname "--serve"] (with-dyns (comment [:out @""]) (server 9001))
  _ ())
(pp "oi?")

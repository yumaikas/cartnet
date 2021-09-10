(declare-project 
    :name "cartnet"
    :description "Networking, but with a horse-drawn cart (aka shelling to a CURL executable)"
    :author "Andrew Owen <yumaikas94@gmail.com>"
    :url "https://github.com/yumaikas/cartnet"
    :dependencies [
        "https://github.com/janet-lang/path.git"
        "https://github.com/andrewchambers/janet-uri.git"
        "https://github.com/swlkr/osprey.git" 
         ])

(declare-source :source ["cartnet.janet"])

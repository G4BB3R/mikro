(module Main
    (export main)
    (import (Api                   (Cred))
            (Article.Slug as AS    (run))
            (Json.Decode as Decode (Value Json))
            (Math as M)))


(type Colors Red Green Blue)

(alias Mother { name : String, age : Int })

(alias Mom Mother)

(@ Int -> Int)
(defn minus [a b]
    (- a b))

(@ Int -> Int)
(defn wtf [a b]
    (let[(def a 3000)]
        (if (= a b)
            (+ a b)
            (- a b))))

(module Main
    (export main)
    (import (Api                   (Cred))
            (Article.Slug as AS    (run))
            (Json.Decode as Decode (Value Json))
            (Math as M)))


(defn lambida [ a b ]
    (case (+ a b)
		(1  10)
		(2  20)
		(_ 999)))

(@ Int -> Int )
(defn fib [ n ]
    (let [
        (defn n1 [] 2)
    ]
        (case n
            (0 0)
            (1 1)
            (_ (+ (fib (- n 1)) (fib (- n 2)) ))))
    )

(defn main []
	(print (fib 10)))

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
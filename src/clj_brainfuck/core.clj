(ns clj-brainfuck.core
  (:gen-class))

;the memory tape, there may be a way to dispose of this mutable state later
(def memory (int-array 100))

;dispatch based on the tokens value
(defmulti parse-token (fn [token memory-pointer instruction-pointer] token))

(defmethod parse-token \> [token memory-pointer instruction-pointer]
  [(inc memory-pointer) (inc instruction-pointer)])
(defmethod parse-token \< [token memory-pointer instruction-pointer]
  [(dec memory-pointer) (inc instruction-pointer)])
(defmethod parse-token \+ [token memory-pointer instruction-pointer]
  (do
    (aset memory memory-pointer (inc (aget memory memory-pointer)))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \- [token memory-pointer instruction-pointer]
  (do
    (aset memory memory-pointer (dec (aget memory memory-pointer)))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \. [token memory-pointer instruction-pointer]
  (do
    (print (aget memory memory-pointer))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \, [token memory-pointer instruction-pointer]
  [memory-pointer (inc instruction-pointer)])
(defmethod parse-token \[ [token memory-pointer instruction-pointer]
  [memory-pointer (inc instruction-pointer)])
(defmethod parse-token \] [token memory-pointer instruction-pointer]
  [memory-pointer (inc instruction-pointer)])
;skip every character that doesn't have an explicit purpose, they are comment characters
(defmethod parse-token :default [token memory-pointer instruction-pointer]
  [memory-pointer (inc instruction-pointer)])

(defn parse-string [data]
  (loop [tokens data
         memory-pointer 0
         instruction-pointer 0]
    (if
      (>= instruction-pointer (count tokens))
      (println "")
      (let
        [
          [new-memory-pointer new-instruction-pointer]
          (parse-token (get tokens instruction-pointer) memory-pointer instruction-pointer)
        ]
        (recur
          tokens
          new-memory-pointer
          new-instruction-pointer
          )
        )
      )
    )
  )

(defn run-brainfuck [filename] (parse-string (slurp filename)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (run-brainfuck (first args)))

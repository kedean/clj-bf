(ns clj-brainfuck.core
  (:gen-class))

;the memory tape, there may be a way to dispose of this mutable state later
(def memory (int-array 10000))

(defn next-matching-bracket [init-position sequence]
  (loop
    [
      position (inc init-position)
      unmatched-bracket-count 1
    ]
    (cond
      (= unmatched-bracket-count 0)
      position
      (= \] (get sequence position)) ;found match
      (recur (inc position) (dec unmatched-bracket-count))
      (= \[ (get sequence position)) ;new unmatched
      (recur (inc position) (inc unmatched-bracket-count))
      :else
      (recur (inc position) unmatched-bracket-count)
      )
    )
  )

(defn prev-matching-bracket [init-position sequence]
  (loop
    [
      position (dec init-position)
      unmatched-bracket-count 1
    ]
    (cond
      (= unmatched-bracket-count 0)
      position
      (= \[ (get sequence position)) ;found match
      (recur (dec position) (dec unmatched-bracket-count))
      (= \] (get sequence position)) ;new unmatched
      (recur (dec position) (inc unmatched-bracket-count))
      :else
      (recur (dec position) unmatched-bracket-count)
      )
    )
  )

;dispatch based on the tokens value
(defmulti parse-token (fn [current-token tokens memory-pointer instruction-pointer] current-token))

(defmethod parse-token \> [current-token tokens memory-pointer instruction-pointer]
  [(inc memory-pointer) (inc instruction-pointer)])
(defmethod parse-token \< [current-token tokens memory-pointer instruction-pointer]
  [(dec memory-pointer) (inc instruction-pointer)])
(defmethod parse-token \+ [current-token tokens memory-pointer instruction-pointer]
  (do
    (aset memory memory-pointer (inc (aget memory memory-pointer)))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \- [current-token tokens memory-pointer instruction-pointer]
  (do
    (aset memory memory-pointer (dec (aget memory memory-pointer)))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \. [current-token tokens memory-pointer instruction-pointer]
  (do
    (print (char (aget memory memory-pointer)))
    [memory-pointer (inc instruction-pointer)]
    ))
(defmethod parse-token \, [current-token tokens memory-pointer instruction-pointer]
  (do
    (flush)
    (aset
      memory
      memory-pointer
      (first (read-line))
      )
    [memory-pointer (inc instruction-pointer)]))
(defmethod parse-token \[ [current-token tokens memory-pointer instruction-pointer]
  (if
    (= 0 (aget memory memory-pointer))
    [
      memory-pointer
      (next-matching-bracket
        instruction-pointer
        tokens
        )
      ]
    [memory-pointer (inc instruction-pointer)]
    )
  )
(defmethod parse-token \] [current-token tokens memory-pointer instruction-pointer]
  (if
    (not (= 0 (aget memory memory-pointer)))
    [
      memory-pointer
      (prev-matching-bracket
        instruction-pointer
        tokens
        )
      ]
    [memory-pointer (inc instruction-pointer)]
    )
  )
;skip every character that doesn't have an explicit purpose, they are comment characters
(defmethod parse-token :default [current-token tokens memory-pointer instruction-pointer]
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
          (parse-token
            (get tokens instruction-pointer)
            tokens
            memory-pointer
            instruction-pointer)
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

(ns clj-brainfuck.core-test
  (:require [clojure.test :refer :all]
            [clj-brainfuck.core :refer :all]))

(deftest helloworld-test
  (testing "The hello world program runs"
    (run-brainfuck "resources/helloworld.bf")))

(ns clojurer.core-test
  (:use midje.sweet)
  (:require [clojurer.core :refer :all]))

(facts "Do it, do it again, and again"
  (facts "satom?"
    (fact "a number is a satom"
      (satom? 0) => true
      (satom? 12345) => true))
  (facts "satoms-list?"
    (fact "a list of numbers is a list of satoms"
      (satoms-list? '(1 2 3 4 5)) => true
      (satoms-list? '((2))) => false
      (satoms-list? '(1 2 3 4 (5))) => false)))

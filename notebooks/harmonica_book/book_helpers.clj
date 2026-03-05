;; # Helper Functions
;;
;; Small utilities shared across the book's notebooks. Each notebook
;; requires this namespace so you can see exactly where helpers come from.

(ns harmonica-book.book-helpers
  (:require
   [scicloj.lalinea.linalg :as la]))

(defn allclose?
  "True when every element of `a` is within `tol` of the corresponding
   element of `b`. Works on RealTensors, ComplexTensors, vectors,
   and scalars."
  ([a b] (allclose? a b 1e-10))
  ([a b tol]
   (if (and (number? a) (number? b))
     (la/close-scalar? a b tol)
     (la/close? a b tol))))

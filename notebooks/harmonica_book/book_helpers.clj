
;; # Helper Functions
;;
;; Small utilities shared across the book's notebooks. Each notebook
;; requires this namespace so you can see exactly where helpers come from.

(ns harmonica-book.book-helpers
  (:require
   [tech.v3.datatype.functional :as dfn]))

(defn allclose?
  "True when every element of `a` is within `tol` of the corresponding
   element of `b`. Works on dtype buffers, tensors, scalars, and Clojure
   vectors — anything `dfn/-` accepts."
  ([a b] (allclose? a b 1e-10))
  ([a b tol]
   (< (double (dfn/reduce-max (dfn/abs (dfn/- a b)))) (double tol))))

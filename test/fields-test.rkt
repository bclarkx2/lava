
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../src/interpreter.rkt")
(require "common.rkt")


;;; test cases

(assert (test-interpret-class "fields-files/lookup" 'A)
        4)

(assert-interpret-class-err "fields-files/no-such-field"
                            'A
                            'illegal-var-dereferencing)

(assert (test-interpret-class "fields-files/multifield" 'A)
        4)

(assert-interpret-class-err "fields-files/unset-var"
                            'A
                            'unset-instance-field
                            "unset!")

(assert (test-interpret-class "fields-files/implicit-field" 'A)
        13
        "implicit-field")

(assert-interpret-class-err "fields-files/implicit-in-main"
                            'A
                            'illegal-var-dereferencing)

(assert (test-interpret-class "fields-files/second-class-field" 'A) 8)

(assert (test-interpret-class "fields-files/local-not-shadows" 'A) 2)

(assert (test-interpret-class "fields-files/local-shadows-instance-field" 'A) 2)

(assert (test-interpret-class "fields-files/explicit-this" 'A) 5)

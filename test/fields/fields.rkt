
#lang racket
(provide (all-defined-out))

;;; requirements
(require xrepl)
(require racket/trace)
(require rackunit "../../src/interpreter.rkt")
(require "../common.rkt")


;;; test cases

(assert (test-interpret-class "lookup" "A")
        4)

(assert-interpret-class-err "no-such-field"
                            "A"
                            'illegal-var-dereferencing)

(assert (test-interpret-class "multifield" "A")
        4)

(assert-interpret-class-err "unset-var"
                            "A"
                            'unset-instance-field
                            "unset!")

(assert (test-interpret-class "implicit-field" "A")
        13
        "implicit-field")

(assert-interpret-class-err "implicit-in-main"
                            "A"
                            'illegal-var-dereferencing)

(assert (test-interpret-class "second-class-field" "A")
        8
        "second-class-field")

(assert (test-interpret-class "local-not-shadows" "A")
        2
        "local-not-shadows")

(assert (test-interpret-class "local-shadows-instance-field" "A")
        2
        "local-shadows-instance-field")

(assert (test-interpret-class "explicit-this" "A")
        5
        "explicit-this")

(assert (test-interpret-class "update-field" "A")
        5
        "update-field")

(assert (test-interpret-class "instance-field-side-effect" "A")
        1
        "instance-field-side-effect")

(assert (test-interpret-class "cascading-fields" "A")
        2
        "cascading-fields")

(assert (test-interpret-class "set-field-field" "A")
        1
        "set-field-field")

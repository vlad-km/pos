;;; -*- mode:lisp; coding:utf-8 -*-

;;; Lisp persistence object
;;; This file is part of the :dbo package
;;; Copyright © 2018 Vladimir Mezentsev
;;;



(eval-when (:compile-toplevel :load-toplevel :execute)
    (unless (find :pos *features*)
        (push :pos *features*)) )

(defpackage #:pos
  (:use #:cl)
  (:export #:expire-time #:expire-date
           #:create #:bind #:save))

;;; EOF

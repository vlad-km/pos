;;; -*- mode:lisp; coding:utf-8 -*-

;;; Lisp persistence object
;;; This file is part of the :dbo package
;;; Copyright © 2018 Vladimir Mezentsev
;;;



(in-package :pos)

(das:structure dbo
               source
               adapter readfn writefn
               (default (new))
               instance
               created
               expire)


;;; include into def-type
;;; for future use with das:generic
(das:def-type 'dbo
    (lambda (obj)
        (eq (cdr (das::das/structure-pred obj)) 'dbo )))

(in-package :cl-user)

;;; EOF

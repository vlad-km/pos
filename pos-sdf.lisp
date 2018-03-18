;;; -*- mode:lisp; coding:utf-8 -*-

;;; Lisp persistence object
;;; This file is part of the :dbo package
;;; Copyright © 2018 Vladimir Mezentsev
;;;


(lores:defsys :pos
    :path "git/pos"
    :components ((:file "pos-pkg")
                 (:file "dbo-structures")
                 (:file "dbo-io")
                 (:file "pos" :depends ("dbo-structures" "dbo-io"))))





;;; EOF

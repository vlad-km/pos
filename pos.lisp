;;; -*- mode:lisp; coding:utf-8 -*-


;;; Lisp persistence object
;;; This file is part of the :pos package
;;; Copyright © 2018 Vladimir Mezentsev
;;;


(in-package :pos)
(export '(jscl::new jscl::oget jscl::make-new jscl::concat jscl::fset))
(export '(jscl::js-null-p))


(defun fetch (dbo)
    (let* ((default (dbo-default dbo))
           (data (funcall (dbo-readfn dbo) (dbo-source dbo))))
        (unless data
            (funcall (dbo-writefn dbo) (dbo-source dbo) (jso:copy default))
            (setq data default))
        (set-dbo-instance dbo data)))

(defun expire-tap (created dbo)
    (let ((expire (dbo-expire dbo)))
        (cond ((integerp expire) (+ created expire))
              ((objectp expire)
               (set-dbo-expire dbo
                               (funcall ((oget expire "getTime" "bind") expire)))))))

(defun fetch-expire (dbo)
    (let* ((default (dbo-default dbo))
           (data (funcall (dbo-readfn dbo) (dbo-source dbo)))
           (created)
           (expire))
        (unless data
            (setq created (#j:Date:now)
                  expire (expire-tap created dbo)
                  data (local-storage-container (jso:copy default) created expire))
            (funcall (dbo-writefn dbo)
                     (dbo-source dbo) data)
            (set-dbo-created dbo created))
        (set-dbo-instance dbo data)))



;;; create persistence object with name
;;; from local-storage / file-system / memory
;;;
;;; if object not exists, he will be create from default
;;; pattern
;;;

;;; adapter selector
(defun choice-read-adapter (storage flag)
    (case storage
      (:ls (if flag *storage-read-expired-fn *storage-read-fn) )
      (:fs (if flag *fs-read-fn *fs-read-fn ))))

;;; expire helper for days/hours/minutes/seconds
;;;
;;; use:
;;;
;;;  (pos:create "name" :storage :ls :default template-1
;;;              :expire (expire-time :hours 12 :minutes 30)
;;;
(defun expire-time (&key (days 0) (hours 0) (minutes 0) (seconds 0))
    (reduce #'+
            (list (* seconds 1000)(* minutes 60000)(* hours 3600000) (* days 86400000))))


;;; expire helper for date year/month/day
;;;
;;; use:
;;;
;;;  (pos:create "name" :storage :ls :default template-1
;;;              :expire (expire-date :year 2019 :month 3 :day 12)
;;;
;;; this object expired 2019/04/12 at 00:00:00
;;;
(defun expire-date (&key (year 2020) (month 0) (day 1))
    (make-new #j:Date year month day 0 0 0))


;;; create object
(defun create (name &key (storage :memory) (default (new)) expire)
    (let ((readfn)
          (writefn)
          (dbo)
          (fetchfn #'fetch))
        (cond ((eql storage :memory)
               (setq readfn *memory-read-fn writefn *memory-write-fn))
              ((eql storage :ls)
               (setq readfn *storage-read-fn writefn *storage-write-fn))
              ((eql storage :fs)
               (setq readfn *fs-read-fn writefn *fs-write-fn)))
        (when expire
            (setq fetchfn #'fetch-expire))
        (setq dbo
              (make-dbo :source name :adapter storage
                        :readfn (choice-read-adapter storage expire)
                        :writefn writefn
                        :default default
                        :expire expire))
        (funcall fetchfn dbo)
        dbo))

;;;
;;; binding local variable and db instance
;;;
;;; use:
;;;
;;; (let ((dbo (pos:create "Some" :storage :ls))
;;;       (instance))
;;;    (pos:bind instance dbo)
;;;    (jso:_set (instance "name") "Jaan")
;;;    (jso:_set (instance "profile") nil)
;;;    (jso:_set (instance "kpi") (jso:mk "k1" 0.11 "k2" 1.1 "k3" -0.1))
;;;    (pos:save dbo)))
;;;

(defmacro bind (var dbo)
    `(progn
         (if (dbo-expire ,dbo)
             (setf ,var (oget (dbo-instance ,dbo) "instance"))
             (setf ,var (dbo-instance ,dbo)))))


;;; Save object
(defun save (dbo)
    (funcall (dbo-writefn dbo) (dbo-source dbo) (dbo-instance dbo)))

(in-package :cl-user)
;;; EOF

;;; -*- mode:lisp; coding:utf-8 -*-

;;; Lisp persistence object
;;; This file is part of the :pos package
;;; Copyright © 2018 Vladimir Mezentsev
;;;



(in-package :pos)


;;; stub for memory storage
;;;
(defvar *memory-read-fn (lambda (key)))
(defvar *memory-write-fn (lambda (key data)))


;;; host file system

(defparameter *file-options* (jso:mk "encoding" "utf8" "spaces" 2))
(defvar *fs-read-fn (lambda (key) (read-from-file key)))
(defvar *fs-write-fn (lambda (key data) (write-to-file key data)))


;;; read - only sync
(defun read-from-file (path)
    (json-file:read-sync-from path "utf8"))

;;; write - only async
(defun write-to-file (path object)
    (json-file:write-to path object *file-options*  (lambda (x) nil)))


;;; local storage

(defvar *storage-read-fn (lambda (key) (local-storage-read key)))
(defvar *storage-read-expired-fn (lambda (key) (local-storage-with-expire-read key)))
(defvar *storage-write-fn (lambda (key data) (local-storage-write key data)))


(defun local-storage-container (instance created expire)
    (jso:mk "created" created "expire" expire "instance" instance))

(defun local-storage-read (key)
    (let ((raw (#j:localStorage:getItem key)))
        (if (js-null-p raw) (setq raw nil))
        (#j:JSON:parse raw)))

#|
(defun local-storage-with-expire-read (key)
    (let ((raw (#j:localStorage:getItem key))
          (instance))
        (if (js-null-p raw) (setq raw nil))
        (setq instance (#j:JSON:parse raw))
        (if (check-expire instance)
            nil
            instance)))
|#

(defun local-storage-with-expire-read (key)
    (let ((instance (local-storage-read key)))
        (if (check-expire instance)
            nil
            instance)))


(defun check-expire (obj)
    (if (eql obj nil) t
        (let ((expire (jso:_get (obj "expire")))
              (created (jso:_get (obj "created")))
              (instance (jso:_get (obj "instance")))
              (expired))
            (setq expired
                  (cond ((and expire created instance)
                         (if (>= (#j:Date:now) expire) t nil))
                        (t (error "its not expire object")) ))
            expired)))

(defun local-storage-write (key data)
    (#j:localStorage:setItem
     key
     (#j:JSON:stringify data)))

(in-package :cl-user)

;;; EOF

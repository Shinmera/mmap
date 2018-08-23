#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.mmap)

(define-condition mmap-error (simple-error)
  ())

(defun cfold (env var form)
  (if (constantp var env)
      `(load-time-value ,form)
      form))

(defun translate-path/size (path/size)
  (etypecase path/size
    (string path/size)
    (pathname (uiop:native-namestring path/size))
    (fixnum path/size)))

#-(or unix windows)
(defun mmap (path &rest args)
  (error "Platform not supported."))

#-(or unix windows)
(defun munmap (addr fd size)
  (error "Platform not supported."))

(defmacro with-mmap ((addr fd size path &rest args) &body body)
  `(multiple-value-bind (,addr ,fd ,size) (mmap ,path ,@args)
     (unwind-protect
          (progn ,@body)
       (munmap ,addr ,fd ,size))))

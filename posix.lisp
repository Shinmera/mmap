#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.mmap)

(defun protection-flag (flag)
  (ecase flag
    (:none osicat-posix:prot-none)
    (:read osicat-posix:prot-read)
    (:write osicat-posix:prot-write)
    (:exec osicat-posix:prot-exec)))

(defun mmap-flag (flag)
  (ecase flag
    (:shared osicat-posix:map-shared)
    (:private osicat-posix:map-private)
    (:no-reserve osicat-posix:map-noreserve)
    (:locked osicat-posix:map-locked)
    (:grows-down osicat-posix:map-growsdown)
    (:anonymous osicat-posix:map-anonymous)
    (:populate osicat-posix:map-populate)
    (:non-block osicat-posix:map-nonblock)))

(defun msync-flag (flag)
  (ecase flag
    (:sync osicat-posix:ms-sync)
    (:async osicat-posix:ms-async)
    (:invalidate osicat-posix:ms-invalidate)))

(defun fopen-flag (flag)
  (ecase flag
    (:read osicat-posix:o-rdonly)
    (:write osicat-posix:o-wronly)
    (:create osicat-posix:o-creat)
    (:ensure-create osicat-posix:o-excl)
    (:truncate osicat-posix:o-trunc)
    (:dont-claim-tty osicat-posix:o-noctty)
    (:non-block osicat-posix:o-nonblock)
    (:no-follow osicat-posix:o-nofollow)
    (:async osicat-posix:o-async)
    (:direct osicat-posix:o-direct)
    (:directory osicat-posix:o-directory)
    (:large-file osicat-posix:o-largefile)
    (:file-sync osicat-posix:o-sync)
    (:data-sync osicat-posix:o-dsync)))

(cffi:defcfun strerror :string
  (errnum :int))

(cffi:defcvar errno :int)

(defmacro with-translated-posix-failure (&body body)
  `(handler-bind ((osicat:system-error
                    (lambda (e)
                      (mmap-error (osicat:system-error-code e)
                                  (strerror (osicat:system-error-code e))))))
     ,@body))

(declaim (notinline %mmap))
(defun %mmap (path size offset open protection mmap)
  (declare (type fixnum open protection mmap))
  (declare (optimize speed))
  (with-translated-posix-failure
    (let ((fd -1))
      (etypecase path
        (string
         (setf fd (osicat-posix:open path open))
         (unless size
           (setf size (osicat-posix:stat-size (osicat-posix:fstat fd)))))
        (null))
      (handler-bind ((error (lambda (e)
                              (declare (ignore e))
                              (when (/= fd -1)
                                (osicat-posix:close fd)))))
        (let ((addr (osicat-posix:mmap (cffi:null-pointer)
                                       size
                                       protection
                                       mmap
                                       fd
                                       offset)))
          (values addr fd size))))))

(defun mmap (path &key (open '(:read)) (protection '(:read)) (mmap '(:private)) size (offset 0))
  (%mmap (etypecase path
           (string path)
           (pathname (uiop:native-namestring path)))
         size offset
         (reduce #'logior open :key #'fopen-flag)
         (reduce #'logior protection :key #'protection-flag)
         (reduce #'logior mmap :key #'mmap-flag)))

(define-compiler-macro mmap (&environment env path &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)) size (offset 0))
  `(%mmap ,(cfold env `(translate-path ,path) path)
          ,size ,offset
          ,(cfold env `(reduce #'logior ,open :key #'fopen-flag) open)
          ,(cfold env `(reduce #'logior ,protection :key #'protection-flag) protection)
          ,(cfold env `(reduce #'logior ,mmap :key #'mmap-flag) mmap)))

(defun munmap (addr fd size)
  (with-translated-posix-failure
    (osicat-posix:munmap addr size)
    (when fd (osicat-posix:close fd))
    NIL))

(defun msync (addr fd size &key (flags '(:sync)))
  (declare (ignore fd))
  (with-translated-posix-failure
    (osicat-posix:msync addr size (reduce #'logior flags :key #'msync-flag))
    NIL))

(define-compiler-macro msync (&environment env addr fd size &key (flags ''(:sync)))
  (declare (ignore fd))
  `(with-translated-posix-failure
     (osicat-posix:msync ,addr ,size ,(cfold env `(reduce #'logior ,flags :key #'msync-flag)) flags)
     NIL))

(defun mprotect (addr size protection)
  (with-translated-posix-failure
    (osicat-posix:mprotect addr size (reduce #'logior protection :key #'protection-flag))
    NIL))

(define-compiler-macro mprotect (&environment env addr size protection)
  `(with-translated-posix-failure
     (osicat-posix:mprotect ,addr ,size ,(cfold env `(reduce #'logior ,protection :key #'protection-flag) protection))
     NIL))

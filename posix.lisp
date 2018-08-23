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
    (:fixed osicat-posix:map-fixed)
    (:failed osicat-posix:map-failed)
    (:no-reserve osicat-posix:map-noreserve)
    (:locked osicat-posix:map-locked)
    (:grows-down osicat-posix:map-growsdown)
    (:anonymous osicat-posix:map-anonymous)
    (:populate osicat-posix:map-populate)
    (:non-block osicat-posix:map-nonblock)))

(defun fopen-flag (flag)
  (ecase flag
    (:read osicat-posix:o-rdonly)
    (:write osicat-posix:o-wronly)
    (:create osicat-posix:o-creat)
    (:ensure-create osicat-posix:o-excl)
    (:truncate osicat-posix:o-trunc)
    (:append osicat-posix:o-append)
    (:no-c-tty osicat-posix:o-noctty)
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

(defmacro check-posix (condition)
  `(unless ,condition
     (error 'mmap-error
            :format-control "Failed mmap file (E~d):~%  ~a"
            :format-arguments (list errno (strerror errno)))))

(declaim (inline %mmap))
(defun %mmap (path/size open protection mmap offset)
  (declare (type fixnum open protection mmap))
  (declare (optimize speed))
  (let (fd size)
    (etypecase path/size
      (string
       (setf fd (osicat-posix:open path/size open))
       (check-posix (/= -1 fd))
       (let ((stat (osicat-posix:fstat fd)))
         (check-posix (not (cffi:null-pointer-p stat)))
         (setf size (osicat-posix:stat-size stat))))
      (fixnum
       (setf fd -1)
       (setf size path/size)))
    (let ((addr (osicat-posix:mmap (cffi:null-pointer)
                                   size
                                   protection
                                   mmap
                                   fd
                                   offset)))
      (check-posix (not (cffi:null-pointer-p addr)))
      (values addr fd size))))

(defun mmap (path &key (open '(:read)) (protection '(:read)) (mmap '(:private)))
  (%mmap (etypecase path
           (string path)
           (pathname (uiop:native-namestring path)))
         (reduce #'logior open :key #'fopen-flag)
         (reduce #'logior protection :key #'protection-flag)
         (reduce #'logior mmap :key #'mmap-flag)))

(define-compiler-macro mmap (&environment env path/size &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)))
  `(%mmap ,(cfold env `(translate-path/size ,path/size) path/size)
          ,(cfold env `(reduce #'logior ,open :key #'fopen-flag) open)
          ,(cfold env `(reduce #'logior ,protection :key #'protection-flag) protection)
          ,(cfold env `(reduce #'logior ,mmap :key #'mmap-flag) mmap)))

(declaim (inline munmap))
(defun munmap (addr fd size)
  (osicat-posix:munmap addr size)
  (when fd (osicat-posix:close fd)))

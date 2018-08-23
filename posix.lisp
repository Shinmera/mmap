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
    (:exclusive osicat-posix:o-excl)
    (:truncate osicat-posix:o-trunc)
    (:append osicat-posix:o-append)
    (:no-c-tty osicat-posix:o-noctty)
    (:non-block osicat-posix:o-nonblock)
    (:ndleay osicat-posix:o-ndelay)
    (:sync osicat-posix:o-sync)
    (:no-follow osicat-posix:o-nofollow)
    (:async osicat-posix:o-async)
    (:direct osicat-posix:o-direct)
    (:directory osicat-posix:o-directory)
    (:large-file osicat-posix:o-largefile)
    (:d-sync osicat-posix:o-dsync)
    (:r-sync osicat-posix:o-rsync)))

(declaim (inline %mmap))
(defun %mmap (path/size open protection mmap offset)
  (declare (type fixnum open protection mmap))
  (declare (optimize speed))
  (let (fd size)
    (etypecase path/size
      (string
       (setf fd (osicat-posix:open path/size open))
       (setf size (osicat-posix:stat-size (osicat-posix:fstat fd))))
      (fixnum
       (setf fd -1)
       (setf size path/size)))
    (let ((addr (osicat-posix:mmap (cffi:null-pointer) size
                                   protection
                                   mmap
                                   fd offset)))
      ;; FIXME: better error checks
      (when (cffi:null-pointer addr)
        (error "Failed."))
      (values addr fd size))))

(defun mmap (path &key (open '(:read)) (protection '(:read)) (mmap '(:private)))
  (%mmap (etypecase path
           (string path)
           (pathname (uiop:native-namestring path)))
         (reduce #'logior open :key #'fopen-flag)
         (reduce #'logior protection :key #'protection-flag)
         (reduce #'logior mmap :key #'mmap-flag)))

(define-compiler-macro mmap (&environment env path/size &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)))
  `(%mmap ,(cfold env path/size `(translate-path/size ,path/size))
          ,(cfold env open `(reduce #'logior ,open :key #'fopen-flag))
          ,(cfold env protection `(reduce #'logior ,protection :key #'protection-flag))
          ,(cfold env mmap `(reduce #'logior ,mmap :key #'mmap-flag))))

(declaim (inline munmap))
(defun munmap (addr fd size)
  (osicat-posix:munmap addr size)
  (when fd (osicat-posix:close fd)))

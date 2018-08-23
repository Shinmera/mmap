#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.mmap)

(cffi:defctype handle :fixme)
(cffi:defctype lpsecurity-attributes :fixme)
(cffi:defctype dword :fixme)
(cffi:defctype size_t :fixme)

;; https://docs.microsoft.com/en-us/windows/desktop/api/fileapi/nf-fileapi-createfilea
(cffi:defcfun (create-file "CreateFile") handle
  (path :string)
  (access dword)
  (share-mode dword)
  (attributes lpsecurity-attributes)
  (creation-disposition dword)
  (flags-and-attributes dword)
  (template-file handle))

;; https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-createfilemappinga
(cffi:defcfun (create-file-mapping "CreateFileMapping") handle
  (file handle)
  (attributes lpsecurity-attributes)
  (protect dword)
  (maximum-size-high dword)
  (maximum-size-low dword)
  (name :pointer))

;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa366761(v=vs.85).aspx
(cffi:defcfun (map-view-of-file "MapViewOfFile") :pointer
  (file-mapping-object handle)
  (desired-access dword)
  (file-offset-high dword)
  (file-offset-low dword)
  (number-of-bytes-to-map size_t))

(cffi:defcfun (unmap-view-of-file "UnmapViewOfFile") :boolean
  (base-address :pointer))

(cffi:defcfun (_close "CloseHandle") :boolean
  (object handle))

(declaim (inline %mmap))
(defun %mmap (path/size open protection mmap offset)
  (declare (type fixnum open protection mmap))
  (declare (optimize speed))
  (let (fd size)
    (etypecase path/size
      (string
       (setf fd (create-file path/size ...))
       (setf size FIXME))
      (fixnum
       (setf fd invalid-handle-value)
       (setf size path/size)))
    (let* ((end (+ size offset))
           (handle (create-file-mapping fd 0 protection end end (cffi:null-pointer)))
           (success (map-view-of-file handle mmap offset offset size)))
      (unless success
        (close-handle handle)
        (error "Failed."))
      (values handle fd size))))

(defun flagp (flags &rest tests)
  (loop for test in tests
        always (find test flags)))

(defun translate-open-flags (flags)
  ;; FIXME
  )

(defun translate-protection-flags (flags)
  (cond ((flagp flags :write)
         page-execute-readwrite)
        ((flagp flags :write)
         page-readwrite)
        ((flagp flags :read :exec)
         page-execute-read)
        ((flagp flags :read)
         page-readonly)))

(defun translate-mmap-flags (flags)
  ;; FIXME
  )

(defun mmap (path/size &key (open '(:read)) (protection '(:read)) (mmap '(:private)))
  (%mmap (translate-path/size path/size)
         (translate-open-flags open)
         (translate-protection-flags protection)
         (translate-mmap-flags mmap)))

(define-compiler-macro mmap (&environment env path/size &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)))
  `(%mmap ,(cfold env path/size `(translate-path/size ,path/size))
          ,(cfold env open `(translate-open-flags ,open))
          ,(cfold env protection `(translate-protection-flags ,protection))
          ,(cfold env mmap `(translate-mmap-flags ,mmap))))

(declaim (inline munmap))
(defun munmap (addr fd size)
  (unmap-view-of-file addr)
  (when fd (close-handle fd)))

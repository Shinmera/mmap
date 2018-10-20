#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.mmap)

(defconstant create-new 1)
(defconstant file-attribute-normal 128)
(defconstant file-flag-no-buffering 536870912)
(defconstant file-flag-write-through 2147483648)
(defconstant file-map-copy 1)
(defconstant file-map-execute 32)
(defconstant file-map-read 4)
(defconstant file-map-write 2)
(defconstant file-share-delete 4)
(defconstant file-share-read 1)
(defconstant file-share-write 2)
(defconstant format-message-from-system 4096)
(defconstant format-message-ignore-inserts 512)
(defconstant generic-read 2147483648)
(defconstant generic-write 1073741824)
(defconstant invalid-file-size 4294967295)
(defconstant invalid-handle-value
  (if (boundp 'invalid-handle-value)
      invalid-handle-value
      (cffi:make-pointer 4294967295)))
(defconstant open-always 4)
(defconstant open-existing 3)
(defconstant page-execute-read 32)
(defconstant page-execute-readwrite 64)
(defconstant page-readonly 2)
(defconstant page-readwrite 4)
(defconstant truncate-existing 5)

(cffi:defctype wchar_t :uint16)
(cffi:defctype handle :pointer)
(cffi:defctype lpsecurity-attributes :uint64 #+x86 :uint32)
(cffi:defctype dword :uint32)
(cffi:defctype size_t #+x86-64 :uint64 #+x86 :uint32)

;; https://docs.microsoft.com/en-us/windows/desktop/api/fileapi/nf-fileapi-createfilew
(cffi:defcfun (create-file "CreateFileW") handle
  (path :pointer)
  (access dword)
  (share-mode dword)
  (attributes lpsecurity-attributes)
  (creation-disposition dword)
  (flags-and-attributes dword)
  (template-file handle))

;; https://docs.microsoft.com/en-us/windows/desktop/api/fileapi/nf-fileapi-getfilesize
(cffi:defcfun (get-file-size "GetFileSize") dword
  (file handle)
  (file-size-high :pointer))

;; https://docs.microsoft.com/en-us/windows/desktop/api/winbase/nf-winbase-createfilemappinga
(cffi:defcfun (create-file-mapping "CreateFileMappingA") handle
  (file handle)
  (attributes lpsecurity-attributes)
  (protect dword)
  (maximum-size-high dword)
  (maximum-size-low dword)
  (name :pointer))

(cffi:defcfun (close-handle "CloseHandle") :boolean
  (object handle))

;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa366761(v=vs.85).aspx
(cffi:defcfun (map-view-of-file "MapViewOfFile") :pointer
  (file-mapping-object handle)
  (desired-access dword)
  (file-offset-high dword)
  (file-offset-low dword)
  (number-of-bytes-to-map size_t))

;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa366882(v=vs.85).aspx
(cffi:defcfun (unmap-view-of-file "UnmapViewOfFile") :boolean
  (base-address :pointer))

;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa366563(v=vs.85).aspx
(cffi:defcfun (flush-view-of-file "FlushViewOfFile") :boolean
  (base-address :pointer)
  (number-of-bytes-to-flush size_t))

;; https://docs.microsoft.com/en-us/windows/desktop/api/fileapi/nf-fileapi-flushfilebuffers
(cffi:defcfun (flush-file-buffers "FlushFileBuffers") :boolean
  (file handle))

;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa366898(v=vs.85).aspx
(cffi:defcfun (virtual-protect "VirtualProtect") :boolean
  (address :pointer)
  (size size_t)
  (new-protect dword)
  (old-protect :pointer))

(cffi:defcfun (get-last-error "GetLastError") dword)

(cffi:defcfun (format-message "FormatMessageW") dword
  (flags dword)
  (source :pointer)
  (message-id dword)
  (language-id dword)
  (buffer :pointer)
  (size dword)
  (arguments :pointer))

(defmacro check-windows (condition)
  `(unless ,condition
     (let ((errno (get-last-error)))
       (cffi:with-foreign-object (string 'wchar_t 256)
         (format-message (logior format-message-from-system format-message-ignore-inserts)
                         (cffi:null-pointer) errno 0 string 256 (cffi:null-pointer))
         (error-mmap errno (cffi:foreign-string-to-lisp string :encoding :utf-16))))))

(declaim (inline %mmap))
(defun %mmap (path size offset open-access open-disposition open-flags protection map-access)
  (declare (type fixnum open-access open-disposition open-flags protection map-access offset))
  (declare (optimize speed))
  (let ((fd invalid-handle-value))
    (declare (type (or null (unsigned-byte 64)) size))
    (declare (type cffi:foreign-pointer fd))
    (etypecase path
      (string
       (cffi:with-foreign-string (string path :encoding :utf-16)
         (setf fd (create-file string
                               open-access
                               (logior file-share-delete
                                       file-share-read
                                       file-share-write)
                               0
                               open-disposition
                               open-flags
                               0)))
       (check-windows (cffi:pointer-eq fd invalid-handle-value))
       (unless size
         (cffi:with-foreign-object (high 'dword)
           (let ((low (get-file-size fd high)))
             (declare (type (unsigned-byte 16) low))
             (check-windows (/= low invalid-file-size))
             (setf size (+ low (ash (cffi:mem-ref high 'dword) 16)))))))
      (null))
    (let* ((end (+ (the (unsigned-byte 64) size) offset))
           (handle (create-file-mapping fd
                                        0
                                        protection
                                        (ldb (byte 16 16) end)
                                        (ldb (byte 16 0) end)
                                        (cffi:null-pointer)))
           (success (map-view-of-file handle
                                      map-access
                                      (ldb (byte 16 16) offset)
                                      (ldb (byte 16 0) offset)
                                      size)))
      (handler-bind ((mmap-error (lambda (e)
                                   (declare (ignore e))
                                   (close-handle handle))))
        (check-windows success)
        (values handle fd size)))))

(defun flagp (flags &rest tests)
  (loop for test in tests
        always (find test flags)))

(defmacro set-flag (var flags test value)
  `(when (flagp ,flags ,test)
     (setf ,var (logior ,var ,value))))

(defun translate-open-access (flags)
  (let ((flag 0))
    (set-flag flag flags :read generic-read)
    (set-flag flag flags :write generic-write)
    flag))

(defun translate-open-disposition (flags)
  (if (flagp flags :create)
      (if (flagp flags :ensure-create)
          create-new
          open-always)
      (if (flagp flags :truncate)
          truncate-existing
          open-existing)))

(defun translate-open-flags (flags)
  (let ((flag file-attribute-normal))
    (set-flag flag flags :direct file-flag-no-buffering)
    (set-flag flag flags :file-sync file-flag-write-through)))

(defun translate-protection-flags (flags)
  (cond ((flagp flags :write)
         page-execute-readwrite)
        ((flagp flags :write)
         page-readwrite)
        ((flagp flags :read :exec)
         page-execute-read)
        ((flagp flags :read)
         page-readonly)
        (T
         (error "PROTECTION flags must include :READ."))))

(defun translate-access-flags (protection flags)
  (let ((flag (if (flagp protection :write)
                  file-map-write
                  file-map-read)))
    (set-flag flag protection :exec file-map-execute)
    (unless (or (set-flag flag flags :private file-map-copy)
                (flagp flags :shared))
      (error "MMAP flags must include either :PRIVATE or :SHARED."))
    flag))

(defun mmap (path &key (open '(:read)) (protection '(:read)) (mmap '(:private)) size (offset 0))
  (%mmap (translate-path path)
         size offset
         (translate-open-access open)
         (translate-open-disposition open)
         (translate-open-flags open)
         (translate-protection-flags protection)
         (translate-access-flags protection mmap)))

(define-compiler-macro mmap (&environment env path &key (open ''(:read)) (protection ''(:read)) (mmap ''(:private)) size (offset 0))
  `(%mmap ,(cfold env `(translate-path ,path) path)
          ,size ,offset
          ,(cfold env `(translate-open-access ,open) open)
          ,(cfold env `(translate-open-disposition ,open) open)
          ,(cfold env `(translate-open-flags ,open) open)
          ,(cfold env `(translate-protection-flags ,protection) protection)
          ,(cfold env `(translate-access-flags ,protection ,mmap) protection mmap)))

(defun munmap (addr fd size)
  (declare (ignore size))
  (check-windows (unmap-view-of-file addr))
  (when fd (check-windows (close-handle fd)))
  NIL)

(defun msync (addr fd size &key (flags '(:sync)))
  (check-windows (flush-view-of-file addr size))
  (when (find :sync flags)
    (check-windows (flush-file-buffers fd)))
  NIL)

(defun mprotect (addr size protection)
  (cffi:with-foreign-object (oldprotect 'dword)
    (check-windows (virtual-protect addr size (translate-protection-flags protection) oldprotect))
    NIL))

(define-compiler-macro mprotect (&environment env addr size protection)
  `(cffi:with-foreign-object (oldprotect 'dword)
     (check-windows (virtual-protect ,addr ,size ,(cfold env `(translate-protection-flags ,protection) protection) oldprotect))
     NIL))

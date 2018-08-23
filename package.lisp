#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:mmap
  (:nicknames #:org.shirakumo.fraf.trial.mmap)
  (:use #:cl)
  (:export
   #:mmap-error
   #:mmap
   #:munmap
   #:with-mmap))

(asdf:defsystem mmap
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Portable mmap (file memory mapping) utility library."
  :homepage "https://shinmera.com/docs/mmap/"
  :bug-tracker "https://shinmera.com/project/mmap/issues"
  :source-control (:git "https://shinmera.com/project/mmap.git")
  :serial T
  :components ((:file "package")
               (:file "generic")
               (:file "posix" :if-feature :unix)
               (:file "windows" :if-feature :windows)
               (:file "documentation"))
  :defsystem-depends-on (:trivial-features)
  :depends-on (:documentation-utils
               :pathname-utils
               :cffi)
  :in-order-to ((asdf:test-op (asdf:test-op :mmap-test))))

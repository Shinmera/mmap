#|
 This file is a part of mmap
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon .eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.trial.mmap)

(docs:define-docs
  (type mmap-error
    "Error signalled if the mmap attempt fails for some reason.

Possible reasons include, but are not limited to:
- File not found
- File access denied
- Out of memory
- Out of address space
- Mapping not allowed
- Invalid combination of flags

See MMAP
See CODE
See MESSAGE")

  (function code
    "The OS-specific error code returned for the mmap failure.

See MMAP-ERROR")

  (function message
    "The (hopefully) user-readable error message for the mmap failure.

See MMAP-ERROR")

  (function mmap
    "Map the given path or number of bytes into the address space.

PATH/SIZE can be either a pathname designator, or a fixnum. If it is a
fixnum, an anonymous file is mapped and the MMAP flag list must include the
flag :ANONYMOUS. If it is a path, then the contents of the given file on the
file system are mapped into the address space. The file contents can then be
read, written, or executed depending on the given flags as if normal memory
was accessed.

If the map attempt fails, an error of type MMAP-ERROR is signalled.
If the call succeeds, three values are returned:

  PTR  --- A CFFI:FOREIGN-POINTER that points to the start of the place in
           memory where the file contents have been mapped. The contents
           should be placed in increasing address order, unless the flag
           :GROWS-DOWN is active.
  FD   --- An opaque file descriptor. You should not touch this.
  SIZE --- The size of the region of memory that has been mapped in bytes.

All three values need to be passed on to MUNMAP completely unchanged. Any
change could cause severe issues.

The three options OPEN, PROTECTION, and MMAP are lists of flags. Not all of
those flags are portable, some are only allowed on Linux, some only on non-
Windows systems. To indicate support, the flags are marked as EVERY if they
are supported everywhere, POSIX if they do not have Windows support, and
LINUX if they are only available on Linux.

OPEN
- READ          --- [EVERY] Opens the file for read access.
- WRITE         --- [EVERY] Opens the file for write access.
- CREATE        --- [EVERY] Creates the file if it does not exist yet.
- ENSURE-CREATE --- [EVERY] Creates the file if it does not exist yet and
                            errors if it does.
- TRUNCATE      --- [EVERY] Truncates the file and replaces it if it exists.
- DIRECT        --- [EVERY] Causes system buffers to be bypassed.
- FILE-SYNC     --- [EVERY] Causes writes to the file to be flushed asap.
- DATA-SYNC     --- [POSIX] 
- APPEND        --- [POSIX] Causes writes to append to the file.
- NO-C-TTY      --- [POSIX] 
- NON-BLOCK     --- [POSIX] 
- NO-FOLLOW     --- [LINUX] 
- ASYNC         --- [LINUX] 
- DIRECTORY     --- [LINUX] 
- LARGE-FILE    --- [LINUX] 

PROTECTION
- READ          --- [EVERY] Allows reading from the memory region. The OPEN
                            flag :READ is required for this protection mode.
                            This flag is required on windows.
- WRITE         --- [EVERY] Allows writing to the memory region.
- EXEC          --- [EVERY] Allows executing code in the memory region.
- NONE          --- [POSIX] Prevents accessing the memory region.

MMAP
- PRIVATE       --- [EVERY] The underlying file is not changed if the memory
                            area is written to. Copy-on-write is employed to
                            ensure separation.
- SHARED        --- [EVERY] The underlying file is changed if the memory
                            area is written to and the change will be
                            visible to other processes. In this case the
                            OPEN flag :WRITE must be specified.
- NO-RESERVE    --- [LINUX] 
- LOCKED        --- [LINUX] 
- GROWS-DOWN    --- [LINUX] 
- ANONYMOUS     --- [LINUX] 
- POPULATE      --- [LINUX] 
- NON-BLOCK     --- [LINUX] 

See MUNMAP
See WITH-MMAP
See MMAP-ERROR")

  (function munmap
    "Unmaps the memory region, freeing the address space and its file.

The values passed to this function must be the ones retrieved from a call
to MMAP. Calling MUNMAP with the same values more than once will lead to
undefined consequences and may very well corrupt your system to crash. The
same goes for calling MUNMAP with values not directly returned by MMAP,
calling it with changed values returned by MMAP, or attempting to
dereference the PTR after a call to MUNMAP.

This function returns nothing useful.

This function may signal an MMAP-ERROR in case the operating system notices
a problem.

See MMAP
See MMAP-ERROR
See WITH-MMAP")

  (macro with-mmap
    "Map the file or number of bytes to a memory region within the body.

This is a convenience macro that calls MMAP with the given arguments,
binds the results to the variables ADDR and SIZE, and automatically ensures
that MUNMAP is called with the correct values when the body is exited.

It is safe to change the ADDR and SIZE bindings, though probably not very
good style to do so. It is NOT safe to save the ADDR and SIZE values
somewhere and use them outside of the dynamic scope of the body. Attempting
to do so is very likely going to burn your process to the ground.

See MMAP
See MUNMAP"))

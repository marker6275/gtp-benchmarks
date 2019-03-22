#lang racket/base

(provide define-test-env
         resolve-path-string)

(require syntax/parse/define
         racket/file
         racket/runtime-path
         (for-syntax racket/base))

(define-simple-macro (define-test-env (setup:id cleanup:id)
                       #:directories ([dir:id dirpath:expr] ...)
                       #:files ([filename:id path:expr contents] ...)
                       {~optional {~and #:provide provide-kw}})
  #:with maybe-provides (if (attribute provide-kw)
                            #'(provide filename ... dir ...)
                            #'(void))
  (begin
    (define-runtime-path dir dirpath) ...
    (define filename path) ...
    (define (setup)
      (cleanup)
      (make-directory dirpath) ...
      (display-to-file contents filename) ...)
    (define (cleanup)
      (for ([f (in-list (list filename ...))])
        (when (file-exists? f)
          (delete-file f)))
      (for ([d (in-list (list dir ...))])
        (when (directory-exists? d)
          (delete-directory/files d))))
    maybe-provides))

(define (resolve-path-string path-str)
  (path->string (path->complete-path (string->path path-str))))

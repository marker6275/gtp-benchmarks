#lang racket/base

(provide define-test-env
         resolve-path-string)

(require syntax/parse/define
         racket/file)

(define-simple-macro (define-test-env (setup:id cleanup:id)
                       ({~datum directories}
                        [dir:id dirpath:expr] ...)
                       ({~datum files}
                        [filename:id path:expr contents] ...))
  (begin
    (define dir dirpath) ...
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
    (provide filename ... dir ...)))

(define (resolve-path-string path-str)
  (path->string (path->complete-path (string->path path-str))))

(module data racket/base
  (#%module-begin
   (struct Array (shape size strict? strict! unsafe-proc) #:prefab)
   (struct Settable-Array Array (set-proc) #:prefab)
   (struct Mutable-Array Settable-Array (data) #:prefab)))

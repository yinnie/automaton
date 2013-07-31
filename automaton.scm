;;implementation of a simple finite state machine
;;horizonal: transitions  
;;vertical:  states
;;         c    a    d   r
;;   init more  -   -    -
;;   more  -   more more end
;;   end   -    -    -    -  

;;1. states as functions
(define machine0
  (lambda (str)
    (letrec 
       ((init 
          (lambda (str)
           (cond
            ((null? str) #f)
            (else
            (case (car str)
             (('c) (more (cdr str)))
             (else #f))))))
       (more
          (lambda (str)
           (cond
            ((null? str) #f)
            (else
              (let ((first (car str))
                    (rest  (cdr str)))
                (case (first)
                 (('c) #f)
                 (('a) (more rest))
                 (('d) (more rest))
                 (('r) (end rest))))))))
       (end
          (lambda (str)
            (cond
              ((null? str) #t)
             (else #f)))))
     (init str))))

;;2. states as functions. functions take only one char of the string. 
(define machine
 (lambda (str)
  (let ((start-state
        (letrec ((init(lambda (b)
                              (cond
                                ((null? b) #f)
                                (else
                                 (case b
                                    ('c more)
                                    (else #f))))))
                 (more(lambda (b)
                              (cond
                                ((null? b) #f)
                                (else
                                  (case b
                                    ('c #f)
                                    ('a more)
                                    ('d more)
                                    ('r end))))))
                 (end (lambda (b)
                              (cond
                                ((null? b) #t)
                                (else #f)))))
              init)))
      (letrec ((driver (lambda (str state)
                          (cond
                            ((boolean? state) state)
                            ((null? str) (state str))
                            (else
                              (driver (cdr str) (state (car str))))))))
           (driver str start-state)))))

;;3. treat states as symbols. use pattern matching

           
                        
          
          
          
          
          
          
          
          
          
          
          
          
          
                  

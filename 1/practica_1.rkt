 #lang racket
(require dyoo-while-loop)

; Ejercicio 1
(define (tiempo  años )
  (list
   (* años 365 24)
   (* años 365 24 60)
   (* años 365 24 60 60)
   )
 
 )
(display "Ejercicio1\n")
(tiempo 20)
(tiempo 0 )

;Ejercicio 2
;func: función booleana
;lst una lista

(define (filtro func lst )

  (cond ( (null?  lst )  null) ;si es vacía regresa una lista vácia
        ( (func (first lst)) ; si la cabeza de lista cumple la condición
          (cons  (first lst ) (filtro func (rest lst)))) ; lo agregamos a nuestra lista y recursivamente llamamos a filtro en
          ;el resto de la lista

        ; llamanos filtro en el resto de la lista
        ; si la cabeza no cumple con la condición
        ( else (filtro func (rest lst)))
             )
        
        )

(display "Ejercicio2\n")

(filtro even? '(1 2 3 4 5 6 7 8 9 ) )

(filtro even? (list 1 2 3 4 5 6 7 8 9 10 11 ) )

(define (escero x)
  (equal?  x 0 )
  )

(filtro escero '(0 1 2 3 100) )

;Ejercicio 3
; List of Numbers
; LON contiene el atributo numbers
; que es una lista de enteros
(struct LON (numbers ))


; a)
;lon: LON lista de números
(define (any-even? lon)
  (if  (null? ( LON-numbers lon  ) )
       #f
       (if (even?  (first (LON-numbers lon)))
           #t
           (any-even?   (LON ( rest (LON-numbers lon))))
       )
   )
  )


(display "Ejercicio3\n")

(define lon (LON  (list 0 12 23 4425  10) ) )

(any-even? lon)
(any-even?  (LON (list 3 5 7 9 )) )
(any-even? (LON  (list )))
(any-even? (LON null))

;b)
;lon: LON lista de números
(define (total-length lon  )

  (if (null? (LON-numbers lon) )
      0
      (+  1   (total-length (LON (rest (LON-numbers lon)))) )
      )
  )


(total-length lon )

(total-length (LON (list 0) ) )

(total-length (LON null ))


(total-length (LON (list 0 1 2 3 4 5 6 7  8 9 10 ) ))



; Ejercicio 4

;Estructura nodo.
;l nodo izquiero (null |  node)
;r nodo derecho (null | node)
; valor
(struct node ([l #:mutable] [r #:mutable] [value #:mutable ]))


; value; entero
; tree-node: struct node 
(define (div-tree  value)

  (define foundone #false)

  (define tree (node null null value) )

  (for ( [i (in-range 2 (ceiling (sqrt value )) )] )
   
          #:break foundone
          
          (cond
            (( = (modulo value i ) 0 )
             
             (set!  foundone #true ) ; i es divisor de value
             
         
             (set! tree ( node (div-tree i ) (div-tree (quotient value i))   value))
             )
           )

       )
  tree 
  )



(display "Ejercicio4\n")
;n: struct node
;Recorre el arbol enraizado en n
(define (traverse n) ; n node

  (define values  null )

  (if (null? n)
      values   
   
      (begin
        (set! values  (cons  (node-value n ) values    ))
        (set! values (append values (traverse  (node-l n))) )
        (set! values (append values (traverse (node-r n))) )
       )
  )

  values
)


;n: struct node 
(define (traverse_leafs n) ; n node

  (define values  '() )

  (cond
    ((null? n)
      values)

      (else
        (cond
          (
           (and (null? (node-l n)) (null?  (node-r n)))
           
           (set! values (cons  (node-value n) values ))
           )
        
          (else
         
           (set! values (append values (traverse_leafs (node-l n))))

           (set! values (append values (traverse_leafs (node-r n))))
           )
         )
        )
       )
  
  values
)

;n: entero
(define (prime-fac n)
  (define tree (div-tree n ))
  (define factors (traverse_leafs tree))

  factors
  )
(display "TEST div-tree\n")
(define pf1 (prime-fac  2))

(define pf2 (prime-fac  7))

(define pf3 (prime-fac 35))

(define pf4 (prime-fac  90))

(define pf5 (prime-fac  154))

(define pf6 (prime-fac  305))

(define pf7 (prime-fac  35324))


pf1
pf2
pf3
pf4
pf5
pf6
pf7


; Ejercicio 5
(struct leafy (l r h )  #:transparent )

; Function to construct leafy with optional height parameter
(define (make-leafy  l r [h 0]  )
  (leafy l r  h)
  )

(define (height root); receives a leafy tree rooted at root

  (define lh 0)
  (define rh 0)
  (define h 0 )
  
  (cond
    (
     ( not (leafy? root) )
     h
     )
    (else
     (set!  lh (height   (leafy-l root))) ; compute left tree height
     (set!  rh (height (leafy-r root)))
     (set! h (+ (max lh rh) 1) )
     )
    )
  h
  )

(define  (is_in leafy_list t)
  (define value #f)

  (for ([tree  leafy_list])

    ( if (equal? tree t)

      (set! value #t)

      value
    )
   )

  value
  
  )

;n: entero
; Construye una lista con todos los arboles leafy
; de altura a lo más n
(define (list_trees n)
  (define trees null)
  (define first_trees null)

  (cond

    ((= n 0 )
      (set! trees (cons "leaf" trees)))

    ((= n 1)

     (set! trees (cons (make-leafy "leaf" null  ) trees   ))

     (set! trees (cons (make-leafy null "leaf"  ) trees   ))

     (set! trees (cons (make-leafy "leaf" "leaf"  ) trees   ))

     (set! trees (append trees (list_trees 0 )))

     )

    (else

    ;(define subtrees (filtro (lambda (t) (= (height t) (- n 1) )) ( list_trees (- n 1))))
     
    (for ([ Tree  (list_trees (- n 1)) ])

      (cond
        ((not (is_in trees Tree))
      
          (set! trees (cons Tree  trees ))
        )
      )
      
      (for ([ i (in-range 0 (- n 1))])

        (define subtrees (list_trees  i))

        
        
        (for  ([t  subtrees])
            
             (define tree_l (make-leafy Tree t) )
          
             

             (cond
               ( (not (is_in trees tree_l))

                 (set! trees (cons tree_l trees) )

                )

               )

         
             (cond
                   ((or (< i (- n 1)) (not (equal? t Tree )))
 
                    (set! trees (cons (make-leafy t Tree) trees))

                    (define new_tree  (make-leafy Tree Tree ))

                    (cond
                      ((not (is_in trees new_tree)) 

                       (set! trees (cons new_tree trees))
                       )
                     )

                    )
              )

               )
              
              )
            )
          )
        )
      

  (define result (append first_trees trees) )
  result
  )

(display "LIST TREES \n")

(define lt  (list_trees 2 ))
(display "\n")
(display  (string-append "Lenght list of trees : "  (number->string (length lt))))
(display "\n")


(define (repeat s  n)

  (define ret "" )

  (for ([i n])
    (set! ret (string-append  ret s ) )
    )
  ret
)

(define (print-tree root level)
  (cond
    ((leafy? root)
     
     (print-tree (leafy-l root) ( + level 1))
     (display "\n")
     (display (string-append (repeat "    " level) "-> O"))
     (display "\n")

     (print-tree (leafy-r root) (+ level 1 ))
     )
    
     
     ((string? root) 
       (display (string-append (repeat "    " level) "->leaf"))
     )
      
    )
   
 )



(define (print-list-trees trees)

  (for ([t trees])
    (display "treee \n")

    (print-tree t 0)

    (display "\n")
    )
  )


(print-list-trees lt)




; Ejercicio 6
;root: leafy
;depth of leftmost leaf
(define (find_left_depth root)

  (define d 0)

  (while (leafy? root)
         
         (set! d (+ d 1))
         (set! root (leafy-l root))
         
         )

  d
  )

(find_left_depth "leaf" )

(find_left_depth (make-leafy null null) )

(find_left_depth (make-leafy "leaf" null) )

(find_left_depth (make-leafy "leaf" "leaf") )


(find_left_depth (make-leafy (make-leafy "leaf" "leaf") "leaf") )



(define  (is_perfect_aux root d [level 0 ] )

  (define value #f )

  (cond
    ((not (leafy? root))
     (set! value #t)
     )

    (
     (and (not(leafy? (leafy-l root)))  (not (leafy? (leafy-r root))))  

     (cond
       (
        (and  (string? (leafy-l root)) (string? (leafy-r root)))
        (set! value (= d (+ level 1)))
       )

       (
        (and  (null? (leafy-l root)) (null? (leafy-r root)))
        (set! value (= d (+ level 1)))
       )
     )
     
     )
 
    ((or  (null? (leafy-l root))   (null? (leafy-r root)))
     (set! value #f)
     )

    ((or  (string? (leafy-l root))   (string? (leafy-r root)))
     (set! value #f)
     )

    (else

     (set! value (and (is_perfect_aux (leafy-l root) d (+ level 1)) (is_perfect_aux (leafy-r root) d (+ level 1))))
     
     )

    )
  value
  )


(define (is_perfect root)
  (define d (find_left_depth root))

  (is_perfect_aux root d)
 
  )

(is_perfect "leaf")

(is_perfect  (list-ref lt 0 )   ) ;t

(is_perfect  (list-ref lt 1 )   ) ;t

(is_perfect  (list-ref lt 2 )   ) ;f

(is_perfect  (list-ref lt 3 )   ) ;f

(is_perfect  (list-ref lt 10 )   ) ;t


(print-tree (list-ref lt 10) 0  )

(display "\n")

(height (list-ref lt 10))
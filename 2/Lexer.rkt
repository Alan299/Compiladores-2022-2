#lang nanopass

;; Bibliotecas chidas para lexear
(require parser-tools/lex
         parser-tools/lex-plt-v200
         (prefix-in : parser-tools/lex-sre);Operadores
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out));Exporta todos los identificadores que están definidos en el  nivel
;de fase relevante dentro del módulo de exportación, y que tienen el mismo contexto léxico

(define-tokens a (NUM VAR))
(define-empty-tokens b (LP RP + - EOF  DIVISION POWER NEG TBOOL ELSE FALSE FST FUN IF  TINT IS LET TLIST  MATCH REC SND THEN TRUE QUIT WITH TARROW DARROW CONS SEMICOLON2 MOD COMMA  COLON LESS EQUAL LBRACK RBRACK ALTERNATIVE   ))

; sre : S-regular expressions
(define calc-lexer
           (lexer
             [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) ; (a-z | A-Z)^+
              ; =>
              (token-VAR (string->symbol lexeme))]

             [(::  (:or #\- (epsilon)) (:: (:* (char-range #\0 #\9)) (:: (:or (:: #\. (char-range #\0 #\9)) (:: (char-range #\0 #\9)) #\.) (:* (char-range #\0 #\9)))))
              ; =>
              (token-NUM (string->number lexeme))]

             [#\+
              ; =>
              (token-+)]

             [#\-
              ; =>
              (token--)]

             [#\*
               ; =>
               (string->symbol lexeme)]

             ;usa los define-empty-tokens
             [#\(
              ;=>
              (token-LP)]

             [#\)
              ;=>
              (token-RP)]

             [#\/
              ; = >
              (token-DIVISION)]

             [#\^
              ; =>
              
              (token-POWER)]

             ["not"
              ; =>
              (token-NEG)]

             ["bool"

              ; =>

              (token-TBOOL)]

             ["else"
              ; =>
              (token-ELSE)
              ]

             ["false"
              ; =>
              (token-FALSE)]

             ["fst"
              ;=>
              (token-FST)]

             ["fun"
              ; = >
              (token-FUN)]

             ["if"
              ;=>
              (token-IF)]

             ["int"
              ; =>
              (token-TINT)]

             ["is"
              ;=>
              (token-IS)]

             ["let"
              ; =>
              (token-LET)]

             ["list"
              ; =>
              (token-TLIST)]

             ["match"
              ; = >
              (token-MATCH)]
             
             ["rec"
              ; = >
              (token-REC)]

             ["snd"
              ; = >
              (token-SND)]
             
             ["then"
              ; = >
              (token-THEN)]

             ["true"
              ; = >
              (token-TRUE)]

             [":quit"
              ; = >
              (token-QUIT)]

             ["with"
              ; = >
              (token-WITH)]

             ["->"
              ; = >
              (token-TARROW)]

             ["=>"
              ; = >
              (token-DARROW)]

             ["::"
              ; = >
              (token-CONS)]

             [";;"
              ; = >
              (token-SEMICOLON2)]

             ["%"
              ; = >
              (token-MOD)]
             
             [","
              ; = >
              (token-COMMA)]

             [":"
              ; = >
              (token-COLON)]
             
             ["<"
              ; = >
              (token-LESS)]

             ["="
              ; = >
              (token-EQUAL)]

             ["["
              ; = >
              (token-LBRACK)]

             ["]"
              ; = >
              (token-RBRACK)]
             
             ["|"
              ; = >
              (token-ALTERNATIVE)]
             
             
             [whitespace
              ; =>
              (calc-lexer input-port)]

             [(eof)
              (token-EOF)]


             )


  )

(define-struct arith-exp (op e1 e2) #:transparent)
(define-struct num-exp (n) #:transparent)
(define-struct var-exp (i) #:transparent)



;cada llamada a calc-lexer en input
; devuelve el siguiente token

(display "Test calc-lexer with 182 * 23 \n")
(define input  (open-input-string "182 * 23" ))
(calc-lexer input)
(calc-lexer input)
(calc-lexer input)


;solo obtiene un token
(display "calc-lexer on:  3 - 3.3 + 6 \n")
(calc-lexer  (open-input-string "3 - 3.3 + 6"))

; port: result of open-input-string
(define (get_tokens port)

  (define output null)

  (define element (calc-lexer port  ))

  (set! output (cons  element output))

  

  (when (not(eq?  element (token-EOF)))
         (set! output  (append output (get_tokens port)))
        
   )

  output
  )

(display "Test get_tokens \n")

(get_tokens  (open-input-string   "12.1 * 23"))

(get_tokens  (open-input-string   "182 * 23"))

(get_tokens  (open-input-string    "(182 * 23)"))

(get_tokens (open-input-string  "((:) 0 ((:) 1 ((:) 2 [])))"))


;port: el port del archivo que se quiere procesar
;Regresa la primer conincidencia de un token en la entrada
(define (minHS-lexer port)
  (calc-lexer port )
  )

(display "Prueba minHS-lexer \n")

(minHS-lexer (open-input-string "3 - 3.3 + 6"))

(minHS-lexer (open-input-string "182 * 23"))

(minHS-lexer (open-input-string "(182 * 23)"))
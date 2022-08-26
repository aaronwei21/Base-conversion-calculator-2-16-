;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |base conversions|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define A 10)
(define B 11)
(define C 12)
(define D 13)
(define E 14)
(define F 15)

;; dec-convert
;; converts number from non-decimal base to decimal
;; string -> natural

(check-expect (dec-convert "1000110111" 2) 567)
(check-expect (dec-convert "D9A94" 16) 891540)

(define (dec-convert n1 base)
  (local [(define (d-c lon pos)
            (cond [(empty? lon) 0]
                  [else
                   (+ (* (first lon) (expt base pos))
                      (d-c (rest lon) (add1 pos)))])) 
          ]
    (d-c (make-lon n1) 0))) 

;; helper: make-lon
;; makes entries of dec-convert into lists
(define (make-lon s1)
  (local [(define (first-dig s) (substring s 0 1))

          (define (convert-fig s)
            (cond [(string=? s "A") A]
                  [(string=? s "B") B] 
                  [(string=? s "C") C]
                  [(string=? s "D") D]
                  [(string=? s "E") E]
                  [(string=? s "F") F]
                  [else
                   (string->number s)])) 

          (define (m-l s lol)
            (cond [(string=? s "") lol]
                  [else
                   (m-l (substring s 1 (string-length s))
                        (cons (convert-fig (first-dig s)) lol))]))]  
    (m-l s1 empty)))         

;;==============================================================================
;; change-base--dec
;; converts number from decimal to non-decimal base
;; natural -> string

(check-expect (change-base--dec 49 2) "110001")
(check-expect (change-base--dec 65468 16) "FFBC")
(check-expect (change-base--dec 720 16) "2D0")

(define (change-base--dec n1 base)
  (local [(define (select-digit n) 
            (cond [(= 10 n) "A"]
                  [(= 11 n) "B"]
                  [(= 12 n) "C"]
                  [(= 13 n) "D"]
                  [(= 14 n) "E"] 
                  [(= 15 n) "F"]
                  [else
                   (number->string n)]))

          (define (to-num lod)
            (foldr string-append "" lod))

          (define (c-b n lod)
            (cond [(zero? n) (to-num lod)] 
                  [else
                   (c-b (/ (- n (modulo n base)) base) 
                        (cons (select-digit (modulo n base)) lod))]))    

          ]
    (c-b n1 empty)))

;;==============================================================================
;; change-base
;; converts number from one base to another
;; string number number -> string

(define (change-base s b1 b2)
  (local [(define to-dec (dec-convert s b1))
          (define converted (change-base--dec to-dec b2))
          ]
    converted)) 

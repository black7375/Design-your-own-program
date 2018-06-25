;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname section3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;-------------------------Code 3.1-------------------------
(define (f w h) (* w h))
(define (g w h)
  (+ (* 2 w)
     (* 2 h)))

;-------------------------Code 3.2-------------------------
(f 1 1)
(g 1 1)

(f 1 2)
(g 1 2)

(f 2 1)
(g 2 1)

(f 2 2)
(g 2 2)

;-------------------------Code 3.3-------------------------
(define (rect-size    width height)
  (* width height))
(define (rect-length  width height)
  (+ (* 2 width  )
     (* 2 height )))

;-------------------------Code 3.4-------------------------
(define (rect-length-upgrade width height)
  (* 2 (+ width height)))

;-------------------------Code 3.5-------------------------
(define won    1000)
(define dollar 1   )
(define euro   0.8 )
won
dollar
euro

;-------------------------Code 3.6-------------------------
(define usd 1           )
(define eur (* 0.8  usd))
(define krw (* 1000 usd))

;-------------------------Code 3.7-------------------------
(define PI #i3.14)

;-------------------------Code 3.8-------------------------
;-------------------------Solution 3.1
(define cny (* 6 usd))
(cond
  [(<  usd (/ 0.7  eur)) 'EUR]
  [(<= usd (/ 1000 krw)) 'KRW]
  [(>  usd (/ 6.5  cny)) 'CNY]
  [else                  'USD])

;-------------------------Code 3.9-------------------------
;-------------------------Solution 3.2
(define (krw->usd won)
  (cond
    [(or (>= 100000    won)
         (<  100000000 won)) (/ (- won (* 0.03  won)) krw)]
    [(>=        1000000 won) (/ (- won (* 0.04  won)) krw)]
    [(>=       10000000 won) (/ (- won (* 0.05  won)) krw)]
    [(>=       50000000 won) (/ (- won (* 0.04  won)) krw)]
    [(>=      100000000 won) (/ (- won (* 0.035 won)) krw)]))

(krw->usd 20000000)

;-------------------------Code 3.10-------------------------
;-------------------------Solution 3.3
;----------exchange
(define exch-level1 100000   )
(define exch-level2 1000000  )
(define exch-level3 10000000 )
(define exch-level4 50000000 )
(define exch-level5 100000000)

(define (exch-usd money exch-rate)
  (/ money exch-rate))

;----------charge
(define charge-level1 0.03 )
(define charge-level2 0.035)
(define charge-level3 0.04 )
(define charge-level4 0.05 )

(define (charge-calc charge-level money)
  (- money (* charge-level money)))

;----------krw->usd exchange
(define (krw->usd2 won)
  (cond
    [(>  0           won)      'Error-*less-than-0*]
    [(or (>= exch-level1 won)
         (<  exch-level5 won)) (exch-usd (charge-calc charge-level1 won) krw)]
    [(>= exch-level2      won) (exch-usd (charge-calc charge-level3 won) krw)]
    [(>= exch-level3      won) (exch-usd (charge-calc charge-level4 won) krw)]
    [(>= exch-level4      won) (exch-usd (charge-calc charge-level3 won) krw)]
    [(>= exch-level5      won) (exch-usd (charge-calc charge-level2 won) krw)]))

;----------Test
(krw->usd2 20000000)


;-------------------------Test 3.10-------------------------
;----------exch-usd
;;Constract: exch-usd ; number, number->usd
;;Purpose: Exchange money to USD with exchanging rate.

;;;;;TODO: Delete.
;;Example: Input       -> Output
;;         1000, krw   -> 1
;;         0.8,  eur   -> 1
;;         6,    cny   -> 1
;;;;;;;;;;;;;;;;;;;;;;

(exch-usd 1000 krw) ;1
(exch-usd 0.8  eur) ;1
(exch-usd 6    cny) ;1

;----------charge-calc
;;Constract: charge-calc ; number, number->number
;;Purpose: Calculate money excluding commissions.

;;;;;TODO: Delete.
;;Example: Input     -> Output
;;         0.1,  100 -> 90
;;         0.03, 100 -> 97
;;;;;;;;;;;;;;;;;;;;;;

(charge-calc 0.1  100) ;90
(charge-calc 0.03 100) ;97

;----------krw->usd
;;Constract: krw->usd ; number->number
;;Purpose: Exchange KRW to USD and make a commission based on the amount.

;;;;;TODO: Delete.
;;Example: Input     -> Output
;;         -10       -> Error
;;         0         -> 0
;;         100000    -> 97
;;         1000000   -> 960
;;         10000000  -> 9500
;;         50000000  -> 48000
;;         100000000 -> 96500
;;         200000000 -> 194000
;;;;;;;;;;;;;;;;;;;;;;

(krw->usd2 -1000    ) ;Error
(krw->usd2 0        ) ;0
(krw->usd2 100000   ) ;97
(krw->usd2 1000000  ) ;960
(krw->usd2 10000000 ) ;9500
(krw->usd2 50000000 ) ;48000
(krw->usd2 100000000) ;96500
(krw->usd2 200000000) ;194000

;-----Compare with krw->usd
(krw->usd  -1000    ) ;Error -> -0.97 Problem!!!
(krw->usd  0        ) ;0
(krw->usd  100000   ) ;97
(krw->usd  1000000  ) ;960
(krw->usd  10000000 ) ;9500
(krw->usd  50000000 ) ;48000
(krw->usd  100000000) ;96500
(krw->usd  200000000) ;194000

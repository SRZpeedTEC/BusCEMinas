#lang racket

(require rackunit "../src/logicaJuego.rkt")

(define t (nueva-partida 8 10 'facil))
(check-equal? (length t) 8)
(check-equal? (length (first t)) 10)

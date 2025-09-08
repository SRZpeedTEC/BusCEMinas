#lang racket
#| LOGICA DEL JUEGO |#

(provide crear-matrizJuego )

(define (crear-matrizJuego filas colums)
  (define celda '(0 0 0))
  (build-list filas (lambda (_) (build-list colums (lambda (_) celda)))))


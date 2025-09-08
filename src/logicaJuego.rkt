#lang racket
#| LOGICA DEL JUEGO |#

(provide crear-matrizJuego )

;; Creamos Matriz (((BOMBA?, ESTADO, ADYACENTES) , (BOMBA?, ESTADO, ADYACENTES)))

(define (crear-matrizJuego filas colums)
  (define celda '(0 0 0))
  (build-list filas (lambda (_) (build-list colums (lambda (_) celda)))))

(provide descubrir marcar actualizarEstado)


;; in-range? : n min max  -> #t si min <= n < max
(define (in-range? n minimo maximo)
  (and (<= minimo n) (< n maximo)))

;; Actualizamos el estado del tablero

(define (actualizarEstado matrizActual filaSel colSel click)
  
  (define filas (length matrizActual)) ;; Conseguimos tamano filas
  (define columnas  (length (car matrizActual))) ;; Conseguimos tamano columnmas

  ;; Recreamos la fila si se cambio, si no pegamos la fila intacta
  (define (actualizarFila fila j)
    (cond
      [(null? fila) '()]
      [(= j colSel)
       (cons (list (list-ref (car fila) 0)
                   click
                   (list-ref (car fila) 2))  ;; Actualizamos fila, ej. (0, 0, 0) => (0, 1, 0) dependiendo de click (izquierdo = 1, derecho = 2)
             (actualizarFila (cdr fila) (+ j 1)))] ;; Terminamos de reconstruir
      [else
       (cons (car fila) (actualizarFila (cdr fila) (+ j 1)))]))  ;;  
  
  (define (actualizarMatriz matriz i) ;; Misma logica
    (cond
      [(null? matriz) '()]
      [(= i filaSel)
       (cons (actualizarFila (car matriz) 0)
             (actualizarMatriz (cdr matriz) (+ i 1)))]
      [else
       (cons (car matriz)
             (actualizarMatriz (cdr matriz) (+ i 1)))]))
  
  (if (and (in-range? filaSel 0 filas) (in-range? colSel 0 columnas))
      (actualizarMatriz matrizActual 0)
      matrizActual))


;; Se presiono click izquierdo, llamamos a descubrir
(define (descubrir matrizActual filaSel colSel)
  (actualizarEstado matrizActual filaSel colSel 1))

;; Se presiono click derecho, llamamos a marcar
(define (marcar matrizActual filaSel colSel)
  (actualizarEstado matrizActual filaSel colSel 2))


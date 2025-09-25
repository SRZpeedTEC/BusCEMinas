#lang racket
#| LOGICA DEL JUEGO |#

(require racket/list
         racket/set)

;; Dificultad -> porcentaje
(define (difficulty->ratio d)
  (cond [(or (eq? d 'facil)   (and (string? d) (string-ci=? d "facil")))   0.05]
        [(or (eq? d 'medio)   (and (string? d) (string-ci=? d "medio")))   0.10]
        [(or (eq? d 'dificil) (and (string? d) (string-ci=? d "dificil"))) 0.15]
        [else (error 'difficulty->ratio (format "Dificultad desconocida: ~a" d))]))

;; Utilidades tablero (listas)
(define (board-dimensions board)
  (values (length board)
          (if (null? board) 0 (length (first board)))))

(define (make-empty-board rows cols)
  (for/list ([r (in-range rows)])
    (for/list ([c (in-range cols)])
      (list 0 0 0)))) ; '(bomba click ady)

;; Posiciones y cantidad
(define (all-positions rows cols)
  (for*/list ([r (in-range rows)]
              [c (in-range cols)])
    (cons r c)))

(define (num-bombs rows cols ratio)
  (define total (* rows cols))
  (define n (inexact->exact (floor (* ratio total))))
  (cond [(<= total 1) 0]
        [else (max 1 (min (- total 1) n))])) ; al menos 1, deja 1 libre

(define (pick-positions rows cols k)
  (take (shuffle (all-positions rows cols)) k))

;; Helpers de tablero (listas, puros)
(define (get-cell board r c)
  (list-ref (list-ref board r) c))

(define (set-cell board r c new)
  ;; devuelve un NUEVO board con (r,c) reemplazado por 'new'
  (define row (list-ref board r))
  (define new-row
    (append (take row c) (list new) (drop row (add1 c))))
  (append (take board r) (list new-row) (drop board (add1 r))))

(define (set-click board r c val)
  ;; cambia el segundo campo (click) a val en (r,c)
  (define cell (get-cell board r c)) ; '(b c a)
  (set-cell board r c (list (first cell) val (third cell))))


;; Colocar bombas (puro)
;; Cualquiera de estas posiciones queda exactamente '(1 0 0)
(define (place-bombs/list board bomb-positions)
  (define pos-set (list->set bomb-positions)) ; equal?-set
  (for/list ([row board] [r (in-naturals)])
    (for/list ([cell row] [c (in-naturals)])
      (if (set-member? pos-set (cons r c))
          (list 1 0 0)
          cell))))


;; -------------------------
;; API principal
;; -------------------------
;; Devuelve (values nuevo-tablero lista-de-posiciones)
(define (init-bombs/list board dificultad)
  (define-values (rows cols) (board-dimensions board))
  (define ratio (difficulty->ratio dificultad))
  (define k     (num-bombs rows cols ratio))
  (define spots (pick-positions rows cols k))
  (values (place-bombs/list board spots) spots))

;; Conveniencia: crea tablero vacío y ya con bombas
(define (crear-tablero-inicial dificultad rows cols)
  (define empty (make-empty-board rows cols))
  (define-values (with-bombs _spots) (init-bombs/list empty dificultad))
  with-bombs)

(provide crear-matrizJuego
         difficulty->ratio
         make-empty-board
         init-bombs/list
         crear-tablero-inicial
         descubrir marcar actualizarEstado)


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


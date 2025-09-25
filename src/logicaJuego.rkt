#lang racket
#| LOGICA DEL JUEGO |#

(require racket/list
         racket/set)

;; Dificultad -> porcentaje
(define (difficulty->ratio d)
  (cond [(or (eq? d 'facil)   (and (string? d) (string-ci=? d "facil")))   0.10]
        [(or (eq? d 'medio)   (and (string? d) (string-ci=? d "medio")))   0.15]
        [(or (eq? d 'dificil) (and (string? d) (string-ci=? d "dificil"))) 0.20]
        [else (error 'difficulty->ratio (format "Dificultad desconocida: ~a" d))]))

;; Utilidades tablero (listas)
(define (board-dimensions board)
  (values (length board)
          (if (null? board) 0 (length (first board)))))

(define (make-empty-board rows cols)
  (for/list ([r (in-range rows)])
    (for/list ([c (in-range cols)])
      (list 0 0 0)))) ; '(bomba click adyacentes)

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

;; Vecinos y adyacentes
(define neighbors-deltas
  '((-1 -1) (-1 0) (-1 1)
    ( 0 -1)         ( 0 1)
    ( 1 -1) ( 1 0)  ( 1 1)))

(define (in-bounds? rows cols r c)
  (and (in-range? r 0 rows) (in-range? c 0 cols)))

(define (adjacent-bombs board r c)
  (define-values (rows cols) (board-dimensions board))
  (for/sum ([d neighbors-deltas])
    (define rr (+ r (first d)))
    (define cc (+ c (second d)))
    (if (and (in-bounds? rows cols rr cc)
             (= (first (get-cell board rr cc)) 1)) ; bomba?
        1 0)))

(define (rellenar-adyacentes board)
  ;; Devuelve board con el 3er campo (ady) calculado para TODAS las celdas
  (define-values (rows cols) (board-dimensions board))
  (for/list ([r (in-range rows)])
    (for/list ([c (in-range cols)])
      (define cell (get-cell board r c)) ; '(b c a)
      (define b (first  cell))
      (define c2 (second cell))
      (define a (if (= b 1) 0 (adjacent-bombs board r c))) ; bombas mantienen 0
      (list b c2 a))))


;; Devuelve (values nuevo-tablero lista-de-posiciones)
(define (init-bombs/list board dificultad)
  (define-values (rows cols) (board-dimensions board))
  (define ratio (difficulty->ratio dificultad))
  (define k     (num-bombs rows cols ratio))
  (define spots (pick-positions rows cols k))
  (values (place-bombs/list board spots) spots))

(define (crear-tablero-inicial dificultad rows cols)
  (define empty (make-empty-board rows cols))
  (define-values (with-bombs _spots) (init-bombs/list empty dificultad))
  (rellenar-adyacentes with-bombs)) ;; Se calcula de una vez las adyacencias


;; Creamos Matriz (((BOMBA?, ESTADO, ADYACENTES) , (BOMBA?, ESTADO, ADYACENTES)))


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
;; Revela una celda (2º campo = 1)
(define (revelar board r c)
  (set-click board r c 1))

;; DESCUBRIR: permite descubrir aunque esté marcada (clk=2). Solo bloquea si ya está revelada (clk=1).
(define (descubrir board r0 c0)
  (define cell0 (get-cell board r0 c0))
  (define clk0 (second cell0))
  (cond
    [(= clk0 1)board]  ; ya revelada → no hacer nada
    [(= clk0 2) board]  ; marcada para no hacer BFS sobre esta porque tiene bandera (implementado por santiago)
    [else
     (define b0 (first cell0))
     (define a0 (third cell0))
     (cond
       [(= b0 1)
        (revelar board r0 c0)]                  ; clic en bomba (manejo de perder aparte)

       [(> a0 0)
        (revelar board r0 c0)]                  ; número > 0: solo esa

       [else
        ;; a0 = 0 → BFS con visitados (sin recalcular adyacentes)
        (let loop ((queue   (list (cons r0 c0)))
                   (visited (set (cons r0 c0)))
                   (B       (revelar board r0 c0)))
          (cond
            [(null? queue) B]
            [else
             (define r (car (car queue)))
             (define c (cdr (car queue)))
             (define-values (rows cols) (board-dimensions B))

             (let-values ([(B1 V1 tail)
                           (for/fold ([Bacc B] [Vacc visited] [tail '()])
                                     ([d neighbors-deltas])
                             (define rr (+ r (first d)))
                             (define cc (+ c (second d)))
                             (cond
                               [(not (in-bounds? rows cols rr cc))
                                (values Bacc Vacc tail)]
                               [else
                                (define cellN (get-cell Bacc rr cc))
                                (define b (first  cellN))
                                (define k (second cellN))
                                (define a (third  cellN))
                                (cond
                                  [(= b 1)
                                   (values Bacc Vacc tail)]               ; bomba: no revelar
                                  [(or (= k 1) (set-member? Vacc (cons rr cc)))
                                   (values Bacc Vacc tail)]               ; ya revelada/visitada
                                  [else
                                   (define Brev (revelar Bacc rr cc))
                                   (if (= a 0)
                                       (values Brev (set-add Vacc (cons rr cc))
                                               (cons (cons rr cc) tail))  ; encola ceros
                                       (values Brev (set-add Vacc (cons rr cc)) tail))])]))])
               (loop (append (cdr queue) (reverse tail)) V1 B1))]))])]))


;; Se presiono click derecho, llamamos a marcar
(define (marcar matrizActual filaSel colSel)
  (define cell (get-cell matrizActual filaSel colSel))
  (if (= (second cell) 1) ; ya revelada -> no marcar
      matrizActual
      (actualizarEstado matrizActual filaSel colSel 2)))

;; Inspeccionar el tablero

(define (row-lost? row)
  (cond
    [(null? row) #f]
    [else
     (define cell (car row))       ; '(b c a)
     (cond
       [(and (= (first cell) 1)    ; bomba
             (= (second cell) 1))  ; revelada
        #t]
       [else (row-lost? (cdr row))])]))

(define (board-lost? board)
  (cond
    [(null? board) #f]
    [else
     (or (row-lost? (car board))
         (board-lost? (cdr board)))]))

;; ¿Existe alguna celda segura (b=0) que NO esté revelada (c≠1)?
(define (row-has-safe-unrevealed? row)
  (cond
    [(null? row) #f]
    [else
     (define cell (car row))             ; '(b c a)
     (cond
       [(= (first cell) 1)               ; bomba -> no cuenta, seguir
        (row-has-safe-unrevealed? (cdr row))]
       [(= (second cell) 1)              ; segura y revelada -> seguir
        (row-has-safe-unrevealed? (cdr row))]
       [else #t])]))                     ; segura y NO revelada

(define (board-has-safe-unrevealed? board)
  (cond
    [(null? board) #f]
    [else
     (or (row-has-safe-unrevealed? (car board))
         (board-has-safe-unrevealed? (cdr board)))]))

;; game-status : Board -> 'playing | 'lost | 'won
(define (game-status board)
  (cond
    [(board-lost? board) 'lost]
    [(board-has-safe-unrevealed? board) 'playing]
    [else 'won]))

;; toggle-flag 
(define (toggle-flag board r c)
  (define-values (rows cols) (board-dimensions board))
  (cond
    [(in-bounds? rows cols r c)
     (define cell (get-cell board r c))  ; '(b c a)
     (define k (second cell))
     (cond
       [(= k 1) board]                   ; revelada: ignora
       [(= k 0) (set-click board r c 2)] ; poner bandera
       [(= k 2) (set-click board r c 0)] ; quitar bandera
       [else board])]
    [else board]))


(provide difficulty->ratio
         make-empty-board
         init-bombs/list
         crear-tablero-inicial
         descubrir marcar actualizarEstado
         game-status toggle-flag)

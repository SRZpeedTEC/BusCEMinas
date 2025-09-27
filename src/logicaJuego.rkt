#lang racket
#| LOGICA DEL JUEGO |#

;; ---------------------------
;; Dificultad -> porcentaje
;; ---------------------------
(define (dificultad->ratio dif)
  (cond [(or (eq? dif 'facil)   (and (string? dif) (string-ci=? dif "facil")))   0.10]
        [(or (eq? dif 'medio)   (and (string? dif) (string-ci=? dif "medio")))   0.15]
        [(or (eq? dif 'dificil) (and (string? dif) (string-ci=? dif "dificil"))) 0.20]
        [else (error 'dificultad->ratio (format "Dificultad desconocida: ~a" dif))]))

;; ---------------------------
;; Utilidades tablero (listas)
;; ---------------------------
(define (dimensionesMatriz matriz)
  (values (length matriz)
          (cond [(null? matriz) 0]
                [else (length (car matriz))])))

;; filas de '(0 0 0), recursivo
(define (crearFilas cols)
  (cond [(= cols 0) '()]
        [else (cons (list 0 0 0) (crearFilas (sub1 cols)))]))

(define (crearMatrizVacia rows cols)
  (cond [(= rows 0) '()]
        [else (cons (crearFilas cols)
                    (crearMatrizVacia (sub1 rows) cols))]))

;; ---------------------------
;; Posiciones y cantidad
;; ---------------------------
;; genera ((0 . 0) (0 . 1) ... (r . c)) sin for
(define (all-positions rows cols)
  (define (row-positions r c)
    (cond [(= c cols) '()]
          [else (cons (cons r c)
                      (row-positions r (add1 c)))]))
  (define (rows-loop r)
    (cond [(= r rows) '()]
          [else (append (row-positions r 0)
                        (rows-loop (add1 r)))]))
  (rows-loop 0))

(define (num-bombs rows cols ratio)
  (define total (* rows cols))
  (define n (inexact->exact (floor (* ratio total))))
  (cond [(<= total 1) 0]
        [else (max 1 (min (- total 1) n))]))

;; ---------------------------
;; Helpers de listas básicas
;; ---------------------------
;; reemplaza el elemento n-ésimo por val (versión inmutable)
(define (replace-nth lst n val)
  (cond [(null? lst) '()]
        [(= n 0) (cons val (cdr lst))]
        [else (cons (car lst)
                    (replace-nth (cdr lst) (sub1 n) val))]))

;; elimina el elemento n-ésimo
(define (remove-nth lst n)
  (cond [(null? lst) '()]
        [(= n 0) (cdr lst)]
        [else (cons (car lst)
                    (remove-nth (cdr lst) (sub1 n)))]))

;; ---------------------------
;; Selección aleatoria sin shuffle/take (PRNG global de Racket)
;; ---------------------------
;; elige k elementos únicos de una lista, extrayéndolos por índice y removiendo
(define (pick-k-from lst k)
  (cond [(or (= k 0) (null? lst)) '()]
        [else
         (define idx (random (length lst)))
         (define x   (list-ref lst idx))
         (cons x (pick-k-from (remove-nth lst idx) (sub1 k)))]))

(define (pick-positions rows cols k)
  (pick-k-from (all-positions rows cols) k))

;; ---------------------------
;; Helpers de tablero (puros)
;; ---------------------------
(define (get-cell board r c)
  (list-ref (list-ref board r) c))

(define (set-cell board r c new)
  (define row     (list-ref board r))
  (define new-row (replace-nth row c new))
  (replace-nth board r new-row))

(define (set-click board r c val)
  (define cell (get-cell board r c)) ; '(b c a)
  (set-cell board r c (list (car cell) val (caddr cell))))

;; ---------------------------
;; Colocar bombas (sin sets)
;; ---------------------------
(define (pos-in-list? rc ps)
  (cond [(null? ps) #f]
        [(equal? (car ps) rc) #t]
        [else (pos-in-list? rc (cdr ps))]))

(define (place-bombs/list board bomb-positions)
  (define (map-row row r c)
    (cond [(null? row) '()]
          [else
           (define cell (car row))
           (define new-cell
             (cond [(pos-in-list? (cons r c) bomb-positions) (list 1 0 0)]
                   [else cell]))
           (cons new-cell (map-row (cdr row) r (add1 c)))]))
  (define (map-board b r)
    (cond [(null? b) '()]
          [else (cons (map-row (car b) r 0)
                      (map-board (cdr b) (add1 r)))]))
  (map-board board 0))

;; ---------------------------
;; Vecinos y adyacentes
;; ---------------------------
(define neighbors-deltas
  '((-1 -1) (-1 0) (-1 1)
    ( 0 -1)         ( 0 1)
    ( 1 -1) ( 1 0)  ( 1 1)))


(define (in-bounds? rows cols r c)
  (and (in-range? r 0 rows) (in-range? c 0 cols)))

;; suma adyacentes con recursión (sin for/sum)
(define (adjacent-bombs board r c)
  (define-values (rows cols) (dimensionesMatriz board))
  (define (loop ds)
    (cond [(null? ds) 0]
          [else
           (define d  (car ds))
           (define rr (+ r (car d)))
           (define cc (+ c (cadr d)))
           (define here
             (cond [(and (in-bounds? rows cols rr cc)
                         (= (car (get-cell board rr cc)) 1))
                    1]
                   [else 0]))
           (+ here (loop (cdr ds)))]))
  (loop neighbors-deltas))

;; recalcula el 3er campo (ady) para todo el tablero, recursivo
(define (rellenar-adyacentes board)
  (define-values (rows cols) (dimensionesMatriz board))
  (define (row-loop r c acc-row)
    (cond [(= c cols) (reverse acc-row)]
          [else
           (define cell (get-cell board r c)) ; '(b k a)
           (define b (car cell))
           (define k (cadr cell))
           (define a (cond [(= b 1) 0]
                           [else (adjacent-bombs board r c)]))
           (row-loop r (add1 c) (cons (list b k a) acc-row))]))
  (define (board-loop r acc-board)
    (cond [(= r rows) (reverse acc-board)]
          [else
           (board-loop (add1 r)
                       (cons (row-loop r 0 '()) acc-board))]))
  (board-loop 0 '()))

;; ---------------------------
;; Pipeline inicial (API)
;; ---------------------------
(define (init-bombs/list board dificultad)
  (define-values (rows cols) (dimensionesMatriz board))
  (define ratio (dificultad->ratio dificultad))
  (define k     (num-bombs rows cols ratio))
  (define spots (pick-positions rows cols k))
  (values (place-bombs/list board spots) spots))

(define (crear-tablero-inicial dificultad rows cols)
  (define empty (crearMatrizVacia rows cols))
  (define-values (with-bombs _spots) (init-bombs/list empty dificultad))
  (rellenar-adyacentes with-bombs))



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
;; ----------------------------------------
;; Helpers de posiciones (listas puras)
;; ----------------------------------------
(define (pos-eq? p q)
  (and (= (car p) (car q)) (= (cdr p) (cdr q))))

(define (pos-member? p ps)
  (cond [(null? ps) #f]
        [(pos-eq? p (car ps)) #t]
        [else (pos-member? p (cdr ps))]))

;; ----------------------------------------
;; Revelar usando actualizarEstado (puro)
;; ----------------------------------------
(define (revelar board r c)
  ;; pone estado = 1 en (r,c) usando tu primitiva inmutable
  (actualizarEstado board r c 1))

;; ----------------------------------------
;; Descubrir (puro), usando actualizarEstado
;;  - no actúa si clk=1 (revelada) o clk=2 (marcada)
;;  - si hay bomba, revela solo esa
;;  - si ady>0, revela solo esa
;;  - si ady=0, expande (flood-fill) vecinos seguros,
;;    revelando ceros y bordes numéricos.
;; 100% recursivo, sin for/while ni sets.
;; ----------------------------------------
(define (descubrir board r0 c0)
  (define cell0 (get-cell board r0 c0))
  (define clk0  (second cell0))
  (cond
    [(= clk0 1) board]   ; ya revelada → no hacer nada
    [(= clk0 2) board]   ; marcada → no expandir ni revelar
    [else
     (define b0 (first cell0))
     (define a0 (third cell0))
     (cond
       [(= b0 1) (revelar board r0 c0)] ; bomba: revelar solo esa
       [(> a0 0) (revelar board r0 c0)] ; número: revelar solo esa
       [else
        ;; a0 = 0 → expansión (cola y visitados como listas)
        (define (process-deltas ds r c Bacc Vacc enq)
          (cond
            [(null? ds) (list Bacc Vacc enq)]
            [else
             (define d  (car ds))
             (define rr (+ r (car d)))
             (define cc (+ c (cadr d)))
             (define step
               (cond
                 [(not (in-bounds? (car (call-with-values (lambda () (dimensionesMatriz Bacc)) list))
                                   (cadr (call-with-values (lambda () (dimensionesMatriz Bacc)) list))
                                   rr cc))
                  (list Bacc Vacc enq)]
                 [else
                  (define cellN (get-cell Bacc rr cc))
                  (define b (first  cellN))
                  (define k (second cellN))
                  (define a (third  cellN))
                  (cond
                    [(= b 1) (list Bacc Vacc enq)]
                    [(or (= k 1) (pos-member? (cons rr cc) Vacc))
                     (list Bacc Vacc enq)]
                    [else
                     (define B2 (actualizarEstado Bacc rr cc 1)) ; revelar vecino
                     (define V2 (cons (cons rr cc) Vacc))
                     (cond
                       [(= a 0) (list B2 V2 (cons (cons rr cc) enq))] ; encola ceros
                       [else    (list B2 V2 enq)])])]))
             (process-deltas (cdr ds)
                             r c
                             (car  step)
                             (cadr step)
                             (caddr step))]))

        (define (loop queue visited B)
          (cond
            [(null? queue) B]
            [else
             (define r (car  (car queue)))
             (define c (cdr  (car queue)))
             (define triple (process-deltas neighbors-deltas r c B visited '()))
             (loop (append (cdr queue) (reverse (caddr triple)))
                   (cadr triple)
                   (car  triple))]))

        (loop (list (cons r0 c0))
              (list (cons r0 c0))
              (revelar board r0 c0))])]))


;; Se presiono click derecho, llamamos a marcar
(define (marcar matrizActual filaSel colSel)
  (define cell (get-cell matrizActual filaSel colSel))
  (cond
    [(= (second cell) 1) matrizActual] ; ya revelada → no marcar
    [else (actualizarEstado matrizActual filaSel colSel 2)]))

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
  (define-values (rows cols) (dimensionesMatriz board))
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


(provide dificultad->ratio
         crearMatrizVacia
         init-bombs/list
         crear-tablero-inicial
         descubrir marcar actualizarEstado
         game-status toggle-flag)

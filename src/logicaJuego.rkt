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

(define (crearMatrizVacia filas cols)
  (cond [(= filas 0) '()]
        [else (cons (crearFilas cols)
                    (crearMatrizVacia (sub1 filas) cols))]))

;; ---------------------------
;; Posiciones y cantidad
;; ---------------------------
;; genera ((0 . 0) (0 . 1) ... (fila . c)) sin for
(define (posiciones filas cols)
  (define (posicionesFilas fila col)
    (cond [(= col cols) '()]
          [else (cons (cons fila col)
                      (posicionesFilas fila (add1 col)))]))
  (define (filas-loop fila)
    (cond [(= fila filas) '()]
          [else (append (posicionesFilas fila 0)
                        (filas-loop (add1 fila)))]))
  (filas-loop 0))

(define (cantidadBombas filas cols ratio)
  (define total (* filas cols))
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

(define (agarrarPosicion filas cols k)
  (pick-k-from (posiciones filas cols) k))

;; ---------------------------
;; Helpers de tablero (puros)
;; ---------------------------
(define (obtenerCelda matriz fila col)
  (list-ref (list-ref matriz fila) col))

(define (setCelda matriz fila col new)
  (define row     (list-ref matriz fila))
  (define new-row (replace-nth row col new))
  (replace-nth matriz fila new-row))

(define (setClick matriz fila col val)
  (define celda (obtenerCelda matriz fila col)) ; '(b c a)
  (setCelda matriz fila col (list (car celda) val (caddr celda))))

;; ---------------------------
;; Colocar bombas (sin sets)
;; ---------------------------
(define (posicionEnMatriz? rc pos)
  (cond [(null? pos) #f]
        [(equal? (car pos) rc) #t]
        [else (posicionEnMatriz? rc (cdr pos))]))

(define (colocarBomba matriz posicionBomba)
  (define (mapearFila row fila c)
    (cond [(null? row) '()]
          [else
           (define celda (car row))
           (define newCelda
             (cond [(posicionEnMatriz? (cons fila c) posicionBomba) (list 1 0 0)]
                   [else celda]))
           (cons newCelda (mapearFila (cdr row) fila (add1 c)))]))
  (define (mapearMatriz b fila)
    (cond [(null? b) '()]
          [else (cons (mapearFila (car b) fila 0)
                      (mapearMatriz (cdr b) (add1 fila)))]))
  (mapearMatriz matriz 0))

;; ---------------------------
;; Vecinos y adyacentes
;; ---------------------------
(define vecinos
  '((-1 -1) (-1 0) (-1 1)
    ( 0 -1)         ( 0 1)
    ( 1 -1) ( 1 0)  ( 1 1)))


(define (in-bounds? filas cols fila c)
  (and (dentroRango fila 0 filas) (dentroRango c 0 cols)))

;; suma adyacentes con recursión (sin for/sum)
(define (adyacenciaBombas matriz fila c)
  (define-values (filas cols) (dimensionesMatriz matriz))
  (define (loop ds)
    (cond [(null? ds) 0]
          [else
           (define d  (car ds))
           (define rr (+ fila (car d)))
           (define cc (+ c (cadr d)))
           (define here
             (cond [(and (in-bounds? filas cols rr cc)
                         (= (car (obtenerCelda matriz rr cc)) 1))
                    1]
                   [else 0]))
           (+ here (loop (cdr ds)))]))
  (loop vecinos))

;; recalcula el 3er campo (ady) para todo el tablero, recursivo
(define (rellenar-adyacentes matriz)
  (define-values (filas cols) (dimensionesMatriz matriz))
  (define (loopFila fila c acc-row)
    (cond [(= c cols) (reverse acc-row)]
          [else
           (define celda (obtenerCelda matriz fila c)) ; '(b k a)
           (define b (car celda))
           (define k (cadr celda))
           (define a (cond [(= b 1) 0]
                           [else (adyacenciaBombas matriz fila c)]))
           (loopFila fila (add1 c) (cons (list b k a) acc-row))]))
  (define (loopMatriz fila acc-matriz)
    (cond [(= fila filas) (reverse acc-matriz)]
          [else
           (loopMatriz (add1 fila)
                       (cons (loopFila fila 0 '()) acc-matriz))]))
  (loopMatriz 0 '()))

;; ---------------------------
;; Pipeline inicial (API)
;; ---------------------------
(define (inicializarBombas matriz dificultad)
  (define-values (filas cols) (dimensionesMatriz matriz))
  (define ratio (dificultad->ratio dificultad))
  (define k     (cantidadBombas filas cols ratio))
  (define spots (agarrarPosicion filas cols k))
  (values (colocarBomba matriz spots) spots))

(define (crear-tablero-inicial dificultad filas cols)
  (define empty (crearMatrizVacia filas cols))
  (define-values (with-bombs _spots) (inicializarBombas empty dificultad))
  (rellenar-adyacentes with-bombs))



;; Creamos Matriz (((BOMBA?, ESTADO, ADYACENTES) , (BOMBA?, ESTADO, ADYACENTES)))


;; dentroRango : n min max  -> #t si min <= n < max
(define (dentroRango n minimo maximo)
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
  
  (if (and (dentroRango filaSel 0 filas) (dentroRango colSel 0 columnas))
      (actualizarMatriz matrizActual 0)
      matrizActual))


;; Se presiono click izquierdo, llamamos a descubrir
;; ----------------------------------------
;; Helpers de posiciones (listas puras)
;; ----------------------------------------
(define (pos-eq? p q)
  (and (= (car p) (car q)) (= (cdr p) (cdr q))))

(define (pos-member? p pos)
  (cond [(null? pos) #f]
        [(pos-eq? p (car pos)) #t]
        [else (pos-member? p (cdr pos))]))

;; ----------------------------------------
;; Revelar usando actualizarEstado (puro)
;; ----------------------------------------
(define (revelar matriz fila c)
  ;; pone estado = 1 en (fila,c) usando tu primitiva inmutable
  (actualizarEstado matriz fila c 1))

;; ----------------------------------------
;; Descubrir (puro), usando actualizarEstado
;;  - no actúa si clk=1 (revelada) o clk=2 (marcada)
;;  - si hay bomba, revela solo esa
;;  - si ady>0, revela solo esa
;;  - si ady=0, expande (flood-fill) vecinos seguros,
;;    revelando ceros y bordes numéricos.
;; 100% recursivo, sin for/while ni sets.
;; ----------------------------------------
(define (descubrir matriz r0 c0)
  (define celda0 (obtenerCelda matriz r0 c0))
  (define clk0  (second celda0))
  (cond
    [(= clk0 1) matriz]   ; ya revelada → no hacer nada
    [(= clk0 2) matriz]   ; marcada → no expandir ni revelar
    [else
     (define b0 (first celda0))
     (define a0 (third celda0))
     (cond
       [(= b0 1) (revelar matriz r0 c0)] ; bomba: revelar solo esa
       [(> a0 0) (revelar matriz r0 c0)] ; número: revelar solo esa
       [else
        ;; a0 = 0 → expansión (cola y visitados como listas)
        (define (expansionBFS ds fila c Bacc Vacc enq)
          (cond
            [(null? ds) (list Bacc Vacc enq)]
            [else
             (define d  (car ds))
             (define rr (+ fila (car d)))
             (define cc (+ c (cadr d)))
             (define step
               (cond
                 [(not (in-bounds? (car (call-with-values (lambda () (dimensionesMatriz Bacc)) list))
                                   (cadr (call-with-values (lambda () (dimensionesMatriz Bacc)) list))
                                   rr cc))
                  (list Bacc Vacc enq)]
                 [else
                  (define celdaN (obtenerCelda Bacc rr cc))
                  (define b (first  celdaN))
                  (define k (second celdaN))
                  (define a (third  celdaN))
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
             (expansionBFS (cdr ds)
                             fila c
                             (car  step)
                             (cadr step)
                             (caddr step))]))

        (define (loop queue visited B)
          (cond
            [(null? queue) B]
            [else
             (define fila (car  (car queue)))
             (define c (cdr  (car queue)))
             (define triple (expansionBFS vecinos fila c B visited '()))
             (loop (append (cdr queue) (reverse (caddr triple)))
                   (cadr triple)
                   (car  triple))]))

        (loop (list (cons r0 c0))
              (list (cons r0 c0))
              (revelar matriz r0 c0))])]))


;; Se presiono click derecho, llamamos a marcar
(define (marcar matrizActual filaSel colSel)
  (define celda (obtenerCelda matrizActual filaSel colSel))
  (cond
    [(= (second celda) 1) matrizActual] ; ya revelada → no marcar
    [else (actualizarEstado matrizActual filaSel colSel 2)]))

;; Inspeccionar el tablero

(define (filaPerdida fila)
  (cond
    [(null? fila) #f]
    [else
     (define celda (car fila))       ; '(b c a)
     (cond
       [(and (= (first celda) 1)    ; bomba
             (= (second celda) 1))  ; revelada
        #t]
       [else (filaPerdida (cdr fila))])]))

(define (matrizPerdida matriz)
  (cond
    [(null? matriz) #f]
    [else
     (or (filaPerdida (car matriz))
         (matrizPerdida (cdr matriz)))]))

;; ¿Existe alguna celda segura (b=0) que NO esté revelada (c≠1)?
(define (filaRevelada fila)
  (cond
    [(null? fila) #f]
    [else
     (define celda (car fila))             ; '(b c a)
     (cond
       [(= (first celda) 1)               ; bomba -> no cuenta, seguir
        (filaRevelada (cdr fila))]
       [(= (second celda) 1)              ; segura y revelada -> seguir
        (filaRevelada (cdr fila))]
       [else #t])]))                     ; segura y NO revelada

(define (matrizRevelada matriz)
  (cond
    [(null? matriz) #f]
    [else
     (or (filaRevelada (car matriz))
         (matrizRevelada (cdr matriz)))]))

;; gameStatus : matriz -> 'playing | 'lost | 'won
(define (gameStatus matriz)
  (cond
    [(matrizPerdida matriz) 'lost]
    [(matrizRevelada matriz) 'playing]
    [else 'won]))

;; marcarBandera 
(define (marcarBandera matriz fila c)
  (define-values (filas cols) (dimensionesMatriz matriz))
  (cond
    [(in-bounds? filas cols fila c)
     (define celda (obtenerCelda matriz fila c))  ; '(b c a)
     (define k (second celda))
     (cond
       [(= k 1) matriz]                   ; revelada: ignora
       [(= k 0) (setClick matriz fila c 2)] ; poner bandera
       [(= k 2) (setClick matriz fila c 0)] ; quitar bandera
       [else matriz])]
    [else matriz]))


(provide dificultad->ratio
         crearMatrizVacia
         inicializarBombas
         crear-tablero-inicial
         descubrir marcar actualizarEstado
         gameStatus marcarBandera)

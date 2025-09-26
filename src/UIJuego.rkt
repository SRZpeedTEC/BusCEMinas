#lang racket
(require racket/gui
         "logicaJuego.rkt") ; Backend
(require racket/list)

;; FUENTES

(define big-font(make-object font% 48 'roman 'normal 'bold))  ; tamaño 48, negrita

(define txt-font (make-object font% 20 'roman 'normal 'bold))


;; CONSTANTES

(define window-size 800)


;; Creamos nuestra ventana de menu

(define Menu 
  (new frame% [label "Menu - BusCEMinas"]
       [width window-size]
       [height window-size]))


;; Panel principal para acomodar los objetos

(define menuPanel 
  (new vertical-panel% 
       [parent Menu]
       [alignment '(center center)]   ; centra contenido horizontal y vertical
       [stretchable-width #f]
       [stretchable-height #f]))


;; Titulo
(define menuLabel 
  (new message% 
       [parent menuPanel]
       [label "BusCEMinas"]
       [font big-font]
       [color "dark green"]
       [vert-margin 50]))


;; Text Field para columnas y filas de la matriz
(define inputs (new horizontal-panel% [parent menuPanel] [alignment '(center center)]  [vert-margin 50]))
(define tf-fil (new text-field% [parent inputs] [label "Filas: "]  [init-value "10"] [font txt-font] [horiz-margin 50] [min-width 80]))
(define tf-col (new text-field% [parent inputs] [label "Columnas: "] [init-value "10"] [font txt-font]  [min-width 80]))


;; Obtenemos los valores de los Text Field
(define (leer-textFields textField)
  (define content (send textField get-value))
  (define num (string->number content))
  (cond
    [(and num (exact-integer? num) (> num 8) (< num 15)) num]
    [else
     (message-box "Dato inválido"
                  (format "Se debe ingresar un entero valido entre 8 y 15")
                  Menu '(ok))
     #f]))


;; Botones dificultades

(define botonFacil 
  (new button%
     [parent menuPanel]
     [label "Fácil"]
     [font big-font]
     [vert-margin 50]
     [callback (λ (_ e) (iniciar 'facil))]))

(define botonMedio 
  (new button%
     [parent menuPanel]
     [label "Medio"]
     [font big-font]
     [vert-margin 50]
     [callback (λ (_ e) (iniciar 'medio))]))

(define botonDificil 
  (new button%
     [parent menuPanel]
     [label "Difícil"]
     [font big-font]
     [vert-margin 50]
     [callback (λ (_ e) (iniciar 'dificil))]))


;; Funcion al pulsar un boton

(define (iniciar nivel)
  (define filas  (leer-textFields tf-fil))
  (define colums (leer-textFields tf-col))
  (when (and filas colums)
    (define dificultad nivel) 
    (define tablero (crear-tablero-inicial dificultad filas colums))

    (abrir-ventana-juego tablero)
    (send Menu show #f)))


(send Menu show #t)

;; abrir-ventana-juego : Board -> Frame
(define (abrir-ventana-juego tablero0)
  (define filas  (length tablero0))
  (define cols   (length (first tablero0)))
  (define CELL   32)
  (define WBOARD (* cols CELL))
  (define HBOARD (* filas CELL))

  ;; Colores clásicos para números 1..8
  (define num-colors
    (vector "blue" "green" "red" "navy" "maroon" "teal" "black" "gray"))

  ;; === Helpers UI-local (puras respecto al tablero) ===
  (define (flags-count board)
    (define (row-count row)
      (cond [(null? row) 0]
            [else
             (define k (second (car row)))
             (+ (if (= k 2) 1 0) (row-count (cdr row)))]))
    (cond [(null? board) 0]
          [else (+ (row-count (car board)) (flags-count (cdr board)))]))

  ;; Dibuja una celda (x,y) según su triple '(b c a) y estado del juego
  (define (draw-cell dc x y triple estado)
    (define b (first triple))   ; 0/1 (mina)
    (define k (second triple))  ; 0 oculto, 1 revelado, 2 bandera
    (define a (third triple))   ; adyacentes
    (define x0 (* x CELL))
    (define y0 (* y CELL))

    (define (tile-raised)
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "gainsboro" 'solid)
      (send dc draw-rectangle x0 y0 CELL CELL)
      (send dc set-pen "white" 2 'solid)
      (send dc draw-line x0 y0 (+ x0 CELL) y0)
      (send dc draw-line x0 y0 x0 (+ y0 CELL))
      (send dc set-pen "gray" 2 'solid)
      (send dc draw-line (+ x0 CELL -1) (+ y0 1) (+ x0 CELL -1) (+ y0 CELL -1))
      (send dc draw-line (+ x0 1) (+ y0 CELL -1) (+ x0 CELL -1) (+ y0 CELL -1)))

    (define (tile-flat)
      (send dc set-pen "darkgray" 1 'solid)
      (send dc set-brush "silver" 'solid)
      (send dc draw-rectangle x0 y0 CELL CELL))

    (define (draw-flag)
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "gainsboro" 'solid)
      (send dc draw-rectangle x0 y0 CELL CELL)
      (send dc set-pen "black" 2 'solid)
      (send dc draw-line (+ x0 8) (+ y0 4) (+ x0 8) (+ y0 26))
      (send dc set-brush "red" 'solid)
      (send dc draw-polygon (list (cons (+ x0 9) (+ y0 5))
                                  (cons (+ x0 24) (+ y0 10))
                                  (cons (+ x0 9) (+ y0 15)))))

    (define (draw-mine exploded?)
      (tile-flat)
      (send dc set-pen (if exploded? "red" "black") 2 'solid)
      (send dc set-brush (if exploded? "red" "black") 'solid)
      (send dc draw-ellipse (+ x0 8) (+ y0 8) 16 16)
      (for ([ang '(0 45 90 135 180 225 270 315)])
        (define rad (* 3.14159 (/ ang 180.0)))
        (define cx (+ x0 16))
        (define cy (+ y0 16))
        (define dx (inexact->exact (round (* 14 (cos rad)))))
        (define dy (inexact->exact (round (* 14 (sin rad)))))
        (send dc draw-line cx cy (+ cx dx) (+ cy dy))))

    (cond
      [(and (eq? estado 'lost) (= b 1)) (draw-mine (= k 1))]
      [(= k 1)
       (tile-flat)
       (when (> a 0)
         (define idx (- a 1))
         (send dc set-text-foreground (vector-ref num-colors idx))
         (send dc set-font (make-object font% 16 'modern 'normal 'bold))
         (send dc draw-text (number->string a) (+ x0 10) (+ y0 6)))]
      [(= k 2) (draw-flag)]
      [else (tile-raised)]))

  ;; === Ventana y paneles ===
  (define frame
    (new frame%
         [label "Juego - BusCEMinas"]
         [width (+ WBOARD 200)]
         [height (max HBOARD 200)]))

  (define main-panel (new horizontal-panel% [parent frame]))

  ;; Canvas del tablero
  (define board-canvas
    (new
     (class canvas%
       (super-new [parent main-panel]
                  [min-width WBOARD]
                  [min-height HBOARD])

       ;; estado UI local: campos internos + getters públicos
       (field [tablero-actual tablero0])
       (field [estado-actual  (game-status tablero0)])

       (define/public (tablero) tablero-actual)
       (define/public (estado)  estado-actual)

       (define/public (reset! new-board)
         (set! tablero-actual new-board)
         (set! estado-actual (game-status tablero-actual))
         (send this refresh)
         (send lbl-status set-label "En juego"))

       (define/override (on-paint)
         (define dc (send this get-dc))
         (send dc set-brush "black" 'transparent)
         (send dc set-pen "black" 1 'solid)
         (send dc draw-rectangle 0 0 WBOARD HBOARD)
         (for* ([r (in-range filas)] [c (in-range cols)])
           (define triple (list-ref (list-ref tablero-actual r) c))
           (draw-cell dc c r triple estado-actual))
         (send dc set-pen "gray" 1 'solid)
         (for ([x (in-range 0 (+ WBOARD 1) CELL)])
           (send dc draw-line x 0 x HBOARD))
         (for ([y (in-range 0 (+ HBOARD 1) CELL)])
           (send dc draw-line 0 y WBOARD y)))

       (define (aplicar-jugada! nuevo)
         (when (not (equal? nuevo tablero-actual))
           (set! tablero-actual nuevo)
           (set! estado-actual (game-status tablero-actual))
           (cond
             [(eq? estado-actual 'lost)
              (send lbl-status set-label "¡Boom! Perdiste")]
             [(eq? estado-actual 'won)
              (send lbl-status set-label "¡Ganaste!")]
             [else
              (send lbl-status set-label "En juego")])
           (send lbl-flags set-label
                 (format "Banderas: ~a" (flags-count tablero-actual)))
           (send this refresh)))

       (define/override (on-event e)
         (define t (send e get-event-type))
         (when (and (eq? estado-actual 'playing)
                    (member t '(left-down left-up right-down right-up)))
           (define mx (send e get-x))
           (define my (send e get-y))
           (when (and (<= 0 mx) (< mx WBOARD) (<= 0 my) (< my HBOARD))
             (define c (quotient mx CELL))
             (define r (quotient my CELL))
             (cond
               [(member t '(left-down left-up))
                (aplicar-jugada! (descubrir tablero-actual r c))]
               [(member t '(right-down right-up))
                (aplicar-jugada! (toggle-flag tablero-actual r c))])))))))

  ;; HUD a la derecha (¡esto faltaba!)
  (define hud (new vertical-panel% [parent main-panel]
                                   [alignment '(center top)]
                                   [min-width 200]
                                   [stretchable-width #f]))

  (new message% [parent hud] [label "HUD"])
  (define lbl-status (new message% [parent hud] [label "En juego"]))
  (define lbl-flags  (new message% [parent hud]
                          [label (format "Banderas: ~a"
                                         (flags-count (send board-canvas tablero)))]))

  (new button%
       [parent hud]
       [label "Nueva partida"]
       [callback
        (λ (_btn _evt)
          (send frame show #f)
          (send Menu show #t))])

  (send frame show #t)
  frame)
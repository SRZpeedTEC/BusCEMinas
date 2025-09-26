#lang racket
(require racket/gui
         "logicaJuego.rkt")
(require racket/list)

;;FUENTES
(define big-font   (make-object font% 56 'roman 'normal 'bold))   ; título grande
(define title-font (make-object font% 24 'modern 'normal 'bold))  ; subtítulos/labels
(define txt-font   (make-object font% 14 'modern 'normal 'normal))

;;CONSTANTES
(define WINDOW-W 520)
(define WINDOW-H 360)

;;FRAME MENÚ
(define Menu
  (new frame%
       [label "Menú - BusCEMinas"]
       [width WINDOW-W]
       [height WINDOW-H]))

;; Panel raíz
(define menuPanel
  (new vertical-panel%
       [parent Menu]
       [alignment '(center center)]
       [stretchable-width #t]
       [stretchable-height #t]
       [spacing 6]))

;;HEADER: logo + título
(define header
  (new horizontal-panel%
       [parent menuPanel]
       [alignment '(center center)]
       [stretchable-width #f]
       [spacing 10]))

;; Logo
(define logo-canvas
  (new
   (class canvas%
     (super-new [parent header] [min-width 100] [min-height 80])
     (define/override (on-paint)
       (define dc (send this get-dc))
       
       (send dc set-brush (make-object color% 240 240 240) 'solid)
       (send dc set-pen "transparent" 0 'transparent)
       (send dc draw-rectangle 0 0 (send this get-width) (send this get-height))

       ;; Función de dibujo de mina
       (define (draw-mine cx cy r exploded?)
         (send dc set-brush (if exploded? "red" "black") 'solid)
         (send dc set-pen   (if exploded? "red" "black") 2 'solid)
         (send dc draw-ellipse (- cx r) (- cy r) (* 2 r) (* 2 r))
         (for ([ang '(0 45 90 135 180 225 270 315)])
           (define rad (* pi (/ ang 180.0)))
           (send dc draw-line
                 cx cy
                 (+ cx (inexact->exact (round (* (+ r 8) (cos rad)))))
                 (+ cy (inexact->exact (round (* (+ r 8) (sin rad))))))))
       ;; dos minas estilo clásico
       (draw-mine 30 45 16 #f)
       (draw-mine 70 30 14 #f)))))

;; Título 
(define menuLabel
  (new message%
       [parent header]
       [label "BusCEMinas"]
       [font (make-object font% 32 'swiss 'italic 'bold)] ; más grande y estilizado
       [auto-resize #t]
       [vert-margin 8]
       [horiz-margin 8]
       [color "dark green"]))

;; Separador fino (usar canvas%, no panel%)
(define (hr parent)
  (new canvas%
       [parent parent]
       [min-width (- WINDOW-W 60)]
       [min-height 1]
       [stretchable-width #f]
       [stretchable-height #f]
       [paint-callback
        (λ (c dc)
          (send dc set-pen "gray" 1 'solid)
          (define w (send c get-width))
          (send dc draw-line 0 0 w 0))]))

(hr menuPanel)

;; ===== BLOQUE CONFIG (filas/columnas) =====
(define config-panel
  (new vertical-panel%
       [parent menuPanel]
       [alignment '(center center)]
       [stretchable-width #f]
       [spacing 4]
       [vert-margin 8]))

(new message% [parent config-panel] [label "Tamaño del tablero"]
     [font title-font] [auto-resize #t])

(define inputs
  (new horizontal-panel%
       [parent config-panel]
       [alignment '(center center)]
       [spacing 12]))

(define tf-fil
  (new text-field%
       [parent inputs]
       [label "Filas:"]
       [init-value "10"]
       [font txt-font]
       [min-width 60]))

(define tf-col
  (new text-field%
       [parent inputs]
       [label "Columnas:"]
       [init-value "10"]
       [font txt-font]
       [min-width 60]))

;; Validación
(define (leer-textFields textField)
  (define content (send textField get-value))
  (define num (string->number content))
  (cond
    [(and num (exact-integer? num) (<= 8 num) (<= num 15)) num]
    [else
     (message-box "Dato inválido"
                  "Se debe ingresar un entero válido entre 8 y 15"
                  Menu '(ok))
     #f]))

(hr menuPanel)

;; ===== Acción de inicio =====
(define (iniciar nivel)
  (define filas  (leer-textFields tf-fil))
  (define colums (leer-textFields tf-col))
  (when (and filas colums)
    (define dificultad nivel)
    (define tablero (crear-tablero-inicial dificultad filas colums))
    (abrir-ventana-juego tablero) ; definida en tu módulo/archivo
    (send Menu show #f)))

;;BLOQUE DIFICULTAD
(define diff-panel
  (new vertical-panel%
       [parent menuPanel]
       [alignment '(center center)]
       [stretchable-width #f]
       [spacing 6]
       [vert-margin 6]))

(new message% [parent diff-panel] [label "Dificultad"]
     [font title-font] [auto-resize #t])

(define botones
  (new horizontal-panel%
       [parent diff-panel]
       [alignment '(center center)]
       [spacing 10]))

(define botonFacil
  (new button%
       [parent botones]
       [label "Fácil"]
       [font (make-object font% 16 'modern 'normal 'bold)]
       [min-width 110]
       [callback (λ (_ e) (iniciar 'facil))]))

(define botonMedio
  (new button%
       [parent botones]
       [label "Medio"]
       [font (make-object font% 16 'modern 'normal 'bold)]
       [min-width 110]
       [callback (λ (_ e) (iniciar 'medio))]))

(define botonDificil
  (new button%
       [parent botones]
       [label "Difícil"]
       [font (make-object font% 16 'modern 'normal 'bold)]
       [min-width 110]
       [callback (λ (_ e) (iniciar 'dificil))]))

(hr menuPanel)

;; Pie de página
(new message%
     [parent menuPanel]
     [label "© Proyecto BusCEMinas"]
     [font (make-object font% 10 'modern 'normal 'normal)]
     [auto-resize #t]
     [color "dim gray"])

;; Mostrar centrado
(send Menu center)
(send Menu show #t)

;; abrir-ventana-juego : Board -> Frame
(define (abrir-ventana-juego tablero0)
  (define filas  (length tablero0))
  (define cols   (length (first tablero0)))
  (define CELL   32)
  (define WBOARD (* cols CELL))
  (define HBOARD (* filas CELL))

  (define num-colors
    (vector "blue" "green" "red" "navy" "maroon" "teal" "black" "gray"))

  ;; Helpers de lógica de UI
  (define (flags-count board)
    (define (row-count row)
      (cond [(null? row) 0]
            [else
             (define k (second (car row)))
             (+ (if (= k 2) 1 0) (row-count (cdr row)))]))
    (cond [(null? board) 0]
          [else (+ (row-count (car board)) (flags-count (cdr board)))]))

  (define (draw-cell dc x y triple estado)
    (define b (first triple))   ; mina 0/1
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

;; Ventana y paneles

;;evita que el HUD se desborde en 10x10=320px
(define compact? (< WBOARD 400))

(define frame
  (new frame%
       [label "Juego - BusCEMinas"]
       [width WBOARD]                     
       [style '(no-resize-border)]))


;; Contenedor raíz en columna: arriba HUD, abajo tablero
(define main-root
  (new vertical-panel%
       [parent frame]
       [stretchable-width #t]
       [stretchable-height #t]
       [spacing 4]))

;; Wrap NO estirable que centra el HUD y NO fuerza ancho del frame
(define hud-wrap
  (new horizontal-panel%
       [parent main-root]
       [alignment '(center center)]
       [stretchable-width #t]
       [stretchable-height #f]))

;; ---------- HUD (horizontal, centrado, no-estirable) ----------
(define hud
  (new horizontal-panel%
       [parent hud-wrap]
       [alignment '(center center)]
       [stretchable-width #f]            
       [stretchable-height #f]
       [spacing (if compact? 8 16)]))

(define hud-font (make-object font% (if compact? 12 14) 'modern 'normal 'bold))

(define lbl-status
  (new message%
       [parent hud]
       [label (if compact? "En juego" "En juego")]
       [auto-resize #f] [font hud-font]))

;; separador vertical fino
(when (not compact?)
  (new canvas% [parent hud] [min-width 1] [min-height 18]
       [paint-callback (λ (c dc) (send dc set-pen "gray" 1 'solid)
                                 (send dc draw-line 0 0 0 18))]))

(define lbl-timer
  (new message%
       [parent hud]
       [label (if compact? "T: 00:00" "Tiempo: 00:00")]
       [auto-resize #f] [font hud-font]))

(when (not compact?)
  (new canvas% [parent hud] [min-width 1] [min-height 18]
       [paint-callback (λ (c dc) (send dc set-pen "gray" 1 'solid)
                                 (send dc draw-line 0 0 0 18))]))

(define lbl-flags
  (new message%
       [parent hud]
       [label (if compact? "B: 0" "Banderas: 0")]
       [auto-resize #f] [font hud-font]))

;; Botón pequeño para que quepa en 320 px si es necesario
(new button%
     [parent hud]
     [label (if compact? "Nuevo" "Nueva partida")]
     [callback (λ (_btn _evt)
                 (stop-timer!)
                 (send frame show #f)
                 (send Menu show #t))])

;; ---------- Timer ----------
(define elapsed 0)
(define running? #f)

(define (fmt-mm:ss s)
  (define m  (quotient s 60))
  (define ss (remainder s 60))
  (format "~a ~a:~a"
          (if compact? "T:" "Tiempo:")
          (~a m  #:min-width 2 #:align 'right #:pad-string "0")
          (~a ss #:min-width 2 #:align 'right #:pad-string "0")))

(define timer
  (new timer%
       [notify-callback
        (λ ()
          (when running?
            (set! elapsed (add1 elapsed))
            (send lbl-timer set-label (fmt-mm:ss elapsed))))]))
(send timer start 1000)

(define (start-timer!) (set! running? #t))
(define (stop-timer!)  (set! running? #f))
(define (reset-timer!)
  (set! elapsed 0)
  (set! running? #f)
  (send lbl-timer set-label (fmt-mm:ss elapsed)))

;; ---------- Contenedor del tablero, centrado abajo ----------
(define board-row
  (new horizontal-panel%
       [parent main-root]
       [alignment '(center center)]
       [stretchable-width #t]
       [stretchable-height #t]))

(new horizontal-panel% [parent board-row] [stretchable-width #t])


(define board-canvas
  (new
   (class canvas%
     (super-new [parent board-row]
                [min-width WBOARD]
                [min-height HBOARD])

     (field [tablero-actual tablero0])
     (field [estado-actual  (game-status tablero0)])

     (define/public (tablero) tablero-actual)
     (define/public (estado)  estado-actual)

     (define/public (reset! new-board)
       (set! tablero-actual new-board)
       (set! estado-actual (game-status tablero-actual))
       (reset-timer!)
       (send this refresh)
       (send lbl-status set-label "En juego")
       (send lbl-flags set-label
             (format "~a ~a"
                     (if compact? "B:" "Banderas:")
                     (flags-count tablero-actual))))

     (define/override (on-paint)
       (define dc (send this get-dc))

       
       
       ;; pinta TODO el canvas del mismo color
       (send dc set-brush "black" 'solid)
       (send dc set-pen "black" 0 'transparent)
       (send dc draw-rectangle 0 0 (send this get-width) (send this get-height))

       ;; Rellena solo el área del tablero
       (send dc set-brush "black" 'solid)
       (send dc set-pen "black" 1 'solid)
       (send dc draw-rectangle 0 0 WBOARD HBOARD)

       ;; Dibuja celdas
       (for* ([r (in-range filas)] [c (in-range cols)])
         (define triple (list-ref (list-ref tablero-actual r) c))
         (draw-cell dc c r triple estado-actual))

       ;; Dibuja las líneas de la cuadrícula
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
            (stop-timer!)
            (send lbl-status set-label "¡Boom! Perdiste")]
           [(eq? estado-actual 'won)
            (stop-timer!)
            (send lbl-status set-label "¡Ganaste!")]
           [else
            (send lbl-status set-label "En juego")])
         (send lbl-flags set-label
               (format "~a ~a"
                       (if compact? "B:" "Banderas:")
                       (flags-count tablero-actual)))
         (send this refresh)))

     (define/override (on-event e)
       (define t (send e get-event-type))
       (when (and (eq? estado-actual 'playing)
                  (or (eq? t 'left-down) (eq? t 'right-down)))
         (unless running? (start-timer!))
         (define mx (send e get-x))
         (define my (send e get-y))
         (when (and (<= 0 mx) (< mx WBOARD) (<= 0 my) (< my HBOARD))
           (define c (quotient mx CELL))
           (define r (quotient my CELL))
           (cond
             [(eq? t 'left-down)
              (aplicar-jugada! (descubrir tablero-actual r c))]
             [(eq? t 'right-down)
              (aplicar-jugada! (toggle-flag tablero-actual r c))])))))))
  
(new horizontal-panel% [parent board-row] [stretchable-width #t])

;; Inicializa labels dependientes del tablero
(send lbl-flags set-label
      (format "~a ~a" (if compact? "B:" "Banderas:")
              (flags-count (send board-canvas tablero))))
(reset-timer!)

(send frame show #t)
frame)
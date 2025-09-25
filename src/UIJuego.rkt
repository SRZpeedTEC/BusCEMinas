#lang racket
(require racket/gui
         "logicaJuego.rkt") ; Backend

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
  (define cell   70)
  (define width  (+ 1 (* cols cell)))
  (define height (+ 1 (* filas cell)))

  (define frameJuego
    (new frame%
         [label "Juego - BusCEMinas"]
         [width (+ width 400)]))

  (define canvasJuego
    (new
     (class canvas%
       ;; Estado UI-local (no boxes): una variable capturada
       (init-field)
       (super-new [parent frameJuego]
                  [min-width width]
                  [min-height height])

       ;; Tablero actual visible (mutable SOLO en la UI)
       (define tablero tablero0)

       ;; Helper: mostrar el triple real
       (define (cell->string triple)
         (~a triple))

       ;; Dibujo
       (define/override (on-paint)
         (define dc (send this get-dc))
         (send dc set-brush "white" 'solid)
         (send dc set-pen "black" 1 'solid)
         (send dc draw-rectangle 0 0 width height)

         ;; Grid
         (for* ([fila (in-range filas)] [col (in-range cols)])
           (send dc draw-rectangle (* col cell) (* fila cell) cell cell))

         ;; Texto por celda (desde `tablero`)
         (send dc set-font (make-object font% 10 'modern 'normal 'normal))
         (send dc set-text-foreground "black")
         (for* ([fila (in-range filas)] [col (in-range cols)])
           (define x (* col cell))
           (define y (* fila cell))
           (define triple (list-ref (list-ref tablero fila) col))
           (send dc draw-text (cell->string triple) (+ x 6) (+ y 8))))

       ;; Eventos: derecho = descubrir (BFS), izquierdo = marcar
       (define/override (on-event e)
         (define tipo   (send e get-event-type))
         (define mousex (send e get-x))
         (define mousey (send e get-y))
         (when (and (<= 0 mousex) (< mousex width)
                    (<= 0 mousey) (< mousey height))
           (define colSel  (quotient mousex cell))
           (define filaSel (quotient mousey cell))

           ;; Calcula nuevo tablero usando LÓGICA PURA
           (define tablero-nuevo
             (cond [(or (eq? tipo 'left-down) (eq? tipo 'left-up) (eq? tipo 'menu))
                    (descubrir tablero filaSel colSel)] ; REVELAR (BFS)
                   [(or (eq? tipo 'right-down)  (eq? tipo 'right-up))
                    (marcar    tablero filaSel colSel)] ; MARCAR
                   [else tablero]))

           ;; Si cambió, actualiza la var local y repinta
           (unless (equal? tablero-nuevo tablero)
             (set! tablero tablero-nuevo)
             (send this refresh)))))))

  (send frameJuego show #t)
  frameJuego)





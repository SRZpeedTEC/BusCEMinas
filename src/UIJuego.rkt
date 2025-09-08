#lang racket
(require racket/gui
         "logicaJuego.rkt") ; tu lógica

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
       [label "Facil"]
       [font big-font]
       [vert-margin 50]
       [callback (λ (_ e) (iniciar 'facil))]))

(define botonMedio 
  (new button% 
       [parent menuPanel]
       [label "Medio"]
       [font big-font]
       [vert-margin 50]
       [callback (λ (_ e) (iniciar 'facil))]))

(define botonDificil 
  (new button% 
       [parent menuPanel]
       [label "Dificil"]
       [font big-font]
       [vert-margin 50]
       [callback (λ (_ e) (iniciar 'facil))]))


;; Funcion al pulsar un boton

(define (iniciar _nivel) ; Por ahora en todas se inicia por defecto
  
  (define filas (leer-textFields tf-fil))
  (define colums  (leer-textFields tf-col))
  
  (when (and filas colums)    
    (define matrizJuego (crear-matrizJuego filas colums))
    (abrir-ventana-juego matrizJuego)
    (send Menu show #f)
    
    ;; Comprobacion matriz ha sido creada
    (displayln (format "Matriz (~a x ~a):" filas colums))   
    ))


(send Menu show #t)


(define (abrir-ventana-juego matrizInicial)
  (define filas  (length matrizInicial))
  (define cols   (length (first matrizInicial)))
  (define cell   70)
  (define width  (+ 1 (* cols cell)))
  (define height (+ 1 (* filas cell)))

  ;; Estado en caja (reemplazamos por una nueva matriz en cada clic)
  (define matrizJuego (box matrizInicial))

  (define frameJuego
    (new frame%
         [label "Juego - BusCEMinas"]
         [width (+ width 400)]))

  (define canvasJuego
    (new
     (class canvas%
       (super-new [parent frameJuego]
                  [min-width width]
                  [min-height height])

       ;; Dibujo
       (define/override (on-paint)
         (define dc (send this get-dc))
         (send dc set-brush "white" 'solid)
         (send dc set-pen "black" 1 'solid)
         (send dc draw-rectangle 0 0 width height)

         ;; Grid
         (for* ([fila (in-range filas)] [col (in-range cols)])
           (send dc draw-rectangle (* col cell) (* fila cell) cell cell))

         ;; Mostrar valored de cada celda
         (send dc set-font (make-object font% 10 'modern 'normal 'normal))
         (send dc set-text-foreground "black")
         (define matrizActual (unbox matrizJuego))
         
         (for* ([fila (in-range filas)] [col (in-range cols)])
           (define x (* col cell))
           (define y (* fila cell))
           (define triple (list-ref (list-ref matrizActual fila) col))
           (send dc draw-text (~a triple) (+ x 6) (+ y 8))))

       ;; Clicks: izquierdo = 1 (revelado), derechos = 2 (marcado)
       (define/override (on-event e)
         (define evento (send e get-event-type))
         (define mouse_x (send e get-x))
         (define mouse_y (send e get-y))
         (when (and (<= 0 mouse_x) (< mouse_x width) (<= 0 mouse_y) (< mouse_y height))
           (define colSel (quotient mouse_x cell))
           (define filaSel (quotient mouse_y cell))

           (define matrizActual (unbox matrizJuego))
           (define matrizNueva
             (cond [(eq? evento 'left-down)  (descubrir matrizActual filaSel colSel)]   ; lógica pura
                   [(eq? evento 'right-down) (marcar    matrizActual filaSel colSel)]
                   [else matrizActual]))

           (when (not (eq? matrizNueva matrizActual))
             (set-box! matrizJuego matrizNueva)
             (send this refresh))))
       )))

  (send frameJuego show #t)
  frameJuego)










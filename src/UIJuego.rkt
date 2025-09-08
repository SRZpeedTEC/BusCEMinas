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
    (define M (crear-matrizJuego filas colums))
    
    ;; Comprobacion matriz ha sido creada
    (displayln (format "Matriz (~a x ~a):" filas colums))   
    ))


(send Menu show #t)






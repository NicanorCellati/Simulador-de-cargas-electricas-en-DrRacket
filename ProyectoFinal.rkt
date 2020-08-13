#lang racket
;ProyectoParaFinalDeAsignaturaPLyF.rkt

;invocar al modulo gui
(require racket/gui)
(require (lib "graphics.ss" "graphics"))(open-graphics)
(require racket/string)
(require racket/format)

;Logo de Unnoba
(define logo (open-pixmap "Simunnoba" 80 80))

;Incluimos el logo en la ventana principal
(((draw-pixmap-posn "Imagenes/LogoUnnoba.jpg") logo)(make-posn 0 0))

;DEFINICION DE FUNCIONES A UTILIZAR PARA EL CALCULO DE LAS MAGNITUDES DE LAS FUERZAS ENTRE LAS CARGAS

(define constanteDeCoulomb 9000000000) 

(define (cuadrado a) (* a a))

(define (raiz a) (sqrt a))

(define (distanciaEntreCargas x2 y2 x1 y1)
  (raiz (+ (cuadrado (- x2 x1))(cuadrado (- y2 y1)))))

(define (campoElectricoEnLaCargaDePrueba posx posy)
  (/ (* constanteDeCoulomb (send slider1 get-value))
     (cuadrado (distanciaEntreCargas posx posy 250 250))))

(define (fuerzaElectricaQueSufreCargaDePrueba posx posy)
  (/ (* constanteDeCoulomb (send slider1 get-value) (send slider2 get-value))
     (cuadrado (distanciaEntreCargas posx posy 250 250))))

;VENTANA INICIAL

(define ventanainicial
  (new frame%
       [label "Fisica II - UNNOBA"]
       [width 600]
       [height 400]))

(send ventanainicial show #t)

(define panelvertical1 (new vertical-panel%
                   [parent ventanainicial]
                   [horiz-margin 50]
                   [vert-margin 60]
                   [border 3]))

(new canvas%
     [parent panelvertical1]
     [horiz-margin 5]
     [vert-margin 5]
     [paint-callback
      (lambda (canvas dc)
        (send dc set-scale 3 3)
        (send dc set-text-foreground "blue")
        (send dc draw-text "SIMUNNOBA" 0 0))])

(define mensajeInicial
  (new message% [parent panelvertical1]
                [horiz-margin 50]
                [vert-margin 10]
                [min-height 5]
                [min-width 6]
                [label "Bienvenidos/as al simulador donde podrán visualizar la interacción entre dos cargas electricas y  ver como varian sus propiedades"]))


;Esto podria ir en una ventana a parte, donde haya inforacio acerca del software, por el momento la dejo aca
(define mensaje1a (new message%
                    [label "Tener en cuenta que la simulación que se llevará a cabo será entre una carga fuente y una carga de preba.. "]
                    [parent panelvertical1]
                    [horiz-margin 50]
                    [vert-margin 15]
                    [min-height 8]
                    [min-width 8]
                    [auto-resize #t]
                    ))

#|(define mensaje2a (new message%
                    [label "Tiene sentido ademas decir, que la carga que tendra movilidad sera la de prueba, y los datos sobre las magnitudes de las fuerzas que obtendremos a medida que se mueva, seran los que sufre dicha carga, pese a la presencia de la carga fuente. "]
                    [parent panelvertical1]
                    [horiz-margin 60]
                    [vert-margin 15]
                    [min-height 8]
                    [min-width 8]
                    [auto-resize #t]
                    ))|#

(new button% [parent panelvertical1]
             [label "Ingresar"]
             [horiz-margin 50]
             [vert-margin 25]
             [min-height 40]
             [min-width 60]
             [callback (lambda (button event)
                         (send mensajeInicial set-label "Entrando a Simunnoba...")
                         (sleep 1)
                         (send ventanainicial show #f)
                         (send ventana2 show #t))])


;VENTANA DE RESULTADOS DEL SIMULADOR
(define ventana-de-resultados 
  (new frame% [label "Resultados del simulador"]
              [width 150]
              [height 150]
              [enabled #t]))

(define panel-vertical-resultados (new vertical-panel%
                   [parent ventana-de-resultados]
                   [horiz-margin 50]
                   [vert-margin 60]
                   [border 3]))

(define mensajeResultados1 (new message%
                      [parent panel-vertical-resultados]
                      [label "Seleccionar la fuerza que desea conocer a la cual esta sometida la carga de prueba"]
                      [auto-resize #t]
                      ))

(define panel-horizontal-resultados (new horizontal-panel%
                                         [parent panel-vertical-resultados]))


(new button% [parent panel-horizontal-resultados]
     [label " Campo Electrico "]
     [callback (lambda (button event)
                 (send ventana2 show #f)
                 (send mensajeResultados1 set-label
                       (string-append " El campo electrico generado por la carga fuente en la posicion de la carga de prueba tiene una magnitud de: 1,26x10^11 N/C [Newton/Culombio]'")))])
                                            
(new button% [parent panel-horizontal-resultados]
     [label " Fuerza de Coulomb "]
     [callback (lambda (button event)
                  (send mensajeResultados1 set-label " La magnitud de la fuerza de Coulumb que sufre la carga de prueba debido a la presencia de la carga Fuente es: 3,78x10^11 N/C [Newton/Culombio]"))])

(define mensajeResultados2 (new message%
                      [parent panel-vertical-resultados]
                      [label "Si oprime el boton Finalizar, podra volver a la ventana de opciones y se cerrarán las ventanas abiertas en este momento"]
                      [auto-resize #t]
                      ))

(new button%
     [parent panel-vertical-resultados]
     [label " Finalizar "]
     [callback (lambda (b c)
                 (send ventana-de-resultados show #f)
                 (send ventana2 show #t)
                 (close-graphics))])
                 ;((close-viewport ventana-de-simulacion)))])


;VENTANA DE CONFIGURACION

(define ventana-de-configuracion
  (new frame% [label "Configuracion del entorno"]
              [width 150]
              [height 150]
              [enabled #t]))

;Si en algun momento pienso ampliar la funcionalidad podria usar este text-field
#|(define txt-cargas (new text-field%
                            [label " Cantidad de cargas :"] ;
                            [parent ventana-de-configuracion ]))|#

(define textventc1 (new message% ;textventc: texto en ventana de configuracion
                    [parent ventana-de-configuracion ]
                    [label " Atencion: las unidades de las cargas son en microcoulomb "]))

(define textventc2 (new message% ;textventc: texto en ventana de configuracion
                    [parent ventana-de-configuracion ]
                    [label " Seleccione los valores que tendran asociados las cargas "]))


(define slider1 (new slider%
                     [label " Valor de la carga fuente :"]
                     [parent ventana-de-configuracion]
                     [min-value -50]
                     [max-value 50]
                     [init-value 0]))

(define slider2 (new slider%
                     [label " Valor de la carga de prueba :"]
                     [parent ventana-de-configuracion]
                     [min-value -50]
                     [max-value 50]
                     [init-value 0]))

(new button%
     [parent ventana-de-configuracion]
     [label " Listo "]
     [callback (lambda (b c)
                 (send ventana-de-configuracion show #f)
                 (send ventana2 show #t))])


;VENTANA 2
(define ventana2 
  (new frame% [label "Panel de opciones"]
              [width 150]
              [height 150]
              [enabled #t])) 

(define mensaje2 (new message%
                    [label "Aún no ha configurado el entorno, le recomiendo que seleccione la opción configurar "]
                    [parent ventana2 ]
                    [horiz-margin 90]
                    [min-height 8]
                    [min-width 8]
                    [auto-resize #t]
                    ))

(define panelhorizontal2 (new horizontal-panel%
                   [parent ventana2]
                   [horiz-margin 250]
                   [vert-margin 20]
                   [border 3]))

(define lanzar (new button%
                    [parent panelhorizontal2]
                    [label "Configuracion"]
                    [min-height 5]
                    [min-width 5]
                    [callback
                     (lambda (b c)
                       (send ventana2 show #f)
                       (send ventana-de-configuracion show #t)
                       (send mensaje2 set-label
                             (string-append " Valor de la carga fuente : '"
                                            (number->string (send slider1 get-value))
                                            "', Valor de la carga de prueba : '"
                                            (number->string (send slider2 get-value))
                                            "'"
                                            )
                             )
                       )]
                    ))

(new button% [parent panelhorizontal2]
             [label "Comenzar"]
             [min-height 5]
             [min-width 5]
             [callback (lambda (button event)
                         (send ventana2 show #f)
                         (send ventana-de-resultados show #t)
                         (open-graphics)
                         (let* ( ; w1 and w2 are viewport descriptors for different windows
                                (ventana-de-simulacion  (open-viewport "ventana-de-simulacion" 600 600))
                                (ventana-de-simulacion2  (open-pixmap "ventana-de-simulacion2" 600 600))

                                ; d1 and d2 are functions that draw lines in different viewports
                                (d1  (draw-rectangle ventana-de-simulacion))
                                (d2  (draw-solid-ellipse ventana-de-simulacion))
                                (d3  (draw-solid-ellipse ventana-de-simulacion)))

                           ; draws a line in viewport labeled "viewport 1"
                           (d1 (make-posn 50 50) 500 500 "black")

                           ; draws a line in viewport labeled "viewport 2"
                           (d2 (make-posn 100 100) 50 50 "orange")

                           (d3 (make-posn 250 250) 60 60 "green")))]) 

                         ;(close-graphics))])           ; removes the viewports)])


;VENTANA DE SIMULACION

#|(define ventana-de-simulacion (open-viewport "ventana-de-simulacion" 600 600))
((close-viewport ventana-de-simulacion))

(define ventana-de-simulacion2 (open-pixmap "ventana-de-simulacion2" 600 600))

((draw-rectangle ventana-de-simulacion) (make-posn 50 50) 500 500 "black")

((draw-solid-ellipse ventana-de-simulacion) (make-posn 100 100) 50 50 "yellow")

;dibujamos el ciruclo que se va a mover

;(define (bolaverde draw-solid-ellipse ventana-de-simulacion2) (make-posn 250 250) 60 60 "green")
;(define (bolanaranja draw-solid-ellipse ventana-de-simulacion2) (make-posn posx posy) 50 50 "orange")
|#

#|(define (bola posx posy lado)
  (if (equal? lado 'arriba)
      (begin
        ((draw-rectangle ventana-de-simulacion2) (make-posn 50 50) 500 500 "black")
        ((draw-solid-ellipse ventana-de-simulacion2) (make-posn posx posy) 50 50 "orange")
        ((draw-solid-ellipse ventana-de-simulacion2) (make-posn 250 250) 60 60 "green"))

      (if (equal? lado 'abajo)
          (begin
            ((draw-rectangle ventana-de-simulacion2) (make-posn 50 50) 500 500 "black")
            ((draw-solid-ellipse ventana-de-simulacion2) (make-posn posx posy) 50 50 "orange")
            ((draw-solid-ellipse ventana-de-simulacion2) (make-posn 250 250) 60 60 "green"))

          (if (equal? lado 'izquierda)
              (begin
                ((draw-rectangle ventana-de-simulacion2) (make-posn 50 50) 500 500 "black")
                ((draw-solid-ellipse ventana-de-simulacion2) (make-posn posx posy) 50 50 "orange")
                ((draw-solid-ellipse ventana-de-simulacion2) (make-posn 250 250) 60 60 "green"))

              (if (equal? lado 'derecha)
                  (begin
                    ((draw-rectangle ventana-de-simulacion2) (make-posn 50 50) 500 500 "black")
                    ((draw-solid-ellipse ventana-de-simulacion2) (make-posn posx posy) 50 50 "orange")
                    ((draw-solid-ellipse ventana-de-simulacion2) (make-posn 250 250) 60 60 "green"))

                  (void)))))
  
  (copy-viewport ventana-de-simulacion2 ventana-de-simulacion)
  ;(close-viewport ventana-de-simulacion))
  ((clear-viewport ventana-de-simulacion2)))

;EVENTOS DEL TECLADO

(define (teclado posx posy tecla)

  (if (< posx 50)
      (begin
        (bola 50 posy 'izquierdo)
        (teclado 50 posy (key-value(get-key-press ventana-de-simulacion)))
        (lambda (b c)
                    (send mensajeResultados1 set-label
                             (string-append " Valor de campo electrico generado por la carga fuente en la posicion de la carga de prueba es : '"
                                            (number->string ((campoElectricoEnLaCargaDePrueba 50 (posn-y bola))))
                                            "', Valor de la fuerza de Coulomb que sufre la carga de prueba es: '"
                                            (number->string ((fuerzaElectricaQueSufreCargaDePrueba 50 (posn-y bola)))))
                                            "'"
                                            )))
           
      (if (> posx 500)
          (begin
            (bola 500 posy 'izquierdo)
            (teclado 500 posy(key-value(get-key-press ventana-de-simulacion)))
            (campoElectricoEnLaCargaDePrueba 500 (posn-y bola))
            (fuerzaElectricaQueSufreCargaDePrueba 500 (posn-y bola)))

          (if (< posy 50)
              (begin
                (bola posx 50 'izquierdo)
                (teclado posx 50 (key-value(get-key-press ventana-de-simulacion)))
                (campoElectricoEnLaCargaDePrueba (posn-x bola) 50)
                (fuerzaElectricaQueSufreCargaDePrueba (posn-x bola) 50))

              (if (> posy 500)
                  (begin
                    (bola posx 500 'izquierdo)
                    (teclado posx 500 (key-value(get-key-press ventana-de-simulacion)))
                    (campoElectricoEnLaCargaDePrueba (posn-x bola) 500)
                    (fuerzaElectricaQueSufreCargaDePrueba (posn-x bola) 500))
      
  (if (equal? tecla 'up)
      (begin
        (bola posx (- posy 10) 'arriba)
        (teclado posx (- posy 10) (key-value (get-key-press ventana-de-simulacion))))

      (if (equal? tecla 'down)
          (begin
            (bola posx (+ posy 10) 'abajo)
            (teclado posx (+ posy 10) (key-value (get-key-press ventana-de-simulacion))))

          (if (equal? tecla 'left)
              (begin
                (bola (- posx 10)posy 'izquierda)
                (teclado (- posx 10)posy (key-value (get-key-press ventana-de-simulacion))))

              (if (equal? tecla 'right)
                  (begin
                    (bola (+ posx 10)posy 'derecha)
                    (teclado (+ posx 10)posy (key-value (get-key-press ventana-de-simulacion))))
              
                  (teclado posx posy (key-value (get-key-press ventana-de-simulacion)))
              ))
          )
      )
  )
              )
          )
      )
  )
              
(teclado 200 200 'up)
|#










#|
;FORMA DE HACERLO CON UNA SOLA VENTANA

(define ventana-de-simulacion (open-viewport "ventana-de-simulacion" 600 600))

(define ventana-de-simulacion2 (open-pixmap "ventana-de-simulacion2" 600 600))

((draw-rectangle ventana-de-simulacion) (make-posn 50 50) 500 500 "black")

((draw-string ventana-de-simulacion) (make-posn 50 25) "Verde" "black")

((draw-solid-ellipse ventana-de-simulacion) (make-posn 100 100) 50 50 "yellow")

(define (moverCirculo x y)
  (begin
    ((draw-solid-ellipse ventana-de-simulacion) (make-posn x y) 50 50 "yellow")
    ((draw-rectangle ventana-de-simulacion) (make-posn 50 50) 500 500 "black")))

(define (teclado x y tecla)
  
  ; PARA QUE NO SE SALGA DEL RECTANGULO PARTE DERECHA
  (if(< x 50)
     (begin
       ((draw-solid-ellipse ventana-de-simulacion) (make-posn 40 y) 50 50 "white") ; BORRAR RASTRO CIRCULO
       (moverCirculo 50 y)
       (teclado 50 y (key-value(get-key-press ventana-de-simulacion))))

     ; PARA QUE NO SE SALGA DEL RECTANGULO PARTE IZQUIERDA
     (if(> x 500)
        (begin
          ((draw-solid-ellipse ventana-de-simulacion) (make-posn 510 y) 50 50 "white") ; BORRAR RASTRO CIRCULO
          (moverCirculo 500 y)
          (teclado 500 y (key-value(get-key-press ventana-de-simulacion))))

        ; PARA QUE NO SE SALGA DEL RECTANGULO PARTE ARRIBA
        (if(< y 50)
           (begin
             ((draw-solid-ellipse ventana-de-simulacion) (make-posn x 40) 50 50 "white") ; BORRAR RASTRO CIRCULO
             (moverCirculo x 50)
             (teclado x 50 (key-value(get-key-press ventana-de-simulacion))))

           ; PARA QUE NO SE SALGA DEL RECTANGULO PARTE ABAJO
           (if(> y 500)
              (begin
                ((draw-solid-ellipse ventana-de-simulacion) (make-posn x 510) 50 50 "white") ; BORRAR RASTRO CIRCULO
                (moverCirculo x 500)
                (teclado x 500 (key-value(get-key-press ventana-de-simulacion))))
        
              (if(equal? tecla 'up)
                 (begin
                   ((draw-solid-ellipse ventana-de-simulacion) (make-posn x y) 50 50 "white") ; BORRAR RASTRO CIRCULO
                   (moverCirculo x (- y 10))
                   (teclado x (- y 10) (key-value(get-key-press ventana-de-simulacion)))) ; LLAMADA RECURSIVA

                 (if(equal? tecla 'down)
                    (begin
                      ((draw-solid-ellipse ventana-de-simulacion) (make-posn x y) 50 50 "white") ; BORRAR RASTRO CIRCULO
                      (moverCirculo x (+ y 10))
                      (teclado x (+ y 10) (key-value(get-key-press ventana-de-simulacion))))

                    (if(equal? tecla 'left)
                       (begin
                         ((draw-solid-ellipse ventana-de-simulacion) (make-posn x y) 50 50 "white") ; BORRAR RASTRO CIRCULO
                         (moverCirculo (- x 10) y)
                         (teclado (- x 10) y (key-value(get-key-press ventana-de-simulacion))))
              
                       (if(equal? tecla 'right)
                          (begin
                            ((draw-solid-ellipse ventana-de-simulacion) (make-posn x y) 50 50 "white") ; BORRAR RASTRO CIRCULO
                            (moverCirculo (+ x 10) y)
                            (teclado (+ x 10) y (key-value(get-key-press ventana-de-simulacion))))
                          ; FUNCION EN FALSO, PARA NO DETENER EL LLAMADO RECURSIVO.
                          (teclado x y (key-value(get-key-press ventana-de-simulacion))))))))))))

; LLAMAMOS A LA FUNCION TECLADO
(teclado 270 270 'up)

|#



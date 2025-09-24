             IDENTIFICATION DIVISION.
       PROGRAM-ID. crearclientes.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS KEY-STATUS.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       SELECT OPTIONAL DATOS  ASSIGN TO
           "DATOS.DAT"
           ORGANIZATION INDEXED
           ACCESS MODE DYNAMIC
           RECORD KEY NIT
           ALTERNATE RECORD KEY IS NIT-EDAD
                     WITH DUPLICATES
           ALTERNATE RECORD KEY IS DIRECCION
                     WITH DUPLICATES
           ALTERNATE RECORD KEY IS NOMBRE
                     WITH DUPLICATES
           FILE STATUS STATUS-DATOS.

       DATA DIVISION.
       FILE SECTION.

       FD  DATOS LABEL RECORD IS STANDARD.
       01  REG-DATOS.
           05 NIT.
              07 CODIGO           PIC 9(06).
           05 NOMBRE              PIC A(60).
           05 DIRECCION           PIC X(40).
           05 NIT-EDAD.
              07 EDAD             PIC 9(03).

       WORKING-STORAGE SECTION.
       77  AA           PIC 99.
       77  JJ           PIC 9(6).
       77  J            PIC 99.
       77  STATUS-DATOS PIC XX.
       77  PAUSAR       PIC X value spaces.
       77  CAIDA        PIC X.
       77  MAS-CODIGO   PIC ZZZZZ9.
       77  MAS-EDAD     PIC ZZ9.
       77  ERROR-TEXTO  PIC X(50).
       77  SN           PIC X.
       77  SNN          PIC X.
       77  TEXTO-LIMPIO PIC X(80) VALUE SPACES.
       77  LINEA-LIMPIO PIC X(60) VALUE SPACES.
       77  AUX-OP       PIC 9(04).
       77  AUX-EDAD     PIC 9(04).
       77  AUX-NOMBRE   PIC A(60).
       77  AUX-DIRE     PIC X(40).
       77  AUX-LINE     PIC 99.

       01  KEY-STATUS.
           03 KEY-LLAVE     PIC 9(04).
              88 F2         VALUE 1002.
              88 F3         VALUE 1003.
              88 F5         VALUE 1005.
              88 F6         VALUE 1006.
              88 F7         VALUE 1007.
              88 F8         VALUE 1008.
              88 F9         VALUE 1009.
              88 ESC        VALUE 1004.

       01  vector-datos.
           05 LVECC occurs 9999 times.
              10 VCOD   PIC 9(06).

       PROCEDURE DIVISION.
       DECLARATIVES.
       000-File-Error SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON DATOS.
       000-Handle-Error.
           move "S" to caida.
       END DECLARATIVES.
       MAIN-LOGIC SECTION.
       PROGRAM-BEGIN.
           perform abrir
           perform menu thru fin-menu.
           perform cerrar.
           goback.

       abrir.
           open i-o datos.
       CERRAR.
           close DATOS.

       menu.
           MOVE SPACES TO SN.
           PERFORM UNTIL SN = "F"
            PERFORM limpiar-todo
            display "Unidad Vecinal" at line 1 col 1
               display "Opciones del Sistema" at line 5 col 25
                                        with reverse-video
            display "(F5) Agregar, Modificar, Eliminar" at line 7  col 4
            display "(F6) Consulta por Edad           " at line 8  col 4
            display "(F7) Consulta por Nombre         " at line 9  col 4
            display "(F8) Consulta por Direccion      " at line 10 col 4
            display "(F9) Exit                        " at line 11 col 4
            display "( ) Presione Tecla              " at line 13 col 4
                                                     with reverse-video

                   accept SN at line 13 col 5 with prompt underline
                          reverse-video end-accept

                   IF F5
                     perform ingreso thru fin-ingreso
                   end-if
                   IF F6 OR F7 OR F8
                      MOVE KEY-LLAVE TO AUX-OP
                      perform consulta thru fin-consulta until esc
                   end-if
                   IF F9   MOVE "F" TO SN END-IF
           END-PERFORM.
       fin-menu.
           exit.

       consulta.
           MOVE AUX-OP  TO KEY-LLAVE
           INITIALIZE vector-datos
      *Mueve espacios a REG-DATOS para no mostrar los 0 de edad
           move spaces to REG-DATOS
           perform limpiar-todo.
           display "Unidad Vecinal" at line 1 col 1
           display "Consulta Informacion" at line 5 col 25
                                        with reverse-video.
           display
           "*********************************************************"
                                                     at line 8 col 1
                                               "*********************"
           display
           "Codigo   Nombre                                 Edad"
                                     at line 9 col 1 with reverse-video
                        "   Direccion              " with reverse-video
           display
           "**********************************************************"
                                                        at line 10 col 1
                                              "********************"
           display "(F4) Exit /(F2) Sube (F3) Baja" at
                    line 24 col 25 with reverse-video.
           IF F6 go to consulta-edad.
           IF F7 go to consulta-nombre.
           IF F8 go to consulta-direccion.
       consulta-edad.
           display "Edad      :"  at line 7 col 4 with reverse-video.
           accept edad at line 7 col 17 with prompt UNDERLINE UPDATE.

           display "Edad      :" at line 7 col 4.
           MOVE EDAD TO MAS-EDAD
           display MAS-edad  at line 7 col 17.

           if esc go to fin-consulta.
           if F2  go to aceptar-edad.
           if EDAD < 1 OR > 100
              move "Edad fuera de Rango" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NIT-EDAD
              GO TO consulta-edad.
           GO TO consulta-empezar.
        consulta-nombre.
           display "Nombre    :"  at line 7 col 4 with reverse-video.
           accept NOMBRE at line 7 col 17 with prompt UNDERLINE UPDATE.

           display "Nombre    :" at line 7 col 4.
           display NOMBRE  at line 7 col 17.

           if esc go to fin-consulta.
           if F2  go to aceptar-nombre.
           if NOMBRE = SPACES
              move "Debe Ingresar Nombre" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NOMBRE
              GO TO consulta-nombre.
           MOVE 0 TO AA
      *ESTE IF RECORRE  NOMBRE RESTANDOLE 1 A J HASTA QUE SEA MENOR A 1
      * O QUE NOMBRE NO SEA IGUAL ESPACIOS (J:1) INDICA QUE EN LA POSICION DE J
      *DONDE NO HAY ESPACIOS QUE AGARRA UTILIZE UN BYTE DE ESA POSICION Y LA COMPARE CON ESPACIOS
      *DE NO SER ESPACIOS QUIERE DECIR QUE ENCONTRO UN NOMBRE.
           PERFORM VARYING J FROM 60 BY -1 UNTIL J < 1
                   OR NOMBRE(J:1) NOT = SPACES
                   ADD 1 TO AA
           END-PERFORM
           COMPUTE AA = 60 - AA

           GO TO consulta-empezar.
        consulta-direccion.
           display "Direccion  :"  at line 7 col 4 with reverse-video.
           accept direccion at line 7 col 17 with prompt
                                        UNDERLINE UPDATE.

           display "Direccion  :" at line 7 col 4.
           display direccion  at line 7 col 17.

           if esc go to fin-consulta.
           if F2  go to aceptar-direccion.
           if direccion = SPACES
              move "Debe Ingresar Direccion" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO DIRECCION
              GO TO consulta-direccion.
           MOVE DIRECCION TO AUX-DIRE
           MOVE 0 TO AA
           PERFORM VARYING J FROM 40 BY -1 UNTIL J < 1
                   OR DIRECCION(J:1) NOT = SPACES
                   ADD 1 TO AA
           END-PERFORM
           COMPUTE AA = 40 - AA.
       consulta-empezar.
           MOVE 1      TO JJ.
           MOVE AUX-OP TO KEY-LLAVE.
           IF F6
              MOVE  EDAD TO AUX-EDAD
              START DATOS KEY IS NOT LESS THAN NIT-EDAD
                    INVALID KEY GO TO fin-consulta
              END-START
           END-IF.
           IF F7
              MOVE  NOMBRE TO AUX-NOMBRE
              START DATOS KEY IS NOT LESS THAN NOMBRE
                    INVALID KEY GO TO fin-consulta
              END-START
           END-IF.
           IF F8
              MOVE  DIRECCION TO AUX-DIRE
              START DATOS KEY IS NOT LESS THAN DIRECCION
                    INVALID KEY GO TO fin-consulta
              END-START
           END-IF.
       consulta-vector.
           READ DATOS NEXT WITH NO LOCK AT END
                      GO TO consulta-mostrar.
           IF F6
              IF AUX-EDAD NOT = EDAD
                 GO TO consulta-mostrar
              END-IF
           END-IF.
      *CODIGO QUE COMPARA LA IGUALDAD ENTRE NOMBRES.
           IF F7
              IF AUX-NOMBRE(1:AA) NOT = NOMBRE(1:AA)
                 GO TO consulta-vector
              END-IF
           END-IF.
           IF F8
              IF AUX-DIRE(1:AA) NOT = DIRECCION(1:AA)
                 GO TO consulta-vector
              END-IF
           END-IF.

           MOVE CODIGO TO VCOD(JJ)
           ADD 1 TO JJ
           go to consulta-vector.
       consulta-mostrar.
           MOVE 1 TO JJ J.
           MOVE 11 TO AUX-LINE.
       consulta-display.
           IF   VCOD(JJ) = ZEROES OR JJ > 9999
                go to consulta-pregunta.

           INITIALIZE MAS-CODIGO MAS-EDAD
           MOVE VCOD(JJ)   TO CODIGO.
           READ DATOS WITH NO LOCK INVALID KEY
                      MOVE SPACES  TO REG-DATOS.

           MOVE CODIGO TO MAS-CODIGO
           MOVE EDAD   TO MAS-EDAD

           DISPLAY MAS-CODIGO  AT LINE  AUX-LINE COL 2.
           DISPLAY NOMBRE      AT LINE  AUX-LINE COL 15.
           DISPLAY MAS-EDAD    AT LINE  AUX-LINE COL 50.
           DISPLAY DIRECCION   AT LINE  AUX-LINE COL 55.
           ADD   1 TO AUX-LINE JJ
           IF    AUX-LINE > 21
                 DISPLAY
                 "Presiones Enter para Continuar/F2 para Retroceder"
                 at line 23 col 2 with reverse-video
                 accept sn line 23 col 1 with prompt
                                  UNDERLINE UPDATE end-accept

                 MOVE 11 TO AUX-LINE
                 PERFORM VARYING J FROM 11 BY 1 UNTIL J > 21
                         display TEXTO-LIMPIO at line J col 1
                 END-PERFORM
                 IF F2
                    IF JJ = 12
                       go to consulta
                    ELSE
                       COMPUTE JJ = JJ - 11
                       MOVE 11 TO AUX-LINE
                       go to consulta-display
                   END-IF
                END-IF
           END-IF.

           GO TO consulta-display.
       consulta-pregunta.
           DISPLAY
           "Presiones Enter para Continuar/F2 para Retroceder"
                       at line 23 col 2 with reverse-video
           accept sn line 23 col 1 with prompt
                   UNDERLINE UPDATE end-accept .
           IF F2
             IF JJ < 12
                GO TO CONSULTA
             ELSE
              COMPUTE AA = (AUX-LINE - 11) + 1
              COMPUTE JJ =
              (JJ + (11 - AA) - 22 ) + 1
              MOVE 11 TO AUX-LINE
              PERFORM VARYING J FROM 11 BY 1 UNTIL J > 21
                      display TEXTO-LIMPIO at line J col 1
              END-PERFORM
              GO TO consulta-display
            END-IF
           END-IF.
       fin-consulta.
           exit.


       limpiar-todo.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > 24
                   display TEXTO-LIMPIO at line J col 1
           END-PERFORM.
************************************************************************
***********  AGREGAR MODIFICAR ELIMINAR                                *
************************************************************************
       ingreso.
           PERFORM limpiar-todo
           MOVE    SPACES TO  REG-DATOS.
           perform pantalla thru fin-pantalla
           perform aceptar-datos thru fin-aceptar-datos until esc.
       fin-ingreso.
           exit.

       pantalla.
           display "Unidad Vecinal" at line 1 col 1
           display "Ingresa Informacion" at line 5 col 25
                                        with reverse-video.
           display "Codigo    :"  at line 7  col 4.
           display "Edad      :"  at line 8  col 4.
           display "Nombre    :"  at line 9  col 4.
           display "Direccion :"  at line 10 col 4.
           display "(F4) Exit /(F2) Sube (F3) Baja" at
                    line 24 col 25 with reverse-video.
       fin-pantalla.
           exit.

       aceptar-datos.
           MOVE SPACES TO  REG-DATOS.
           PERFORM aceptar-limpiar.
       aceptar-nit.
      *Muestra el menu de ingreso de datos y pide datos.
           display "Codigo    :"  at line 7  col 4 with reverse-video.
           accept CODIGO at line 7 col 17 with prompt UNDERLINE UPDATE.
      *Muestra el codigo con el dato ingresado con un DISPLAY.
           display "Codigo    :" at line 7 col 4.
           MOVE CODIGO TO MAS-CODIGO
           display MAS-CODIGO at line 7 col 17

           READ DATOS WITH NO LOCK
                INVALID KEY CONTINUE
                NOT INVALID KEY
                    PERFORM LLENAR THRU FIN-LLENAR
                    IF SNN = "E"
                       GO TO ACEPTAR-DATOS
                    END-IF
           END-READ

           if esc go to fin-aceptar-datos.
           if F2  go to aceptar-nit.
           if F3  go to aceptar-edad.
           if CODIGO = zeroes
              move "Error Codido debe ser mayor a cero" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NIT
              GO TO ACEPTAR-NIT.
       aceptar-edad.
           display "Edad      :"  at line 8  col 4 with reverse-video.
           accept edad at line 8 col 17 with prompt UNDERLINE UPDATE.

           display "Edad      :" at line 8 col 4.
           MOVE EDAD TO MAS-EDAD
           display MAS-edad  at line 8 col 17.

           if esc go to fin-aceptar-datos.
           if F2  go to aceptar-nit.
           if F3  go to aceptar-nombre.
           if EDAD < 1 OR > 100
              move "Edad fuera de Rango" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NIT-EDAD
              GO TO ACEPTAR-EDAD.
       aceptar-nombre.
           display "Nombre    :"  at line 9  col 4 with reverse-video.
           accept nombre at line 9 col 17 with prompt UNDERLINE UPDATE.

           display "Nombre    :" at line 9 col 4.
           display nombre  at line 9 col 17.

           if esc go to fin-aceptar-datos.
           if F2  go to aceptar-edad.
           if F3  go to aceptar-direccion.
           if nombre = spaces
              move "Debe Ingresar Nombre" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NOMBRE
              GO TO ACEPTAR-EDAD.
       aceptar-direccion.
           display "Direccion :" at line 10  col 4 with reverse-video.
           accept direccion at line 10 col 17 with
                          prompt UNDERLINE UPDATE.

           display "Direccion :" at line 10 col 4.
           display direccion  at line 10 col 17.

           if esc go to fin-aceptar-datos.
           if F2  go to aceptar-nombre.
           if F3  go to aceptar-direccion.
           if direccion = spaces
              move "Debe Ingresar direccion" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO DIRECCION
              GO TO ACEPTAR-DIRECCION.
       aceptar-validacion-codigo.
           if codigo = zeroes
              move "Error Codido debe ser mayor a cero" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NIT
              GO TO ACEPTAR-NIT.
       aceptar-validacion-edad.
           if edad < 1 or > 100
              move "Edad fuera de Rango" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NIT-EDAD
              GO TO ACEPTAR-EDAD.
       aceptar-validacion-nombre.
           if NOMBRE = SPACES
              move "Debe Ingresar Nombre" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO NOMBRE
              GO TO ACEPTAR-NOMBRE.
       aceptar-validacion-direccion.
           if direccion = SPACES
              move "Debe Ingresar direccion" TO
                    ERROR-TEXTO
              PERFORM MUESTRO-ERROR
              MOVE SPACES TO DIRECCION
              GO TO ACEPTAR-DIRECCION.
           PERFORM GRABAR THRU FIN-GRABAR.
       aceptar-limpiar.
           DISPLAY LINEA-LIMPIO AT LINE 7  COL 15.
           DISPLAY LINEA-LIMPIO AT LINE 8  COL 15.
           DISPLAY LINEA-LIMPIO AT LINE 9  COL 15.
           DISPLAY LINEA-LIMPIO AT LINE 10 COL 15.
       fin-aceptar-datos.
           exit.

       GRABAR.
           WRITE REG-DATOS INVALID KEY
                 REWRITE REG-DATOS INVALID KEY
                         MOVE "ERROR AL GRABAR/REGRABAR" TO ERROR-TEXTO
                         PERFORM MUESTRO-ERROR
                         GO TO FIN-GRABAR
                 END-REWRITE
           END-WRITE.
           MOVE "INFORMACION FUE GRABADA" TO ERROR-TEXTO
           PERFORM MUESTRO-ERROR .
       FIN-GRABAR.
           EXIT.

       LLENAR.
           MOVE CODIGO TO MAS-CODIGO
           display MAS-CODIGO at line 7  col 17.
           MOVE EDAD TO MAS-EDAD
           display MAS-edad   at line 8  col 17.
           display nombre     at line 9  col 17.
           display direccion  at line 10 col 17.

           Display "*******************************************"
                                           at line 12 col 2.
           Display "*( ) (M)odifica   (E)limina               *"
                                           at line 13 col 2.
           Display "*******************************************"
                                           at line 14 col 2.
           MOVE SPACES TO SNN
           PERFORM UNTIL SNN = "M" OR "E"
                   ACCEPT SNN at line 13 col 4 end-accept
                   IF SNN = "e" MOVE "E" TO SNN END-IF
                   IF SNN = "m" MOVE "M" TO SNN END-IF
           END-PERFORM
           IF SNN = "E"
               DELETE DATOS INVALID KEY
                      MOVE "ERROR AL ELIMINAR" TO ERROR-TEXTO
                      PERFORM MUESTRO-ERROR
               END-DELETE
               MOVE "INFORMACION ELIMINADA" TO ERROR-TEXTO
               PERFORM MUESTRO-ERROR
           END-IF.
           DISPLAY TEXTO-LIMPIO AT LINE 12 COL 1.
           DISPLAY TEXTO-LIMPIO AT LINE 13 COL 1.
           DISPLAY TEXTO-LIMPIO AT LINE 14 COL 1.
       FIN-LLENAR.
           EXIT.

       MUESTRO-ERROR.
           display ERROR-TEXTO at line 23  col 2 with reverse-video
                   "Enter para Continuar".
           accept SN at line 23 col 1 with REVERSE-VIDEO.
           display TEXTO-LIMPIO at line 23  col 1.

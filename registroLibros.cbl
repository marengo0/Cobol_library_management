       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 LIBRERIA.
       AUTHOR.                     LUCAS GALEANO.
       DATE-WRITTEN                9/7/2023.
       DATE-COMPILED               5/8/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REG-LIBROS ASSIGN TO "REGISTRO-LIBROS.DAT"
                               ORGANIZATION IS INDEXED
                               RECORD KEY IS LIBRO-ID
                               ALTERNATE RECORD KEY IS NOMBRE
                               ALTERNATE RECORD KEY IS EDITORIAL
                               WITH DUPLICATES
                               ACCESS MODE IS DYNAMIC
                               FILE STATUS IS FS-REGISTRO-LIBROS.


       DATA DIVISION.
       FILE SECTION.

       FD REG-LIBROS.

       01 DATOS-LIBRO.
           05 LIBRO-ID PIC 999     VALUE ZEROS.
           05 NOMBRE   PIC X(20)   VALUE SPACES.
           05 EDITORIAL PIC X(20)  VALUE SPACES.
           05 ESTADO   PIC X(12)   VALUE SPACES.

       WORKING-STORAGE SECTION.
      *Estados de error del archivo.
       01 FS-REGISTRO-LIBROS   PIC XX.
           88 ESTA-REG         VALUE "00" "02".
           88 N-ESTA-REG       VALUE "23".
           88 F-NOEXISTE-REG   VALUE "35".
           88 FIN-REG          VALUE "10".
           88 F-ABIERTO        VALUE "12" "41".
           88 LENGHT-ERROR     VALUE "04".
           88 F-NO-ABIERTO     VALUE "47".

      *Variables del programa.
       77 CONTINUAR            PIC X.
       77 CONTINUAR-REG        PIC X.
       77 ELIMINAR-OPC         PIC X.
       77 BUSCAR-OPC           PIC X.
       77 OPC                  PIC X.
       77 LIMPIAR              PIC X(80) VALUE SPACES.
       77 LIMPIAR-LINEA        PIC X(80) VALUE SPACES.
       77 N                    PIC 99.
       77 AUX-ID               PIC 999.
       77 AUX-EDIT             PIC X(20).
       77 AUX-NOMBRE           PIC X(20).

       PROCEDURE DIVISION.
       DECLARATIVES.
       FILE-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE ON REG-LIBROS.
       END DECLARATIVES.
      *LOGICA PRINCIPAL
       MENU.
           PERFORM UNTIL CONTINUAR = "N"
           PERFORM LIMPIAR-PANTALLA
           DISPLAY "ADMINISTRADOR DE CATALOGO" AT LINE 1 COL 1 WITH
                                                           REVERSE-VIDEO
           DISPLAY " "                         AT LINE 3 COL 1
           DISPLAY "1.Registrar, Editar, Remplazar libro" AT LINE 4
                                                           COL 1
           DISPLAY "2.Eliminar libro"          AT LINE 5 COL 1
           DISPLAY "3.Ver catalogo"            AT LINE 6 COL 1
           DISPLAY "4.Crear nuevo archivo"     AT LINE 7 COL 1
           DISPLAY "5.Buscar libro"            AT LINE 8 COL 1
           DISPLAY "0.Salir"                   AT LINE 9 COL 1
           DISPLAY " "                         AT LINE 10 COL 1
           DISPLAY "Ingrese tecla ( )"         AT LINE 11 COL 1 WITH
                                                           REVERSE-VIDEO
           ACCEPT OPC                          AT LINE 11 COL 16 WITH
                                               PROMPT UNDERLINE
           EVALUATE OPC
               WHEN 1 PERFORM REGISTRAR-LIBRO
               WHEN 2 PERFORM ELIMINAR-LIBRO
               WHEN 3 PERFORM VER-LIBROS
               WHEN 4 PERFORM CREAR-ARCHIVO
               WHEN 5 PERFORM BUSCAR-LIBRO
               WHEN 0 MOVE "N" TO CONTINUAR
           END-EVALUATE
           END-PERFORM.
           STOP RUN.


       REGISTRAR-LIBRO.
           MOVE SPACES TO DATOS-LIBRO.
           PERFORM LIMPIAR-PANTALLA.
           MOVE "S" TO CONTINUAR-REG.
           PERFORM ABRIR-IO-ARCHIVO
           IF F-NOEXISTE-REG
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "ERROR, NO EXISTE UN ARCHIVO DE REGISTROS"
                       WITH BACKGROUND-COLOR 4 AT LINE 1 COL 1


               DISPLAY "Presione ENTER para salir al menu"
                                               AT LINE 7 COL 12 WITH
                                               REVERSE-VIDEO
                       ACCEPT OPC AT LINE  1 COL 41
               PERFORM MENU
           END-IF.
      *************************************************************************
      *INTERFAZ MENU DE INGRESO DE DATOS
      *************************************************************************
           DISPLAY "Numero de serie:" AT LINE 1 COL 1.
           DISPLAY "Nombre:"       AT LINE 2 COL 1.
           DISPLAY "Editorial:"    AT LINE 3 COL 1.
           DISPLAY "Estado:"       AT LINE 4 COL 1.
           DISPLAY
           "presione ENTER sin ingresar datos para salir al menu"
                                               AT LINE 7 COL 12
                                                     WITH REVERSE-VIDEO.


           DISPLAY "Numero de serie    :" AT LINE 1 COL 1
                                                     WITH REVERSE-VIDEO.

           ACCEPT LIBRO-ID WITH PROMPT UNDERLINE UPDATE AT
                                                   LINE 1 COL 25.


           IF LIBRO-ID EQUALS 000 MOVE "N" TO CONTINUAR-REG
           ELSE
           PERFORM ABRIR-IO-ARCHIVO
           READ REG-LIBROS END-READ
           IF ESTA-REG THEN
               DISPLAY NOMBRE          AT LINE 2 COL 25
               DISPLAY EDITORIAL       AT LINE 3 COL 25
               DISPLAY ESTADO          AT LINE 4 COL 25
           END-IF
           DISPLAY "Numero de serie    :" AT LINE 1 COL 1
           DISPLAY LIBRO-ID            AT LINE 1 COL 25

           DISPLAY "Nombre    :"     AT LINE 2 COL 1 WITH REVERSE-VIDEO
           ACCEPT NOMBRE               AT LINE 2 COL 25
           DISPLAY "Nombre:    "       AT LINE 2 COL 1
           DISPLAY NOMBRE              AT LINE 2 COL 25

           DISPLAY "Editorial    :"  AT LINE 3 COL 1 WITH REVERSE-VIDEO
           ACCEPT EDITORIAL            AT LINE 3 COL 25
           DISPLAY "Editorial    :"    AT LINE 3 COL 1
           DISPLAY EDITORIAL           AT LINE 3 COL 25

           DISPLAY "Estado    :"     AT LINE 4 COL 1 WITH REVERSE-VIDEO
           ACCEPT ESTADO               AT LINE 4 COL 25
           DISPLAY "Estado    :"       AT LINE 4 COL 1
           DISPLAY ESTADO              AT LINE 4 COL 25
      *************************************************************************
           IF N-ESTA-REG THEN
               WRITE DATOS-LIBRO END-WRITE
           ELSE
               REWRITE DATOS-LIBRO END-REWRITE
           END-IF
           IF NOT ESTA-REG THEN
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "ERROR AL REGISTRAR DATOS" AT LINE 1 COL 1
                                           WITH BACKGROUND-COLOR 4
           END-IF.
           PERFORM CERRAR-ARCHIVO.


       VER-LIBROS.
           PERFORM ABRIR-ARCHIVO-INPUT.
           IF F-NOEXISTE-REG THEN
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "ERROR, NO EXISTE UN ARCHIVO DE REGISTROS"
                       WITH BACKGROUND-COLOR 4 AT LINE 1 COL 1



               DISPLAY "Presione ENTER para salir al menu"
                                               AT LINE 7 COL 12 WITH
                                               REVERSE-VIDEO
               ACCEPT OPC AT LINE  10 COL 34
           ELSE
               PERFORM LIMPIAR-PANTALLA
               INITIALIZE LIBRO-ID
               START REG-LIBROS KEY IS >= LIBRO-ID END-START
               READ REG-LIBROS NEXT RECORD END-READ
               DISPLAY
               "ID |TITULO            |EDITORIAL          |ESTADO     "
               WITH REVERSE-VIDEO AT LINE 1 COL 1
               PERFORM VARYING N FROM 02 BY 1 UNTIL FIN-REG
                   DISPLAY DATOS-LIBRO AT LINE N COL 1
                   READ REG-LIBROS NEXT RECORD END-READ
               END-PERFORM
               ADD 1 TO N
               DISPLAY "Presione ENTER para salir al menu"
                                               AT LINE N COL 1 WITH
                                               REVERSE-VIDEO
               ACCEPT OPC AT LINE N COL 34

           END-IF.
           PERFORM CERRAR-ARCHIVO.


       ELIMINAR-LIBRO.
           PERFORM LIMPIAR-PANTALLA.
           PERFORM ABRIR-IO-ARCHIVO.

           IF F-NOEXISTE-REG
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "ERROR, NO EXISTE UN ARCHIVO DE REGISTROS"
                       WITH BACKGROUND-COLOR 4 AT LINE 1 COL 1


               DISPLAY "Presione ENTER para salir al menu"
                                               AT LINE 7 COL 12 WITH
                                               REVERSE-VIDEO
                       ACCEPT OPC AT LINE  1 COL 41
               PERFORM MENU
           END-IF.

           MOVE SPACES TO DATOS-LIBRO

           DISPLAY "Indique por id el libro que desea eliminar:"
           WITH REVERSE-VIDEO AT LINE 1 COL 1.
           DISPLAY "Presione ENTER para salir al menu"
           WITH REVERSE-VIDEO AT LINE 10 COL 12.

           ACCEPT LIBRO-ID AT LINE 1 COL 46 WITH PROMPT UNDERLINE UPDATE.

           IF LIBRO-ID NOT EQUALS 000

           PERFORM LIMPIAR-PANTALLA

           READ REG-LIBROS END-READ
           IF N-ESTA-REG
               DISPLAY "EL REGISTRO NO EXISTE" AT LINE 1 COL 1
                                               WITH BACKGROUND-COLOR 4
                           ACCEPT OPC AT LINE 1 COL 22

           ELSE

           DISPLAY
               "ID |TITULO            |EDITORIAL         |ESTADO     "
                                   WITH REVERSE-VIDEO AT LINE 1 COL 1
               DISPLAY DATOS-LIBRO AT LINE 2 COL 1
               DISPLAY "ELIMINAR(1)|VOLVER(0) :" AT LINE 5 COL 1
                                                   WITH REVERSE-VIDEO

               ACCEPT ELIMINAR-OPC AT LINE 5 COL 25
               EVALUATE ELIMINAR-OPC
                   WHEN 1
                   DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
                   DELETE REG-LIBROS INVALID KEY
                       DISPLAY "ERROR AL ELIMINAR" AT LINE 5 COL 1
                   END-DELETE
                   DISPLAY "REGISTRO ELIMINADO CON EXITO" AT LINE 5
                                                COL 1
                                                WITH BACKGROUND-COLOR 2
                           ACCEPT OPC AT LINE 5 COL 29
                   WHEN 0 PERFORM ELIMINAR-LIBRO
           END-IF

           END-IF.
           PERFORM CERRAR-ARCHIVO.


       CREAR-ARCHIVO.
           PERFORM ABRIR-IO-ARCHIVO
           IF NOT F-NOEXISTE-REG THEN
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "SE DETECTO UN ARCHIVO EXISTENTE, DE PROCEDER EL"
               AT LINE 1 COL 1 WITH BACKGROUND-COLOR 6
               " ARCHIVO ACTUAL SERA ELIMINADO" AT LINE 1 COL 48
                               WITH BACKGROUND-COLOR 6
               DISPLAY "ELIMINAR(1)|SALIR AL MENU(0) :" AT LINE 10 COL 1
                                                   WITH REVERSE-VIDEO
               ACCEPT ELIMINAR-OPC AT LINE 10 COL 31
               EVALUATE ELIMINAR-OPC
                   WHEN 1
                   PERFORM CERRAR-ARCHIVO
                   OPEN OUTPUT REG-LIBROS
                   PERFORM CERRAR-ARCHIVO
                   PERFORM LIMPIAR-PANTALLA
                   DISPLAY "ARCHIVO CREADO CON EXITO."
                                               WITH BACKGROUND-COLOR 2
                   ACCEPT OPC AT LINE 1 COL 27
                   WHEN 0 PERFORM CERRAR-ARCHIVO EXIT
           ELSE
               OPEN OUTPUT REG-LIBROS
               CLOSE REG-LIBROS
               PERFORM LIMPIAR-PANTALLA
               DISPLAY "ARCHIVO CREADO CON EXITO."
                                               WITH BACKGROUND-COLOR 2
               ACCEPT OPC AT LINE 1 COL 27
           END-IF.


       BUSCAR-LIBRO.
           PERFORM LIMPIAR-PANTALLA.
           IF NOT F-NOEXISTE-REG
           PERFORM ABRIR-IO-ARCHIVO
               DISPLAY "MENU DE BUSQUEDA"          AT LINE 1 COL 1
                                               WITH REVERSE-VIDEO
               DISPLAY "(I)Busqueda por id"        AT LINE 3 COL 1
               DISPLAY "(N)Busqueda por nombre"    AT LINE 4 COL 1
               DISPLAY "(E)Busqueda por editorial" AT LINE 5 COL 1
               DISPLAY "Ingrese opcion( )"         AT LINE 7 COL 1
                                               WITH REVERSE-VIDEO
               DISPLAY
               "Presione ENTER sin ingresar datos para salir al menu"
                                                   AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
               ACCEPT BUSCAR-OPC AT LINE 7 COL 16

               IF BUSCAR-OPC EQUALS "i" MOVE "I" TO BUSCAR-OPC END-IF
               IF BUSCAR-OPC EQUALS "n" MOVE "N" TO BUSCAR-OPC END-IF
               IF BUSCAR-OPC EQUALS "e" MOVE "E" TO BUSCAR-OPC END-IF
               IF BUSCAR-OPC
                       NOT EQUALS ("I" AND "N") AND ("E" AND SPACES)
                    PERFORM UNTIL BUSCAR-OPC EQUALS "I" OR "N" OR "E"
                                                              OR SPACES
                       DISPLAY "ERROR, INTRODUZCA UNA OPCION VALIDA"
                                               AT LINE 7 COL 1 WITH
                                               BACKGROUND-COLOR 4
                       ACCEPT OPC                  AT LINE 7 COL 36
                       DISPLAY LIMPIAR-LINEA       AT LINE 7 COL 1
                       DISPLAY "Ingrese opcion( )" AT LINE 7 COL 1
                                                   WITH REVERSE-VIDEO
                       ACCEPT BUSCAR-OPC           AT LINE 7 COL 16
               IF BUSCAR-OPC EQUALS "i" MOVE "I" TO BUSCAR-OPC END-IF
               IF BUSCAR-OPC EQUALS "n" MOVE "N" TO BUSCAR-OPC END-IF
               IF BUSCAR-OPC EQUALS "e" MOVE "E" TO BUSCAR-OPC END-IF
                   END-PERFORM
               END-IF
               EVALUATE BUSCAR-OPC
               WHEN "I"
               PERFORM BUSCAR-ID
               WHEN "N"
               PERFORM BUSCAR-NOMBRE
               WHEN "E"
               PERFORM BUSCAR-EDITORIAL
               WHEN SPACES
               PERFORM CERRAR-ARCHIVO
               PERFORM MENU
               END-EVALUATE
           ELSE
               DISPLAY "ERROR, NO EXISTE UN ARCHIVO DE REGISTROS"
                                 AT LINE 1 COL 1 WITH BACKGROUND-COLOR 4
               ACCEPT OPC AT LINE 1 COL 43
           END-IF.
      *************************************************************************
      *BLOQUE DE CODIGO DE BUSQUEDAS
      *************************************************************************
       BUSCAR-ID.
           PERFORM LIMPIAR-PANTALLA.
           MOVE SPACES TO DATOS-LIBRO.
           DISPLAY "Codigo:   " AT LINE 2 COL 1 WITH REVERSE-VIDEO
           DISPLAY
           "-----------------------------------------------------"
                                               AT LINE 3 COL 1.

           DISPLAY
               "ID |TITULO            |EDITORIAL         |ESTADO     "
                                   WITH REVERSE-VIDEO AT LINE 4 COL 1.
           DISPLAY
           "Presione ENTER sin ingresar datos para salir al sub-menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
           ACCEPT LIBRO-ID AT LINE 2 COL 13 WITH PROMPT UNDERLINE UPDATE.

           MOVE LIBRO-ID TO AUX-ID

           EVALUATE LIBRO-ID
           WHEN ZERO
           PERFORM CERRAR-ARCHIVO
           PERFORM BUSCAR-LIBRO
           GO MENU
           WHEN NOT ZERO CONTINUE
           END-EVALUATE

           DISPLAY LIBRO-ID AT LINE 2 COL 13.
           READ REG-LIBROS END-READ.
           IF N-ESTA-REG
               DISPLAY "EL REGISTRO NO EXISTE" AT LINE 5 COL 1
                                               WITH BACKGROUND-COLOR 4
               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY "Presione ENTER para volver al menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
                        ACCEPT OPC AT LINE 10 COL 35
           ELSE


               PERFORM VARYING N FROM 01 BY 1 UNTIL FIN-REG OR
                                                   LIBRO-ID > AUX-ID
                   DISPLAY DATOS-LIBRO AT LINE 5 COL 1
                   READ REG-LIBROS NEXT RECORD END-READ
               END-PERFORM

               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY "Presione ENTER para volver al menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
               ACCEPT OPC AT LINE 10 COL 35
           END-IF.

           PERFORM CERRAR-ARCHIVO.

       BUSCAR-NOMBRE.
           PERFORM LIMPIAR-PANTALLA.
           INITIALIZE DATOS-LIBRO.
           DISPLAY "Titulo:   " AT LINE 2 COL 1 WITH REVERSE-VIDEO
           DISPLAY
           "-----------------------------------------------------"
                                               AT LINE 3 COL 1.

           DISPLAY
               "ID |TITULO            |EDITORIAL         |ESTADO     "
                                   WITH REVERSE-VIDEO AT LINE 4 COL 1.
           DISPLAY
           "Presione ENTER sin ingresar datos para volver al sub-menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
           ACCEPT AUX-NOMBRE AT LINE 2 COL 14.

           EVALUATE AUX-NOMBRE
           WHEN SPACES
           PERFORM CERRAR-ARCHIVO
           PERFORM BUSCAR-LIBRO
           GO MENU
           WHEN NOT SPACES CONTINUE
           END-EVALUATE.

           MOVE AUX-NOMBRE TO NOMBRE.

           READ REG-LIBROS KEY IS NOMBRE END-READ.

           IF N-ESTA-REG
               DISPLAY "EL REGISTRO NO EXISTE" AT LINE 5 COL 1
                                               WITH BACKGROUND-COLOR 4
               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY "Presione ENTER para volver al menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
                        ACCEPT OPC AT LINE 10 COL 35
           ELSE

               PERFORM VARYING N FROM 01 BY 1 UNTIL FIN-REG OR
                                                  NOMBRE > AUX-NOMBRE
                   DISPLAY DATOS-LIBRO AT LINE 5 COL 1
                   READ REG-LIBROS NEXT RECORD END-READ
               END-PERFORM
               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY "Presione ENTER para volver al menu"
                                                   AT LINE 10 COL 1
                                                   WITH REVERSE-VIDEO
                       ACCEPT OPC AT LINE 10 COL 35

           END-IF.
           PERFORM CERRAR-ARCHIVO.

       BUSCAR-EDITORIAL.
           PERFORM LIMPIAR-PANTALLA.
           INITIALIZE DATOS-LIBRO
           DISPLAY "Editorial:   " AT LINE 2 COL 1 WITH REVERSE-VIDEO
           DISPLAY
           "-----------------------------------------------------"
                                               AT LINE 3 COL 1.

           DISPLAY
               "ID |TITULO            |EDITORIAL         |ESTADO     "
                                   WITH REVERSE-VIDEO AT LINE 4 COL 1.
           DISPLAY
           "Presione ENTER sin ingresar datos para volver al sub-menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO

                           ACCEPT AUX-EDIT AT LINE 2 COL 16.

           EVALUATE AUX-EDIT
           WHEN SPACES
           PERFORM CERRAR-ARCHIVO
           PERFORM BUSCAR-LIBRO
           GO MENU
           WHEN NOT SPACES CONTINUE
           END-EVALUATE.

           MOVE AUX-EDIT TO EDITORIAL.
           READ REG-LIBROS KEY IS EDITORIAL END-READ.
           IF N-ESTA-REG
               DISPLAY "EL REGISTRO NO EXISTE" WITH BACKGROUND-COLOR 4
                                               AT LINE 5 COL 1
               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY "Presione ENTER para volver al menu"
                                               AT LINE 10 COL 1
                                               WITH REVERSE-VIDEO
                           ACCEPT OPC AT LINE 5 COL 22
           ELSE
               START REG-LIBROS KEY IS >= EDITORIAL
               READ REG-LIBROS NEXT RECORD END-READ

               PERFORM VARYING N FROM 05 BY 1 UNTIL FIN-REG OR
                                               EDITORIAL > AUX-EDIT

                   DISPLAY DATOS-LIBRO AT LINE N  COL 1
                   READ REG-LIBROS NEXT RECORD END-READ
               END-PERFORM
               ADD 1 TO N
               DISPLAY LIMPIAR-LINEA AT LINE 10 COL 1
               DISPLAY
           "Presione ENTER para volver al menu"
                                               AT LINE N COL 1
                                               WITH REVERSE-VIDEO
               ACCEPT OPC AT LINE N COL 35
           END-IF.
           PERFORM CERRAR-ARCHIVO.
      *************************************************************************
       LIMPIAR-PANTALLA.
           PERFORM VARYING N FROM 01 BY 1 UNTIL N>24
               DISPLAY LIMPIAR AT LINE N COLUMN 1
           END-PERFORM.

       ABRIR-ARCHIVO-INPUT.
           OPEN INPUT REG-LIBROS.

       ABRIR-IO-ARCHIVO.
           OPEN I-O REG-LIBROS.

       CERRAR-ARCHIVO.
           CLOSE REG-LIBROS.



      *ERRORES SOLUCIONADOS:
      ******************************************************************************************************************************************************
      *-OPCION 3 FUNCIONA MAL AL USAR FILE-STATUS
      *Aparentemente errores de recorrido de archivo
      ******************************************************************************************************************************************************
      *-LOOP INFINITO AL EJECUTAR OPCION 3 LUEGO DE CREAR ARCHIVO
      *el error era a causa de no haber cerrado el archivo en CREAR-ARCHIVO luego de haberlo abierto
      *haciendo que al entrar a la opcion 3 abriese nuevamente en modo I-O un archivo ya abierto
      *rompiendo el programa y generando un error de salida tipo 12.CERRAR SIEMPRE ARCHIVOS.
      ******************************************************************************************************************************************************
      *-NO ACUMULABA LOS DATOS INGRESADOS
      *era a causa de abrir el buffer como output en vez de i-o, open output elimina o sobreescribe el archivo
      *siempre que se trabaja con indexados para abrir el buffer es correcto utilizar OPEN I-O.
      ******************************************************************************************************************************************************
      *-DELETE STATEMENT no eliminaba
      *No habia abierto el archivo en modo I-O, usar un START y READ  siempre antes de usar DELETE
      ******************************************************************************************************************************************************
      *-ERROR DE LECTURA CON SECONDARY KEY
      *Recorria un archivo que fue declarado de una manera diferente al codigo mas actual, es decir
      *las claves secundarias no fueron declaradas cuando el archivo se creo y al intentar utilizar
      *esas llaves el compilador no las reconocia por lo tanto monstraba datos erroneos.
      *Se solucionó creando un nuevo archivo con las nuevas propiedades establecidas.
      ******************************************************************************************************************************************************
      *-ERRORES DE LECTURA AL USAR LAS KEY SECUNDARIAS
      *Parece ser que a menos que quiera recorrer de manera secuencial sease para buscar duplicas o recorrer un archivo de principio a fin,
      *por ejemplo, para mostrar un catalogo completo de libros etc.., no debo utilizar el comando START si busco un archivo especifico que
      *NO ESTA DUPLICADO con utilizar READ.....KEY IS secondary-key el programa lee correctamente y distingue entre registros existentes y no existentes
      *utilizando el FILE-STATUS para consultar su estado actual.
      *Se probó registrar un libro con el mismo nombre para duplicarlo pero no fue registrado claramente porque NOMBRE no esta declarada con WITH DUPLICATES
      *******************************************************************************************************************************************************
      *SE ROMPIA EL PROGRAMA EN SECCION 5 AL VOLVER AL SUB-MENU Y LUEGO BUSCAR UN LIBRO
      *Usaba perform para volver al submenu pero al retornar el control a la posicion de llamada intentaba leer el archivo estando cerrado, se soluciono
      *ingresando un GO TO MENU seguido de recuperar el control

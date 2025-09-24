              IDENTIFICATION DIVISION.
       PROGRAM-ID.                VENTAS-LOCAL.

       DATE-WRITTEN.              19/2/2023.
       AUTHOR.                    LUCAS GALEANO.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT REPORTE-VENTAS  ASSIGN TO "VENTAS.DAT"
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE  IS DYNAMIC
                                  RECORD KEY   IS ID-PRODUCTO.

           SELECT STOCK-MAESTRO   ASSIGN TO "STOCK-MAESTRO.DAT"
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE  IS DYNAMIC
                                  RECORD KEY   IS LLAVE-NUMERO-PRODUCTO.

           SELECT TICKET          ASSIGN TO "TICKET.DAT"
                                  ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.

       FD REPORTE-VENTAS.

       01 VENTA.
           05 LLAVE-VENTAS.
               10 ID-PRODUCTO     PIC 9(2).
           05 NOMBRE              PIC X(20).
           05 U-VENDIDAS          PIC 9(2) value zeros.
           05 PRECIO-BASE         PIC 9(4).

       FD STOCK-MAESTRO.

       01 LLAVE.
           05 LLAVE-NUMERO-PRODUCTO PIC 9(2).
       01 REG-PRODUCTOS.
           05 NOMBRE-PRO      PIC X(20).
           05 CANTIDAD-INICIAL PIC 9(3).
           05 ENTRADA         PIC 9(3).
           05 SALIDA          PIC 9(3).
           05 TOTAL           PIC 9(3).
           05 VALOR-VENTA     PIC 9(5).
       FD TICKET.

       01 TICKET-DATOS.
           05 TABLA OCCURS 100.
               10 CANTIDAD            PIC 9(2).
               10 NOMBRE-PRO-T        PIC X(20).
               10 PRECIO              PIC 9(5).
               10 TOTAL-T             PIC 9(5).
               10 RECIBIMOS           PIC 9(5).
               10 VUELTO              PIC 9(5).

       WORKING-STORAGE SECTION.

      *>***********************************************************************
      *>  *                         MENU INTERFAZ                             *
      *>***********************************************************************
       01 WS-TITULO.

           05 FILLER              PIC X(4)  VALUE "MENU".


       01 WS-OPC-1.
           05 FILLER              PIC X(36) VALUE SPACES.
           05 FILLER              PIC X(17) VALUE "1.REGISTRAR VENTA".
           05 FILLER              PIC X(36) VALUE SPACES.
       01 WS-OPC-2.
           05 FILLER              PIC X(36) VALUE SPACES.
           05 FILLER              PIC X(17) VALUE "2.PROCESAR TICKET".
           05 FILLER              PIC X(36) VALUE SPACES.
       01 WS-OPC-3.
           05 FILLER              PIC X(36) VALUE SPACES.
           05 FILLER              PIC X(17) VALUE "3.IMPRIMIR TICKET".
           05 FILLER              PIC X(36) VALUE SPACES.

       01 WS-OPC-0.
           05 FILLER              PIC X(36) VALUE SPACES.
           05 FILLER              PIC X(7)  VALUE "0.SALIR.".
           05 FILLER              PIC X(36) VALUE SPACES.

      *>***********************************************************************
      *>*                         TICKET                                      *
      *>***********************************************************************

       01 WS-COLUMNA1.
           05 FILLER              PIC X(10) VALUE SPACES.
           05 FILLER              PIC X(59) VALUE ALL "=".
           05 FILLER              PIC X(10) VALUE SPACES.



       01 WS-COLUMNA2.
           05 FILLER              PIC X(10) VALUE SPACES.
           05 FILLER              PIC X(59) VALUE ALL "=".
           05 FILLER              PIC X(10) VALUE SPACES.

       01 WS-LINEA-PRODUCTO.
           05 FILLER              PIC X(15) VALUE SPACES.
           05 WS-CANTIDAD         PIC 9(2).
           05 WS-NOMBRE-PRO       PIC X(20).
           05 FILLER              PIC X(30) VALUE SPACES.
           05 WS-PRECIO           PIC 9(5).
           05 FILLER              PIC X(15) VALUE SPACES.

       01 WS-LINEA-TOTAL.
           05 FILLER              PIC X(37).
           05 FILLER              PIC X(7) VALUE "TOTAL: ".
           05 WS-TOTAL            PIC 9(5).
           05 FILLER              PIC X(37).

       01 WS-LINEA-RECIBO.
           05 FILLER              PIC X(34).
           05 FILLER              PIC X(11) VALUE "RECIBIMOS: ".
           05 WS-RECIBIMOS        PIC 9(5).
           05 FILLER              PIC X(34).

       01 WS-LINEA-VUELTO.
           05 FILLER              PIC X(36).
           05 FILLER              PIC X(8) VALUE "VUELTO: ".
           05 WS-VUELTO           PIC 9(5).
           05 FILLER              PIC X(36).

       77 WS-TERMINAR-PROG        PIC X(2)  VALUE "NO".
       77 WS-CONTINUAR            PIC X(2)  VALUE "SI".
       77 WS-REG-EXISTE           PIC X     VALUE "T".
       77 WS-OPC                  PIC 9(10).
       77 DELINEADO               PIC X(10).
       77 WS-ID-AUX               PIC 9(2).
       77 WS-INDICE               PIC 99.
       77 TEXTO-LIMPIO            PIC X(80) VALUE SPACES.
       PROCEDURE DIVISION.

       010-MENU.
           OPEN I-O REPORTE-VENTAS.
           OPEN I-O STOCK-MAESTRO.
           OPEN OUTPUT TICKET.
           PERFORM LIMPIAR-PANTALLA
           PERFORM UNTIL WS-TERMINAR-PROG = "SI"
               DISPLAY "1.OPCION." AT LINE 4 COL 2
               WITH REVERSE-VIDEO
               DISPLAY "2.OPCION." AT LINE 6 COL 2
               WITH REVERSE-VIDEO
               DISPLAY "3.OPCION." AT LINE 8 COL 2
               WITH REVERSE-VIDEO
               DISPLAY "ERROR!!" WITH BACKGROUND-COLOR 4 AT LINE 9
               COL 2
               DISPLAY "EXITO!!" WITH BACKGROUND-COLOR 2 AT LINE 10
               COL 2
               DISPLAY "RESALTAR" WITH HIGHLIGHT AT LINE 11 COL 2

               ACCEPT DELINEADO AT LINE 14 COL 2 WITH PROMPT UNDERLINE
               UPDATE
               ACCEPT WS-OPC AT LINE 15 COL 2 WITH BEEP PROMPT UNDERLINE
               UPDATE
           EVALUATE WS-OPC
               WHEN 1 PERFORM 020-REGISTRAR-VENTA
      *>         WHEN 2 PERFORM PROCESAR-TICKET
      *>         WHEN 3 PERFORM IMPRIMIR-TICKET
               WHEN 0 MOVE "SI" TO WS-TERMINAR-PROG
           END-EVALUATE
           END-PERFORM.
           CLOSE REPORTE-VENTAS.
           CLOSE STOCK-MAESTRO.
           CLOSE TICKET.
           STOP RUN.

       020-REGISTRAR-VENTA.

           DISPLAY "INGRESE ID DEL PRODUCTO: " WITH NO ADVANCING
           ACCEPT LLAVE-NUMERO-PRODUCTO
           READ STOCK-MAESTRO
               INVALID KEY MOVE "F" TO WS-REG-EXISTE
           END-READ
           IF WS-REG-EXISTE = "F" THEN
               DISPLAY " "
               DISPLAY "PRODUCTO FUERA DE STOCK."
           ELSE
               DISPLAY " "
               DISPLAY "PRODUCTO: " WITH NO ADVANCING
               DISPLAY NOMBRE-PRO   WITH NO ADVANCING
               DISPLAY "STOCK: "    WITH NO ADVANCING
               DISPLAY TOTAL        WITH NO ADVANCING
               DISPLAY "PRECIO: "   WITH NO ADVANCING
               DISPLAY VALOR-VENTA
               DISPLAY " "
               DISPLAY "INGRESE EL MES ACTUAL: " WITH NO ADVANCING
               DISPLAY " "
               initialize venta
               MOVE LLAVE-NUMERO-PRODUCTO TO ID-PRODUCTO
               MOVE NOMBRE-PRO TO NOMBRE
               MOVE ID-PRODUCTO TO WS-ID-AUX

               IF ID-PRODUCTO EQUALS WS-ID-AUX THEN
                   ADD 1 TO U-VENDIDAS GIVING U-VENDIDAS
               ELSE
                   MOVE ID-PRODUCTO TO WS-ID-AUX
                   ADD 1 TO U-VENDIDAS giving U-VENDIDAS
               END-IF
           END-IF.
           write VENTA.
            display VENTA.

       LIMPIAR-PANTALLA.

           PERFORM VARYING WS-INDICE FROM 01 BY 1 UNTIL WS-INDICE >
                                                        24
              DISPLAY TEXTO-LIMPIO AT LINE WS-INDICE COL 1
              END-PERFORM.






      *>COMENTARIOS:
      *>el uso de initialize me permite hacer move de los IDS y evitar el error "not numeric"
      *>se puede "sumar" u-vendidas.

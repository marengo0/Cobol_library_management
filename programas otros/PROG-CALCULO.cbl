       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 PROG-CALCULOS.
       AUTHOR.                    LUCAS GALEANO.
       DATE-WRITTEN.              24/1/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RES ASSIGN TO "RESULTADOS.DAT"
           ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD RES.
       01 RESULTADOS.
           02 SUMA                PIC 9(7)V99.
           02 RESTA               PIC 9(7)V99.
           02 DIV                 PIC 9(4)V99.
           02 MULTIPLICACION      PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01 WS-NUM1                 PIC 9(7)V99  VALUE ZEROS.
       01 WS-NUM2                 PIC 9(7)V99  VALUE ZEROS.
       01 WS-RES                  PIC 9(7)V99  VALUE ZEROS.
       PROCEDURE DIVISION.
       BEGIN-OUTPUT.
           OPEN OUTPUT RES.
           PERFORM GETRESULTS.
               WRITE RESULTADOS.
               DISPLAY "RESULTADOS:".
               DISPLAY "SUMA: ",SUMA.
               DISPLAY "RESTA: ",RESTA.
               DISPLAY "DIVISION: ",DIV.
               DISPLAY "MULTIPLICACION: ",MULTIPLICACION.
           CLOSE RES.
           STOP RUN.
      *-----------------------------END  OUTPUT---------------------*


       GETRESULTS.
               DISPLAY "ENTER TWO VALUES TO ADDITION"
               ACCEPT WS-NUM1.
               ACCEPT WS-NUM2.
               ADD WS-NUM1 TO WS-NUM2 GIVING SUMA.
               DISPLAY "ENTER TWO VALUES TO SUBTRACT"
               ACCEPT WS-NUM1.
               ACCEPT WS-NUM2.
               SUBTRACT WS-NUM2 FROM WS-NUM1 GIVING RESTA.
               DISPLAY "ENTER TWO VALUES TO MULTIPLICATE"
               ACCEPT WS-NUM1.
               ACCEPT WS-NUM2.
               MULTIPLY WS-NUM1 BY WS-NUM2 GIVING MULTIPLICACION.
               DISPLAY "ENTER TWO VALUES TO DIVIDE".
               ACCEPT WS-NUM1.
               ACCEPT WS-NUM2.
               DIVIDE WS-NUM1 BY WS-NUM2 GIVING DIV.
               END PROGRAM PROG-CALCULOS.

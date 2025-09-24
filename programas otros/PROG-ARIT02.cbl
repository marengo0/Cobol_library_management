       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PROG-ARIT02.
       AUTHOR.                    LUCAS GALEANO.
       DATE-WRITTEN.               25/1/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RES ASSIGN "RESULTADOS.DAT"
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
       01 WS-SUMA                 PIC 9(7)V99.
       01 WS-RESTA                PIC 9(7)V99.
       01 WS-DIV                  PIC 9(7)V99.
       01 WS-MULT                 PIC 9(7)V99.

       PROCEDURE DIVISION.
       BEGIN-INPUT.
           OPEN INPUT RES.
           READ RES.
           DISPLAY "THE RESULTS FROM PROG-ARIT WILL BE USED ON".
           DISPLAY "THE RESULTS BELOW."
           ADD 5 TO SUMA GIVING   WS-SUMA.
           ADD -3 TO RESTA GIVING WS-RESTA.
           ADD 2 TO DIV GIVING    WS-DIV.
           ADD 6 TO MULTIPLICACION GIVING WS-MULT.

           DISPLAY " ".
           DISPLAY "SUMA:  ",     WS-SUMA.
           DISPLAY "RESTA: ",     WS-RESTA.
           DISPLAY "DIVISION:  ", WS-DIV.
           DISPLAY "MULTIPLICACION: ",WS-MULT.

           CLOSE RES.
           STOP RUN.
           END PROGRAM PROG-ARIT02.

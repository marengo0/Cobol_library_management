      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 TITULO.
           05 PRIMERO.
           10 FILLER            PIC X(34).
           10 TEXTO               PIC X(11) VALUE "HELLO WORLD".
           10 TEXTO2              PIC X(4)  VALUE "HOLA".
           10 FILLER             PIC X(43).

           05 SEGUNDO.
           10 FILLER             PIC X(34) VALUE SPACES.
           10 TEXTO2              PIC X(4)  VALUE "HOLA".
           10 FILLER             PIC X(43) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

            DISPLAY PRIMERO
            DISPLAY SEGUNDO
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.

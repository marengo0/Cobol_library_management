
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 c   pic 9.
       01 i   pic 9 value zero.
       01 arreglo.
           05 numeros occurs 5 times.
              10 n    PIC 9.
              10 l    Pic x.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           perform varying i from 1 by 1 until i > 5
           add 1 to c
           move c to n(i)
           move "F" to l(i)
           end-perform.
           display arreglo.
            STOP RUN.

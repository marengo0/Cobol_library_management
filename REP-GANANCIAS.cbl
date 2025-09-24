       IDENTIFICATION DIVISION.
       PROGRAM-ID.                REP-GANANCIAS.
       DATE-WRITTEN.              18/2/2023.
       AUTHOR.                    LUCAS GALEANO.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTE ASSIGN TO "GANANCIAS-LOCAL.DAT"
                          ORGANIZATION IS INDEXED
                          ACCESS MODE IS DYNAMIC
                          RECORD KEY IS MES.

           SELECT

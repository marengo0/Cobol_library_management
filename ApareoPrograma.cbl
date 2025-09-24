       IDENTIFICATION DIVISION.
       PROGRAM-ID.     APAREO_PRUEBA.
       AUTHOR          LUCAS GALEANO.
       DATE-WRITTEN    21/8/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NUMEROS_LISTA_1 ASSIGN TO "ARCHIVO_1.DAT"
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS NUM1
                                  ACCESS MODE IS DYNAMIC.
           SELECT NUMEROS_LISTA_2 ASSIGN TO "ARCHIVO_2.DAT"
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS NUM2
                                  ACCESS MODE IS DYNAMIC.
           SELECT NUMEROS_LISTA_3 ASSIGN TO "ARCHIVO_3.DAT"
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS NUM3
                                  ACCESS MODE IS DYNAMIC.
       DATA DIVISION.
       FILE SECTION.

       FD NUMEROS_LISTA_1.
       01 NUMEROS_1.
           05 NUM1            PIC 9.

       FD NUMEROS_LISTA_2.
       01 NUMEROS_2.
           05 NUM2            PIC 9.

       FD NUMEROS_LISTA_3.
       01 NUMEROS_3.
           05 NUM3            PIC 9.

       WORKING-STORAGE SECTION.
       77 N                   PIC 9.
       77 STOPP               PIC 9.

       PROCEDURE DIVISION.

       DISPLAY "INGRESE UN NUMERO DEL 1 AL 9,FINALIZE CON UN 0".
       PERFORM ESCRIBIR-ARCHIVO-1.
       PERFORM ESCRIBIR-ARCHIVO-2.
       PERFORM APAREAMIENTO-PROCEDURE.
       PERFORM RESULTADOS.
       STOP RUN.


       ESCRIBIR-ARCHIVO-1.
           OPEN OUTPUT NUMEROS_LISTA_1.
           INITIALIZE NUMEROS_1.
           ACCEPT NUM1.
           PERFORM UNTIL NUM1 EQUALS 0
               WRITE NUMEROS_1
               ACCEPT NUM1
           END-PERFORM.
           CLOSE NUMEROS_LISTA_1.


       ESCRIBIR-ARCHIVO-2.
           OPEN OUTPUT NUMEROS_LISTA_2.
           INITIALIZE NUMEROS_2.
           ACCEPT NUM2.
           PERFORM UNTIL NUM2 EQUALS 0
               WRITE NUMEROS_2
               ACCEPT NUM2
           END-PERFORM.
           CLOSE NUMEROS_LISTA_2.


       APAREAMIENTO-PROCEDURE.
           OPEN INPUT NUMEROS_LISTA_1.
           OPEN INPUT NUMEROS_LISTA_2.
           OPEN OUTPUT NUMEROS_LISTA_3.
           INITIALIZE NUMEROS_1 NUMEROS_2 NUMEROS_3.
           READ NUMEROS_LISTA_1 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_1
           END-READ.
           READ NUMEROS_LISTA_2 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_2
           END-READ.
           PERFORM VARYING N FROM 01 BY 1 UNTIL NUMEROS_1 EQUALS
                                       HIGH-VALUES OR NUMEROS_2 EQUALS
                                       HIGH-VALUES
               IF NUM1 EQUALS NUM2 THEN
                   MOVE NUM1 TO NUM3
                   WRITE NUMEROS_3
                   READ NUMEROS_LISTA_1 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_1
                   END-READ
                   READ NUMEROS_LISTA_2 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_2
                   END-READ
               ELSE IF NUM1 < NUM2 THEN
                   READ NUMEROS_LISTA_1 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_1
                   END-READ
               ELSE
                   READ NUMEROS_LISTA_2 NEXT RECORD
                                   AT END MOVE HIGH-VALUES TO NUMEROS_2
                   END-READ
               END-IF
           END-PERFORM.
           CLOSE NUMEROS_LISTA_1 NUMEROS_LISTA_2 NUMEROS_LISTA_3.


       RESULTADOS.
           OPEN INPUT NUMEROS_LISTA_3.
           INITIALIZE NUMEROS_3
           DISPLAY "CONTENIDOS DEL ARCHIVO ACTUALIZADO:"
           READ NUMEROS_LISTA_3 AT END MOVE HIGH-VALUES TO NUMEROS_3
           END-READ.
           PERFORM VARYING N FROM 01 BY 1 UNTIL
                                   NUMEROS_3 EQUALS HIGH-VALUES
           DISPLAY NUMEROS_3
           READ NUMEROS_LISTA_3 AT END MOVE HIGH-VALUES TO NUMEROS_3
           END-READ
           END-PERFORM.
           ACCEPT STOPP.
           CLOSE NUMEROS_LISTA_3.

       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PROG-PAGOS-F.
       AUTHOR.                    LUCAS GALEANO.
       DATE-WRITTEN.              1/2/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REG-VENDEDORES ASSIGN TO "REG-MAESTRO.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT REG-VENTAS     ASSIGN TO "REG-VENTAS-MAESTRO.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD REG-VENDEDORES.
       01 VENDEDORES              PIC 9(14).

       FD REG-VENTAS.
       01 COB-VENTAS.
           05 cobros occurs 100 times.
               10 NOMBRE pic x(5).

       WORKING-STORAGE SECTION.
       01 WS-VENDEDORES.
           05 WS-EMPLEADO            PIC 999.
           05 WS-SUELDO-BASE         PIC 9(5).
           05 WS-CANTIDAD-VENTAS     PIC 999.
       01 esto-deberia-estar-fuera.
           05 WS-TOTAL-EMPLEADOS     PIC 999.
           05 WS-COBRO-VENTAS        PIC 9(5).
       01 WS-TABLAS.
           05 WS-REGISTRO-COBROS     PIC 9(5)
                                     OCCURS 100 TIMES.
       77 CONTADOR                   PIC 99 VALUE ZEROS.
       77 indice                  PIC 999.

       PROCEDURE DIVISION.
       BEGIN-OUTPUT.
           OPEN OUTPUT REG-VENDEDORES.
           open output reg-ventas.
           DISPLAY "INGRESE DATOS SOLICITADOS".
           DISPLAY "INGRESE CANTIDAD TOTAL DE EMPLEADOS:".
           ACCEPT WS-TOTAL-EMPLEADOS.
           PERFORM VARYING CONTADOR FROM 1 BY 1
                           UNTIL CONTADOR > WS-TOTAL-EMPLEADOS
             PERFORM INGRESO-DATOS-EMPLEADOS
             WRITE VENDEDORES FROM WS-VENDEDORES
             write cob-ventas end-write
           END-PERFORM.
           CLOSE  REG-VENDEDORES reg-ventas.
           STOP RUN.

       INGRESO-DATOS-EMPLEADOS.

           DISPLAY "INGRESE NUMERO DE EMPLEADO:".
           ACCEPT WS-EMPLEADO.
           DISPLAY "INGRESE SUELDO BASE:".
           ACCEPT WS-SUELDO-BASE.
           DISPLAY "INGRESE CANTIDAD DE VENTAS REALIZADAS:".
           ACCEPT WS-CANTIDAD-VENTAS.
           initialize cob-ventas.
           PERFORM VARYING indice FROM 1 BY 1
                           UNTIL indice > WS-CANTIDAD-VENTAS
                                 or indice = 100
              DISPLAY "INGRESE COBRO DE CADA VENTA:"
              ACCEPT WS-COBRO-VENTAS
              MOVE WS-COBRO-VENTAS TO cobros(indice)
           END-PERFORM.

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
       01 VENDEDORES.
           88 FIN-DEL-ARCHIVO        VALUE HIGH-VALUE.
           05 EMPLEADO               PIC 9(2).
           05 SUELDO-BASE            PIC 9(5).
           05 CANTIDAD-VENTAS        PIC 9.
           05 TOTAL-RECAUDACION      PIC 9(5) VALUE ZEROS.


       FD REG-VENTAS.
       01 COBRO-VENTAS.
           05 VENDEDOR              PIC 9(2) OCCURS 20 TIMES.

       WORKING-STORAGE SECTION.
       01 WS-VENDEDORES.
           05 WS-EMPLEADO            PIC 9(2).
           05 WS-SUELDO-BASE         PIC 9(5).
           05 WS-CANTIDAD-VENTAS     PIC 9.
           05 WS-TOTAL-RECAUDACION   PIC 9(5).


       77 WS-PRECIO-PRODUCTO         PIC 9(5).
       77 WS-TOTAL-EMPLEADOS         PIC 99.
       77 WS-CONTADOR                PIC 99 VALUE ZEROS.
       77 WS-INDICE                  PIC 99.
       77 WS-SUELDO-FINAL            PIC 9(6).



       PROCEDURE DIVISION.
       GO TO BEGIN-INPUT.
       BEGIN-OUTPUT.
           OPEN OUTPUT REG-VENDEDORES.
           OPEN OUTPUT REG-VENTAS.
           DISPLAY "INGRESE DATOS SOLICITADOS".
           DISPLAY " ".
           DISPLAY "INGRESE TOTAL DE EMPLEADOS DE LA COMPANIA".
           ACCEPT WS-TOTAL-EMPLEADOS.
           PERFORM VARYING WS-CONTADOR FROM 01 BY 1
                                 UNTIL WS-CONTADOR > WS-TOTAL-EMPLEADOS
               PERFORM INGRESO-DATOS
               WRITE VENDEDORES FROM WS-VENDEDORES
               WRITE COBRO-VENTAS
               END-WRITE
               MOVE ZEROS TO WS-TOTAL-RECAUDACION
           END-PERFORM.
           CLOSE REG-VENDEDORES.
           CLOSE REG-VENTAS.


       BEGIN-INPUT.
           OPEN INPUT REG-VENDEDORES.
           OPEN INPUT REG-VENTAS.
           READ REG-VENDEDORES AT END SET FIN-DEL-ARCHIVO TO TRUE
           END-READ.
           PERFORM VARYING WS-INDICE FROM 01 BY 1
                             UNTIL VENDEDOR(WS-INDICE) = EMPLEADO
                                   OR FIN-DEL-ARCHIVO
           ADD SUELDO-BASE TO TOTAL-RECAUDACION GIVING WS-SUELDO-FINAL
           DISPLAY "====================================="
           DISPLAY "RECAUDACION DE LA SEMANA DE EMPLEADO ",EMPLEADO
                                  " ES: ",TOTAL-RECAUDACION
           DISPLAY "====================================="

           DISPLAY "SALDO TOTAL A PAGAR: ", WS-SUELDO-FINAL
           READ REG-VENDEDORES AT END SET FIN-DEL-ARCHIVO TO TRUE
           END-READ
           END-PERFORM.
           CLOSE REG-VENDEDORES REG-VENTAS.
           STOP RUN.

       INGRESO-DATOS.
           DISPLAY "INGRESE NUMERO DE EMPLEADO:".
           ACCEPT WS-EMPLEADO.
           DISPLAY "INGRESE SUELDO BASE:".
           ACCEPT WS-SUELDO-BASE.
           DISPLAY "INGRESE CANTIDAD DE VENTAS REALIZADAS:".
           ACCEPT WS-CANTIDAD-VENTAS.
           INITIALIZE COBRO-VENTAS
              PERFORM VARYING WS-INDICE FROM 01 BY 1 UNTIL
                           WS-INDICE > WS-CANTIDAD-VENTAS
           DISPLAY "PRECIO DEL PRODUCTO:"
           ACCEPT WS-PRECIO-PRODUCTO
           ADD WS-PRECIO-PRODUCTO    TO WS-TOTAL-RECAUDACION
           END-PERFORM.
           MOVE WS-EMPLEADO          TO VENDEDOR(WS-INDICE).


      *PASAR LA RECAUDACION DE LA SEMANA AL BUFFER ASI EVITAR INCOHERENCIAS EN LOS INDICES DE TABLAS!!





      *Una compañía de seguros tiene contratados a n vendedores. Cada vendedor realiza
      *múltiples ventas a la semana. La política de pagos de la compañía es que cada vendedor
      *recibe un sueldo base más un 10% extra por comisiones de sus ventas. El gerente de la
      *compañía desea saber, por un lado, cuánto dinero deberá pagar en la semana a cada
      *vendedor por concepto de comisiones de las ventas realizadas, y por otro lado, cuánto
      *deberá pagar a cada vendedor como sueldo total (sueldo base + comisiones). Para cada
      *vendedor ingresar cuanto es su sueldo base, cuantas ventas realizó y cuanto cobró por
      *cada venta.

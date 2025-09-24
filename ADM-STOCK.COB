       IDENTIFICATION DIVISION.

       PROGRAM-ID.                ADM-STOCK.
       DATE-WRITTEN.              10/2/2023.
       AUTHOR.                    LUCAS GALEANO.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STOCK-MAESTRO ASSIGN TO "STOCK-MAESTRO.DAT"
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS LLAVE-NUMERO-PRODUCTO
                                  ACCESS MODE IS DYNAMIC.
       DATA DIVISION.

       FILE SECTION.

       FD STOCK-MAESTRO.

       01 REG-PRODUCTOS.
           05 LLAVE-NUMERO-PRODUCTO PIC XX.
           05 PRODUCTO.
               10 NOMBRE-PRODUCTO PIC X(20).
               10 CANTIDAD-INICIAL PIC 9(3).
               10 ENTRADA         PIC 9(3).
               10 SALIDA          PIC 9(3).
               10 TOTAL           PIC 9(3).
               10 VALOR-VENTA     PIC 9(5).

       WORKING-STORAGE SECTION.
       77 TERMINAR-PROG           PIC XX VALUE "NO".
       77 OPCION                  PIC 9.
       77 REG-EXISTE              PIC X.

       PROCEDURE DIVISION.

       010-MENU.
           OPEN I-O STOCK-MAESTRO.
           DISPLAY "ADMINISTRAR STOCK".
           DISPLAY "=================".
           PERFORM UNTIL TERMINAR-PROG = "SI"
               DISPLAY " "
               DISPLAY "1-MOSTRAR REGISTRO DE STOCK"
               DISPLAY "2-ACTUALIZAR REGISTRO"
               DISPLAY "3-INGRESAR REGISTRO"
               DISPLAY "4-BUSCAR REGISTRO"
               DISPLAY "0-SALIR"
               DISPLAY ":" WITH NO ADVANCING
           ACCEPT OPCION
           EVALUATE OPCION
               WHEN 1 PERFORM 020-MOSTRAR-REG
               WHEN 2 PERFORM 030-ACTUALIZAR-REG
               WHEN 3 PERFORM 040-INGRESAR-REG
               WHEN 4 PERFORM 050-BUSCAR-REG
               WHEN 0 MOVE "SI" TO TERMINAR-PROG
           END-EVALUATE
           END-PERFORM.
           CLOSE STOCK-MAESTRO.
           STOP RUN.
       010-FIN.

       020-MOSTRAR-REG.
           MOVE LOW-VALUES TO LLAVE-NUMERO-PRODUCTO.
           START STOCK-MAESTRO KEY NOT < LLAVE-NUMERO-PRODUCTO
                                         END-START.
           READ STOCK-MAESTRO AT END MOVE HIGH-VALUES TO REG-PRODUCTOS
           END-READ.
           PERFORM UNTIL REG-PRODUCTOS = HIGH-VALUES
           DISPLAY REG-PRODUCTOS
           READ STOCK-MAESTRO AT END MOVE HIGH-VALUES TO REG-PRODUCTOS
           END-READ
           END-PERFORM.
       020-FIN.

       030-ACTUALIZAR-REG.
           MOVE "S" TO REG-EXISTE.
           DISPLAY " ".
           DISPLAY "INGRESE ID DEL PRODUCTO: " WITH NO ADVANCING.
           ACCEPT LLAVE-NUMERO-PRODUCTO.
           READ STOCK-MAESTRO
               INVALID KEY  MOVE "N" TO REG-EXISTE
           END-READ.
           IF REG-EXISTE = "N" THEN
               DISPLAY " "
               DISPLAY "EL PRODUCTO NO EXISTE."
           ELSE
               DISPLAY " "
               DISPLAY "INGRESE NUEVOS DATOS SIGUIENDO LA PLANTILLA:"
               DISPLAY " "
               DISPLAY "PPPPPPPPPPPPIIIEEESSSTTT$$$$$"
               ACCEPT PRODUCTO
           END-IF.
           REWRITE REG-PRODUCTOS
               INVALID KEY DISPLAY "REGISTRO NO ACTUALIZADO."
           END-REWRITE.
       0-30-FIN.

       040-INGRESAR-REG.
           DISPLAY " ".
           DISPLAY "INGRESE DATOS DEL PRODUCTO USANDO LA PLANTILLA:".
           DISPLAY "NNPPPPPPPPPPPPPPPPPPPPIIIEEESSSTTT$$$$$".
           ACCEPT REG-PRODUCTOS.
           DISPLAY " ".
           WRITE REG-PRODUCTOS
               INVALID KEY DISPLAY "ID YA EXISTE."
           END-WRITE.
       040-FIN.

       050-BUSCAR-REG.
           MOVE "S" TO REG-EXISTE.
           DISPLAY "INGRESE ID DEL PRODUCTO QUE DESEA BUSCAR:"
                                  WITH NO ADVANCING.
           ACCEPT LLAVE-NUMERO-PRODUCTO.
           READ STOCK-MAESTRO
               INVALID KEY  MOVE "N" TO REG-EXISTE
           END-READ.
           IF REG-EXISTE = "N" THEN
               DISPLAY "EL PRODUCTO NO EXISTE."
           ELSE
               DISPLAY REG-PRODUCTOS.
       050-FIN.
       END PROGRAM ADM-STOCK.

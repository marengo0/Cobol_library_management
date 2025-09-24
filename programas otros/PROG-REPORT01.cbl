       IDENTIFICATION DIVISION.
       PROGRAM-ID.                PROG-REPORT01.
       AUTHOR.                    LUCAS GALEANO.
       DATE-WRITTEN.              28/1/2023.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-EMPLOYEES ASSIGN TO "EMPLOYEE.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MASTER-EMPLOYEES.
       01 EMPLOYEE-DETAILS.
           05 EMP-NUMBER          PIC 9(5).
           05 EMP-IDE.
               10 EMP-SUR         PIC X(15).
               10 EMP-NAME        PIC X(15).
           05 EMP-ID              PIC 9(8).
           05 EMP-GENDER          PIC X.
           05 SALARY              PIC 9(6).
       WORKING-STORAGE SECTION.
       77 TOTAL-EMP               PIC 9(4).
       77 TOTAL-SALARY            PIC 9(9)V99.
       77 COUNTER-M               PIC 9(3).
       77 COUNTER-F               PIC 9(3).

       PROCEDURE DIVISION.

       BEGIN-EMP-INPUT.
       OPEN INPUT MASTER-EMPLOYEES.
           READ MASTER-EMPLOYEES
               AT END MOVE HIGH-VALUES TO EMPLOYEE-DETAILS
           END-READ.
           PERFORM UNTIL EMPLOYEE-DETAILS = HIGH-VALUES
               ADD 1 TO TOTAL-EMP
               ADD SALARY TO TOTAL-SALARY
               IF EMP-GENDER = "M"
                   ADD 1 TO COUNTER-M
                   ELSE
                   ADD 1 TO COUNTER-F
               END-IF
               DISPLAY EMP-NUMBER SPACE EMP-IDE SPACE EMP-ID SPACE EMP-GENDER SPACE SALARY
               READ MASTER-EMPLOYEES
                   AT END MOVE HIGH-VALUES TO EMPLOYEE-DETAILS
               END-READ
           END-PERFORM.
               CLOSE MASTER-EMPLOYEES.

           DISPLAY " ".
           DISPLAY "TOTAL NUMBER OF EMPLOYEES: ",TOTAL-EMP.
           DISPLAY "TOTAL NUMBER OF MALE EMPLOYEES: ",COUNTER-M.
           DISPLAY "TOTAL NUMBER OF FEMALE EMPLOYEES: ",COUNTER-F.
           DISPLAY "TOTAL SALARIES: ",TOTAL-SALARY.
           STOP RUN.
           END PROGRAM PROG-REPORT01.

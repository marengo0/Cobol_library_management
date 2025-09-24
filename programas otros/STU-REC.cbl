       IDENTIFICATION DIVISION.
       PROGRAM-ID.                STU-REC.
       AUTHOR.                    LUCAS GALEANO.
       DATE-WRITTEN.              21/1/2023.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.           IBM-3083.
       OBJECT-COMPUTER.           IBM-3083.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENTS-RECORD ASSIGN TO "STUDENTS.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD STUDENTS-RECORD.
       01 STUDENT-DETAILS.
           02 STUDENT-ID          PIC 9(8).
           02 STUDENT-NAME.
              03 SURNAME          PIC X(8).
              03 NAM              PIC X(8).
           02 DATE-OF-BIRTH.
              03 YO-BIRTH         PIC 9999.
              03 MO-BIRTH         PIC 99.
              03 DO-BIRTH         PIC 99.
           02 COURSE-CODE         PIC X(4).
           02 GRADES              PIC 99.
           02 GENDER              PIC X.
       WORKING-STORAGE SECTION.
       01 WS-F-COUNTER            PIC 9.
       01 WS-M-COUNTER            PIC 9.

       PROCEDURE DIVISION.

       BEGIN-OUTPUT.
           OPEN EXTEND STUDENTS-RECORD.
           DISPLAY "INTRODUCE STUDENT DETAILS, END INTRODUCING NO DATA".
           PERFORM GETSTUDENT-DETAILS.
           PERFORM UNTIL STUDENT-DETAILS = SPACES
               WRITE STUDENT-DETAILS
               PERFORM GETSTUDENT-DETAILS
           END-PERFORM.
           CLOSE STUDENTS-RECORD.
      *----------------------END OF OUTPUT----------------------------*
       BEGIN-INPUT.
           OPEN INPUT STUDENTS-RECORD.
           READ STUDENTS-RECORD
                  AT END MOVE HIGH-VALUES TO STUDENT-DETAILS
           END-READ
           PERFORM UNTIL STUDENT-DETAILS = HIGH-VALUES
           IF GENDER = "F"
               ADD 1 TO WS-F-COUNTER
               ELSE
                   ADD 1 TO WS-M-COUNTER
               END-IF
           DISPLAY STUDENT-ID SPACE STUDENT-NAME SPACE GENDER
               READ STUDENTS-RECORD
                  AT END MOVE HIGH-VALUES TO STUDENT-DETAILS
               END-READ
           END-PERFORM
           DISPLAY " ".
           DISPLAY "NUMBER OF MALE STUDENTS: ",WS-M-COUNTER.
           DISPLAY "NUMBER OF FEMALE STUDENTS: ",WS-F-COUNTER.
           CLOSE STUDENTS-RECORD
           STOP RUN.
      *---------------------END OF INPUT------------------------------*
           GETSTUDENT-DETAILS.
           DISPLAY "IIIIIIIISSSSSSSSNNNNNNNNYYYYMMDDCCCCGGS".
           ACCEPT STUDENT-DETAILS.
           END PROGRAM STU-REC.

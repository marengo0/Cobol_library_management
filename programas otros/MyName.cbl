       IDENTIFICATION DIVISION.
       PROGRAM-ID. MyNameIs.
       AUTHOR Lucas G.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MyName PIC X(6) VALUE SPACES.
       PROCEDURE DIVISION.
           MOVE "Lucas" TO Myname.
           DISPLAY "My name is ",MyName.
           STOP RUN.

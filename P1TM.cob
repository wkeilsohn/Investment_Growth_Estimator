       IDENTIFICATION DIVISION.
       PROGRAM-ID. P1TM.
       AUTHOR. WILLIAM-KEILSOHN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *01 MONTH_NUMBERS.
      *  05 M1 PIC 9(2) VALUE 1.
      *  05 M2 PIC 9(2) VALUE 2.
      *  05 M3 PIC 9(2) VALUE 3.
      *  05 M4 PIC 9(2) VALUE 4.
      *  05 M5 PIC 9(2) VALUE 5.
      *  05 M6 PIC 9(2) VALUE 6.
      *  05 M7 PIC 9(2) VALUE 7.
      *  05 M8 PIC 9(2) VALUE 8.
      *  05 M9 PIC 9(2) VALUE 9.
      *  05 M10 PIC 9(2) VALUE 10.
      *  05 M11 PIC 9(2) VALUE 11.
      *  05 M12 PIC 9(2) VALUE 12.
       01 MONTH_NAMES.
         05 MN1 PIC X(7) VALUE 'JANUARY'.
         05 MN2 PIC X(8) VALUE 'FEBRUARY'.
         05 MN3 PIC X(5) VALUE 'MARCH'.
         05 MN4 PIC X(5) VALUE 'APRIL'.
         05 MN5 PIC X(3) VALUE 'MAY'.
         05 MN6 PIC X(4) VALUE 'JUNE'.
         05 MN7 PIC X(4) VALUE 'JULY'.
         05 MN8 PIC X(6) VALUE 'AUGUST'.
         05 MN9 PIC X(9) VALUE 'SEPTEMBER'.
         05 MN10 PIC X(7) VALUE 'OCTOBER'.
         05 MN11 PIC X(8) VALUE 'NOVEMBER'.
         05 MN12 PIC X(8) VALUE 'DECEMBER'.

       LINKAGE SECTION.
       01 RECIEVED-MONTH-NUMBER PIC 9(2).
       01 RETURNED-MONTH-NAME PIC X(10).

       PROCEDURE DIVISION USING RECIEVED-MONTH-NUMBER, 
       RETURNED-MONTH-NAME.

           EVALUATE RECIEVED-MONTH-NUMBER
               WHEN 1
                   MOVE MN1 TO RETURNED-MONTH-NAME
               WHEN 2
                   MOVE MN2 TO RETURNED-MONTH-NAME
               WHEN 3
                   MOVE MN3 TO RETURNED-MONTH-NAME
               WHEN 4
                   MOVE MN4 TO RETURNED-MONTH-NAME
               WHEN 5
                   MOVE MN5 TO RETURNED-MONTH-NAME
               WHEN 6
                   MOVE MN6 TO RETURNED-MONTH-NAME
               WHEN 7
                   MOVE MN7 TO RETURNED-MONTH-NAME
               WHEN 8
                   MOVE MN8 TO RETURNED-MONTH-NAME
               WHEN 9
                   MOVE MN9 TO RETURNED-MONTH-NAME
               WHEN 10
                   MOVE MN10 TO RETURNED-MONTH-NAME
               WHEN 11
                   MOVE MN11 TO RETURNED-MONTH-NAME
               WHEN 12
                   MOVE MN12 TO RETURNED-MONTH-NAME
               WHEN OTHER
                   MOVE "ERROR" TO RETURNED-MONTH-NAME
           END-EVALUATE
           .
           
           EXIT PROGRAM
           .

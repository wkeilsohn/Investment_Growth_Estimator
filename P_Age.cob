       IDENTIFICATION DIVISION.
       PROGRAM-ID. P_Age.
       AUTHOR. WILLIAM-KEILSOHN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 YEAR_DIFF PIC 9(3) VALUE 0.

       LINKAGE SECTION.
       01 TDAY PIC 9(2).
       01 TMONTH PIC 9(2).
       01 TYEAR PIC 9(4).
       01 BDAY PIC 9(2).
       01 BMONTH PIC 9(2).
       01 BYEAR PIC 9(4).
       01 AGE PIC 9(3) VALUE 0.

       PROCEDURE DIVISION USING TDAY,
                                TMONTH,
                                TYEAR,
                                BDAY,
                                BMONTH,
                                BYEAR,
                                AGE.
           COMPUTE YEAR_DIFF = TYEAR - BYEAR
           .

           DISPLAY TMONTH
           .
           IF BMONTH > TMONTH THEN
               COMPUTE AGE = YEAR_DIFF - 1
           END-IF
           .

           IF BMONTH < TMONTH THEN
               MOVE YEAR_DIFF TO AGE
           END-IF
           .

           IF BMONTH = TMONTH THEN
               IF BDAY > TDAY THEN
                   COMPUTE AGE = YEAR_DIFF - 1
               ELSE
                   MOVE YEAR_DIFF TO AGE
               END-IF
           END-IF
           .

           EXIT PROGRAM
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. P1.
       AUTHOR. WILLIAM-KEISLOHN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 STARTING_AMOUNT PIC 9(10) VALUE ZEROS.
       01 ENDING_AMOUNT PIC 9(10) VALUE ZEROS.
       01 DIFFERENCE_AMOUNT PIC 9(10) VALUE ZEROS.
       01 PERCENT_CHANGE PIC 99V9999 VALUE 0.
       01 PERCENT_CHANGE_YEAR PIC 99V9999 VALUE 0.
       01 GOAL_AMOUNT PIC 9(10) VALUE 1000000.
       01 BIRTH_DATE.
         05 BIRTH_MONTH PIC 9(2) VALUE ZEROS.
         05 BIRTH_DAY PIC 9(2) VALUE ZEROES.
         05 BIRTH_YEAR PIC 9(4) VALUE ZEROS.
         05 BIRTH_DAY_CORRECT PIC 9 VALUE 0.
           88 DAY-TRUE VALUE 1.
           88 DAY-FALSE VALUE 0.
         05 BIRTH_DAY_COMMON PIC X(50).
         05 BIRTH_MONTH_NAME PIC A(10).
       01 AGE PIC 9(3) VALUE 0.
       01 AGE_VERIFIED PIC 9(1) VALUE 0.
       01 WS-CURRENT-DATE-FIELDS. *> FROM IBM.
         05 WS-CURRENT-DATE.
           10 WS-CURRENT-YEAR PIC 9(4).
           10 WS-CURRENT-MONTH PIC 9(2).
           10 WS-CURRENT-DAY PIC 9(2).
         05 WS-CURRENT-TIME.
           10 WS-CURRENT-HOUR PIC 9(2).
           10 WS-CURRENT-MINUTE PIC 9(2).
           10 WS-CURRENT-SECOND PIC 9(2).
           10 WS-CURRENT-MS PIC 9(2).
         05 WS-DIFF-FROM-GMT PIC S9(4).
       01 YEARS_TO_GROW PIC 9(3) VALUE 0.
       01 GOAL_AGE PIC 9(1) VALUE 0.
       01 USER_ANSWER PIC A(3) VALUE "N".
       01 USER_AGREE PIC 9 VALUE 0.
       01 GOAL_FORMAT PIC $ZZ,ZZZ,ZZZ.ZZCR VALUE ZEROS.
       01 DIFF_FORMAT PIC $ZZ,ZZZ,ZZZ.ZZCR VALUE ZEROS.
       01 USER_SATISFIED PIC 9 VALUE 1.

       PROCEDURE DIVISION.

           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           .

       MAIN-PARA. *>"OOP"
           PERFORM AGE-PARA
           .
           PERFORM INVEST-PARA UNTIL USER_SATISFIED = 0
           .
           STOP RUN
           .

       AGE-PARA.
           PERFORM BIRTH-DATE-PARA UNTIL AGE_VERIFIED = 1
           .
           CALL "P_Age" USING WS-CURRENT-DAY,
                             WS-CURRENT-MONTH,
                             WS-CURRENT-YEAR,
                             BIRTH_DAY,
                             BIRTH_MONTH,
                             BIRTH_YEAR,
                             AGE
           .
           DISPLAY "YOUR CALCULATED AGE IS: " AGE
           .

       BIRTH-DATE-PARA.
           DISPLAY "PLEASE ENTER YOUR BIRTHDAY BELOW"
           .
           DISPLAY "PLEASE ENTER YOUR BIRTH YEAR: " WITH NO ADVANCING
           ACCEPT BIRTH_YEAR
           .
           DISPLAY "PLEASE ENTER YOUR BIRTH MONTH: " WITH NO ADVANCING
           ACCEPT BIRTH_MONTH
           .
           DISPLAY "PLEASE ENTER THE DAY OF THE MONTH YOU WERE BORN: "
             WITH NO ADVANCING
           ACCEPT BIRTH_DAY
           .
           PERFORM COMMON-DATE-PARA
           .
           DISPLAY "IS THIS BIRTHDAY CORRECT? [Y/n] " WITH NO ADVANCING
           ACCEPT USER_ANSWER
           .
           CALL "PUser" USING USER_ANSWER,
                              BIRTH_DAY_CORRECT
           .
           MOVE BIRTH_DAY_CORRECT TO AGE_VERIFIED *> Probably extra. 
           .

       COMMON-DATE-PARA.
           CALL "P1TM" USING BIRTH_MONTH,
                             BIRTH_MONTH_NAME
           .
           STRING BIRTH_MONTH_NAME " " BIRTH_DAY ", " BIRTH_YEAR
             DELIMITED BY SIZE INTO BIRTH_DAY_COMMON
           .
           DISPLAY "THE BIRTHDAY YOU HAVE ENTERED IS: " BIRTH_DAY_COMMON
           .

       INVEST-PARA.
           DISPLAY "PLEASE ENTER YOUR INITIAL INVESTMENT AMOUNT: " WITH
             NO ADVANCING
           ACCEPT STARTING_AMOUNT
           .
           DISPLAY "HAS AN INVESTMENT PERIOD PASSED? [Y/n] " WITH NO
             ADVANCING
             ACCEPT USER_ANSWER
           .
           CALL "PUser" USING USER_ANSWER,
                              USER_AGREE
           .
           IF USER_AGREE = 1 THEN
               CALL "PINV1" USING STARTING_AMOUNT,
                                  ENDING_AMOUNT,
                                  DIFFERENCE_AMOUNT,
                                  PERCENT_CHANGE,
                                  PERCENT_CHANGE_YEAR
               DISPLAY "YOUR MONTHLY INTEREST RATE IS: " PERCENT_CHANGE
               DISPLAY "YOUR ANNUAL INTEREST RATE IS: "
                 PERCENT_CHANGE_YEAR
               PERFORM EST-PARA
           ELSE
               PERFORM SPEC-PARA
           END-IF
           .
           DISPLAY "WOULD YOU LIKE TO TRY AGAIN? [Y/n] " WITH NO
           ADVANCING
           ACCEPT USER_ANSWER
           .
           CALL "PUser" USING USER_ANSWER,
                              USER_AGREE
           .
           MOVE USER_AGREE TO USER_SATISFIED
           .

       EST-PARA.
           MOVE 0 TO USER_AGREE *> REST THE VALUE
           .
           DISPLAY "DO YOU HAVE A GOAL FOR THIS ACCOUNT? [Y/n] "
             WITH NO ADVANCING
           ACCEPT USER_ANSWER
           .
           CALL "PUser" USING USER_ANSWER,
                              USER_AGREE
           .
           IF USER_AGREE = 1 THEN
               DISPLAY "PLEASE ENTER YOUR GOAL AMOUNT: " WITH NO
                 ADVANCING
               ACCEPT GOAL_AMOUNT
           ELSE
               MOVE GOAL_AMOUNT TO GOAL_FORMAT
               DISPLAY "A GOAL AMOUNT OF " GOAL_FORMAT
                 " WILL BE USED."
           END-IF
           .
           CALL "PINV2" USING ENDING_AMOUNT,
                              GOAL_AMOUNT,
                              AGE,
                              GOAL_AGE,
                              YEARS_TO_GROW,
                              PERCENT_CHANGE_YEAR
           .
       SPEC-PARA.
           DISPLAY "PLEASE ENTER ESTIMATED INTEREST AS A DECIMAL: " WITH
             NO ADVANCING
           ACCEPT PERCENT_CHANGE_YEAR
           .
           DISPLAY "ENTER YEARS OF GROWTH: " WITH NO ADVANCING
           ACCEPT YEARS_TO_GROW
           .
           CALL "PINV3" USING PERCENT_CHANGE_YEAR,
                              STARTING_AMOUNT,
                              YEARS_TO_GROW,
                              GOAL_FORMAT,
                              DIFF_FORMAT
           .

       RESET-PARA. *> IF I COMMENT IT OUT, THE PROGRAM BREAKS.
           MOVE 0 TO STARTING_AMOUNT
           .
           MOVE 0 TO ENDING_AMOUNT
           .
           MOVE 0 TO PERCENT_CHANGE
           .
           MOVE 0 TO PERCENT_CHANGE_YEAR
           .
           MOVE 0 TO YEARS_TO_GROW
           .
           MOVE 0 TO DIFFERENCE_AMOUNT
           .
           MOVE 1000000 TO GOAL_AMOUNT
           .
           
       STOP RUN. 

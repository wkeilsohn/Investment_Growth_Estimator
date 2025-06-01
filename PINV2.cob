       IDENTIFICATION DIVISION.
       PROGRAM-ID. PINV2.
       AUTHOR. WILLIAM-KEILSOHN.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       LINKAGE SECTION.
       01 CURRENT_VALUE PIC 9(10) VALUE ZEROS.
       01 END_GOAL PIC 9(10) VALUE ZEROS.
       01 CURRENT_AGE PIC 9(3) VALUE 0.
       01 END_AGE PIC 9(3) VALUE 0.
       01 AGE_DIFF PIC 9(3) VALUE 0.
       01 PER_GROWTH PIC 99V9999 VALUE ZEROS. 

       PROCEDURE DIVISION USING CURRENT_VALUE,
                                END_GOAL,
                                CURRENT_AGE,
                                END_AGE,
                                AGE_DIFF,
                                PER_GROWTH.

           COMPUTE AGE_DIFF = (FUNCTION LOG (END_GOAL / CURRENT_VALUE)
             )
             / (FUNCTION LOG (1 + (PER_GROWTH)))
           .
           COMPUTE END_AGE = CURRENT_AGE + AGE_DIFF
           .
           DISPLAY "AT THE CURRENT INTEREST RATE, IT WILL TAKE: "
             AGE_DIFF " YEARS FOR YOU TO REACH YOUR GOAL."
           .
           DISPLAY "YOU WILL BE APPROX. " END_AGE " YEARS OLD WHEN "
             "YOU REACH YOUR GOAL."
           .

           EXIT PROGRAM
           .
           
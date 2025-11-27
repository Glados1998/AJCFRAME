       IDENTIFICATION DIVISION. 
       PROGRAM-ID. TANOMLOG.
      ******************************************************************
      * TANOMLOG - unit test for ANOMLOG function                      *
      ******************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
         FILE-CONTROL. 
            SELECT TEST-LOG ASSIGN TO DDOUT2
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.         
       FILE SECTION. 
       FD  TEST-LOG RECORDING MODE IS F BLOCK CONTAINS 80 RECORDS.
       01  TEST-RECORD        PIC X(80).

       WORKING-STORAGE SECTION.
       COPY TESTCONT.
       COPY ANOMLOG.

            
              


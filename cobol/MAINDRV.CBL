       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINDRV.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STATS-FILE ASSIGN TO DDOUT1
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-STATS-STATUS.
           SELECT JOURNAL-FILE ASSIGN TO DDOUT2
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-JOURNAL-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  STATS-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  STATS-RECORD         PIC X(80).
       
       FD  JOURNAL-FILE
           RECORDING MODE IS F
           RECORD CONTAINS 80 CHARACTERS.
       01  JOURNAL-RECORD       PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  WS-PARM              PIC X(8).
       01  WS-RESULT            PIC X(100).
       01  WS-STATS-STATUS      PIC XX.
       01  WS-JOURNAL-STATUS    PIC XX.
       01  WS-TIMESTAMP         PIC X(26).
       01  WS-RETURN-CODE       PIC 9(4) VALUE 0.
       
       PROCEDURE DIVISION.
           ACCEPT WS-PARM FROM SYSPARM.
           
      *    Open files for append
           OPEN EXTEND JOURNAL-FILE.
           IF WS-JOURNAL-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING JOURNAL: ' WS-JOURNAL-STATUS
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
           OPEN EXTEND STATS-FILE.
           IF WS-STATS-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING STATS: ' WS-STATS-STATUS
               CLOSE JOURNAL-FILE
               MOVE 8 TO RETURN-CODE
               GOBACK
           END-IF.
           
      *    Call appropriate subprogram based on PARM
           EVALUATE WS-PARM
               WHEN 'SUBPROCA'
                   CALL 'SUBPROCA' USING WS-RESULT
               WHEN 'SUBPROCB'
                   CALL 'SUBPROCB' USING WS-RESULT
               WHEN 'SUBPROCC'
                   CALL 'SUBPROCC' USING WS-RESULT
               WHEN OTHER
                   MOVE 'Invalid subproc Specified' TO WS-RESULT
                   MOVE 4 TO WS-RETURN-CODE
           END-EVALUATE.
           
      *    Write results to journal file with timestamp
           ACCEPT WS-TIMESTAMP FROM TIME.
           STRING WS-TIMESTAMP ' | Task: ' WS-PARM 
                  ' | Result: ' WS-RESULT 
                  DELIMITED BY SIZE INTO JOURNAL-RECORD.
           WRITE JOURNAL-RECORD.
           IF WS-JOURNAL-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING JOURNAL: ' WS-JOURNAL-STATUS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           
      *    Write statistics (formatted result from subprogram)
           MOVE WS-RESULT TO STATS-RECORD.
           WRITE STATS-RECORD.
           IF WS-STATS-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING STATS: ' WS-STATS-STATUS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           
      *    Close files and exit
           CLOSE STATS-FILE.
           CLOSE JOURNAL-FILE.
           
           DISPLAY 'Processing complete: ' WS-RESULT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           STOP RUN.
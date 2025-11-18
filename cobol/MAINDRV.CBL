       CBL DYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINDRV.
      *****************************************************************
      * MAINDRV - Driver program for DB2 subprogram execution         *
      *                                                               *
      * PURPOSE: Receives PARM to select subprogram, dynamically      *
      *          calls it, collects results, writes to output files   *
      *                                                               *
      * INPUT:   PARM from JCL (SUBPROCA/SUBPROCB/SUBPROCC)           *
      * OUTPUT:  DDOUT1 (stats), DDOUT2 (journal with timestamp)      *
      * CALLED:  Subprograms via dynamic CALL (CBL DYNAM)             *
      *                                                               *
      * RETURN CODES: 0=success, 4=invalid PARM, 8=error              *
      *****************************************************************
       
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
       01  WS-PARM              PIC X(8).     *> Normalized subprog name
       01  WS-RESULT            PIC X(100).   *> Result from subprog
       01  WS-STATS-STATUS      PIC XX.       *> File status DDOUT1
       01  WS-JOURNAL-STATUS    PIC XX.       *> File status DDOUT2
       01  WS-TIMESTAMP         PIC X(26).    *> Execution timestamp
       01  WS-RETURN-CODE       PIC 9(4) VALUE 0.
       01  WS-INDEX             PIC 9  VALUE 1. *> Loop counter
       01  WS-LEN               PIC 9  VALUE 0. *> String length
       01  WS-PARM-TRIMMED      PIC X(8).       *> Temp for trimming

       LINKAGE SECTION.
       01  LS-PARM-BLOCK.                       *> JCL PARM structure
           05  LS-PARM-LEN   PIC S9(4) COMP.    *> Length of PARM
           05  LS-PARM-TEXT  PIC X(100).        *> PARM text
       
       PROCEDURE DIVISION USING LS-PARM-BLOCK.
       
      *--- Extract and validate PARM (limit to 8 chars) -------------*
           IF LS-PARM-LEN > LENGTH OF WS-PARM
              MOVE LENGTH OF WS-PARM TO LS-PARM-LEN
           END-IF
           
           IF LS-PARM-LEN > 0
              MOVE LS-PARM-TEXT(1:LS-PARM-LEN) TO WS-PARM
           ELSE
              MOVE 'SUBPROCA' TO WS-PARM        *> Default if no PARM
           END-IF.
           
      *--- Open output files for append -----------------------------*
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

      *--- Normalize PARM: trim spaces, upper-case ------------------*
           INSPECT WS-PARM REPLACING ALL '''' BY SPACE

      *    Remove leading spaces
           PERFORM VARYING WS-INDEX FROM 1 BY 1
             UNTIL WS-INDEX > LENGTH OF WS-PARM
               OR WS-PARM(WS-INDEX:1) NOT = SPACE
           END-PERFORM
           MOVE WS-PARM(WS-INDEX:) TO WS-PARM-TRIMMED

      *    Remove trailing spaces
           MOVE LENGTH OF WS-PARM-TRIMMED TO WS-LEN
           PERFORM VARYING WS-LEN FROM LENGTH OF WS-PARM-TRIMMED BY -1
             UNTIL WS-LEN < 1
               OR WS-PARM-TRIMMED(WS-LEN:1) NOT = SPACE
           END-PERFORM
           IF WS-LEN < LENGTH OF WS-PARM-TRIMMED
              MOVE WS-PARM-TRIMMED(1:WS-LEN) TO WS-PARM-TRIMMED
              IF WS-LEN < LENGTH OF WS-PARM-TRIMMED
                 MOVE SPACES TO WS-PARM-TRIMMED(WS-LEN + 1:)
              END-IF
           END-IF

           MOVE FUNCTION UPPER-CASE(WS-PARM-TRIMMED) TO WS-PARM
           
      *--- Dynamic CALL to selected subprogram ----------------------*
           EVALUATE WS-PARM
               WHEN 'SUBPROCA'
                   CALL 'SUBPROCA' USING WS-RESULT
                       ON EXCEPTION
                           MOVE 'ERROR: SUBPROCA not found' TO WS-RESULT
                           MOVE 8 TO WS-RETURN-CODE
                   END-CALL
               WHEN 'SUBPROCB'
                   CALL 'SUBPROCB' USING WS-RESULT
                       ON EXCEPTION
                           MOVE 'ERROR: SUBPROCB not found' TO WS-RESULT
                           MOVE 8 TO WS-RETURN-CODE
                   END-CALL
               WHEN 'SUBPROCC'
                   CALL 'SUBPROCC' USING WS-RESULT
                       ON EXCEPTION
                           MOVE 'ERROR: SUBPROCC not found' TO WS-RESULT
                           MOVE 8 TO WS-RETURN-CODE
                   END-CALL
               WHEN OTHER
                   MOVE 'Invalid subproc Specified' TO WS-RESULT
                   MOVE 4 TO WS-RETURN-CODE
           END-EVALUATE.
           
      *--- Write timestamped journal entry --------------------------*
           ACCEPT WS-TIMESTAMP FROM TIME.
           STRING WS-TIMESTAMP ' | Task: ' WS-PARM 
                  ' | Result: ' WS-RESULT 
                  DELIMITED BY SIZE INTO JOURNAL-RECORD.
           WRITE JOURNAL-RECORD.
           IF WS-JOURNAL-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING JOURNAL: ' WS-JOURNAL-STATUS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           
      *--- Write statistics (subprogram result) ---------------------*
           MOVE WS-RESULT TO STATS-RECORD.
           WRITE STATS-RECORD.
           IF WS-STATS-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING STATS: ' WS-STATS-STATUS
               MOVE 8 TO WS-RETURN-CODE
           END-IF.
           
      *--- Cleanup and exit -----------------------------------------*
           CLOSE STATS-FILE.
           CLOSE JOURNAL-FILE.
           
           DISPLAY 'Processing complete: ' WS-RESULT.
           MOVE WS-RETURN-CODE TO RETURN-CODE.
           STOP RUN.
       CBL DYNAM, LIB  
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAINDRV.
      *****************************************************************
      * MAINDRV - Driver program for DB2 subprogram execution         *
      *                                                               *
      * PURPOSE: Receives PARM to select subprogram, dynamically      *
      *          calls it, collects results, writes to output files   *
      *                                                               *
      * CALLED:  Subprograms via dynamic CALL (CBL DYNAM)             *
      *                                                               *
      * RETURN CODES: 0=success, 4=invalid PARM, 8=error              *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * No local file usage; logging delegated to ANOMLOG
       
       DATA DIVISION.
       FILE SECTION.
      * Removed local journal FD; ANOMLOG manages DDOUT2
       
       WORKING-STORAGE SECTION.
       01  WS-PARM              PIC X(8).     
       01  WS-RESULT            PIC X(100).   
       01  WS-TIMESTAMP         PIC X(26).    
       01  WS-RETURN-CODE       PIC 9(4) VALUE 0.
       01  WS-INDEX             PIC 9  VALUE 1. 
       01  WS-LEN               PIC 9  VALUE 0. 
       01  WS-PARM-TRIMMED      PIC X(8).
       01  WS-CURRENT-DATE.
           05  WS-YEAR          PIC 9(4).
           05  WS-MONTH         PIC 9(2).
           05  WS-DAY           PIC 9(2).
       01  WS-CURRENT-TIME.
           05  WS-HOUR          PIC 9(2).
           05  WS-MINUTE        PIC 9(2).
           05  WS-SECOND        PIC 9(2).

       COPY ANOMLOG.
       
       LINKAGE SECTION.
       01  LS-PARM-BLOCK.                       
           05  LS-PARM-LEN   PIC S9(4) COMP.    
           05  LS-PARM-TEXT  PIC X(100).        
       
       PROCEDURE DIVISION USING LS-PARM-BLOCK.
       
      *--- Extract and validate PARM (limit to 8 chars) -------------*
           IF LS-PARM-LEN > LENGTH OF WS-PARM
              MOVE LENGTH OF WS-PARM TO LS-PARM-LEN
           END-IF
           
           IF LS-PARM-LEN > 0
              MOVE LS-PARM-TEXT(1:LS-PARM-LEN) TO WS-PARM
           ELSE
              MOVE 'SUBPROCA' TO WS-PARM        
           END-IF.

      *--- Normalize PARM: trim spaces, upper-case ------------------*
      *    Trim spaces and remove quotes if any are present          *
      *    replaces the TRIM function which dosent work              *
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
           
      *--- Display execution header at top --------------------------*
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           DISPLAY ' '
           DISPLAY ' '
           DISPLAY '=================================================='
           DISPLAY 'MAINDRV Execution Started'
           DISPLAY '=================================================='
           DISPLAY 'Date       : ' WS-YEAR '/' WS-MONTH '/' WS-DAY
           DISPLAY 'Time       : ' WS-HOUR ':' WS-MINUTE ':' WS-SECOND
           DISPLAY 'Subprogram : ' WS-PARM
           DISPLAY '=================================================='
           DISPLAY ' '.
           
      *--- Dynamic CALL to selected subprogram ----------------------*
           EVALUATE WS-PARM
               WHEN 'SUBPROCA'
                   CALL 'SUBPROCA' USING WS-RESULT
                       ON EXCEPTION
                        MOVE 'ERROR: SUBPROCA not found' TO WS-RESULT
                        MOVE 8 TO WS-RETURN-CODE
                        MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                        MOVE WS-PARM   TO AL-PROGRAM-NAME
                        MOVE WS-RETURN-CODE TO AL-RETURN-CODE
                        MOVE WS-RESULT TO AL-DESCRIPTION
                        CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   END-CALL
               WHEN 'SUBPROCB'
                   CALL 'SUBPROCB' USING WS-RESULT
                       ON EXCEPTION
                        MOVE 'ERROR: SUBPROCB not found' TO WS-RESULT
                        MOVE 8 TO WS-RETURN-CODE
                        MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                        MOVE WS-PARM   TO AL-PROGRAM-NAME
                        MOVE WS-RETURN-CODE TO AL-RETURN-CODE
                        MOVE WS-RESULT TO AL-DESCRIPTION
                        CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   END-CALL
               WHEN 'SUBPROCC'
                   CALL 'SUBPROCC' USING WS-RESULT
                       ON EXCEPTION
                        MOVE 'ERROR: SUBPROCC not found' TO WS-RESULT
                        MOVE 8 TO WS-RETURN-CODE
                        MOVE 'ANOMALY' TO AL-MESSAGE-TYPE                         
                        MOVE WS-PARM   TO AL-PROGRAM-NAME
                        MOVE WS-RETURN-CODE TO AL-RETURN-CODE
                        MOVE WS-RESULT TO AL-DESCRIPTION
                        CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   END-CALL
               WHEN OTHER
                   MOVE 'Invalid subproc Specified' TO WS-RESULT
                   MOVE 8 TO WS-RETURN-CODE
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE WS-PARM   TO AL-PROGRAM-NAME
                   MOVE WS-RETURN-CODE TO AL-RETURN-CODE
                   MOVE WS-RESULT TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK
               END-EVALUATE.
           
      *--- Display completion footer --------------------------------*
           DISPLAY ' '
           DISPLAY '=================================================='
           IF WS-RETURN-CODE > 0
               DISPLAY '*** EXECUTION FAILED ***'
           ELSE
               DISPLAY '*** EXECUTION SUCCESSFUL ***'
           END-IF
           DISPLAY 'Result     : ' WS-RESULT
           DISPLAY 'Return Code: ' WS-RETURN-CODE
           DISPLAY '=================================================='
           DISPLAY ' '
           DISPLAY ' '
           
      *--- Cleanup and exit -----------------------------------------*
           MOVE WS-RETURN-CODE TO RETURN-CODE
           STOP RUN.
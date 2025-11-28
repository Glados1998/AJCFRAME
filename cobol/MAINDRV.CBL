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
       
       DATA DIVISION.
       
       WORKING-STORAGE SECTION.
       01  WS-PARM              PIC X(8).     
       01  WS-RESULT            PIC X(100).   
       01  WS-TIMESTAMP         PIC X(26).    
       01  WS-RETURN-CODE       PIC 9(4) VALUE 0.
       01  WS-INDEX             PIC 9  VALUE 1. 
       01  WS-LEN               PIC 9  VALUE 0. 
       01  WS-PARM-TRIMMED      PIC X(8).
       01  WS-USERID-TRIMMED    PIC X(8).
       01  WS-USERID            PIC X(8).
       01  WS-FULL-PARM         PIC X(20).
       01  WS-COMMA-POS         PIC 9(2) VALUE 0.
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
       
      *--- Extract and validate PARM (format: SUBPROG,USERID) -------*
           IF LS-PARM-LEN > LENGTH OF WS-FULL-PARM
              MOVE LENGTH OF WS-FULL-PARM TO LS-PARM-LEN
           END-IF
           
           IF LS-PARM-LEN > 0
              MOVE LS-PARM-TEXT(1:LS-PARM-LEN) TO WS-FULL-PARM
           ELSE
              MOVE 'SUBPROCA,IBMUSER' TO WS-FULL-PARM        
           END-IF

      *--- Parse PARM to extract subprogram and userid --------------*
      *    Find comma position
           INSPECT WS-FULL-PARM TALLYING WS-COMMA-POS FOR CHARACTERS
               BEFORE INITIAL ','
           
           IF WS-COMMA-POS < LENGTH OF WS-FULL-PARM
      *       Extract subprogram name (before comma)
              MOVE WS-FULL-PARM(1:WS-COMMA-POS) TO WS-PARM
      *       Extract userid (after comma)  
              MOVE WS-FULL-PARM(WS-COMMA-POS + 2:) TO WS-USERID
           ELSE
      *       No comma found, use defaults
              MOVE WS-FULL-PARM TO WS-PARM
              MOVE 'IBMUSER' TO WS-USERID
           END-IF

      *--- Normalize both values: trim spaces, upper-case -----------*
      *    Trim and upper-case WS-PARM
           PERFORM VARYING WS-INDEX FROM 1 BY 1 
               UNTIL WS-INDEX > LENGTH OF WS-PARM
               OR WS-PARM(WS-INDEX:1) NOT = SPACE
           END-PERFORM
           IF WS-INDEX <= LENGTH OF WS-PARM
               MOVE WS-PARM(WS-INDEX:) TO WS-PARM-TRIMMED
           ELSE
               MOVE SPACES TO WS-PARM-TRIMMED
           END-IF
           INSPECT WS-PARM-TRIMMED CONVERTING 
               'abcdefghijklmnopqrstuvwxyz' TO 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           MOVE WS-PARM-TRIMMED TO WS-PARM
           
      *    Trim and upper-case WS-USERID  
           PERFORM VARYING WS-INDEX FROM 1 BY 1 
               UNTIL WS-INDEX > LENGTH OF WS-USERID
               OR WS-USERID(WS-INDEX:1) NOT = SPACE
           END-PERFORM
           IF WS-INDEX <= LENGTH OF WS-USERID
               MOVE WS-USERID(WS-INDEX:) TO WS-USERID-TRIMMED
           ELSE
               MOVE SPACES TO WS-USERID-TRIMMED
           END-IF
           INSPECT WS-USERID-TRIMMED CONVERTING 
               'abcdefghijklmnopqrstuvwxyz' TO 
               'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           MOVE WS-USERID-TRIMMED TO WS-USERID

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
           DISPLAY 'User       : ' WS-USERID
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
                        MOVE WS-USERID TO AL-USER-ID
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
                        MOVE WS-USERID TO AL-USER-ID
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
                        MOVE WS-USERID TO AL-USER-ID
                        MOVE WS-RETURN-CODE TO AL-RETURN-CODE
                        MOVE WS-RESULT TO AL-DESCRIPTION
                        CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   END-CALL
               WHEN OTHER
                   MOVE 'Invalid subproc Specified' TO WS-RESULT
                   MOVE 8 TO WS-RETURN-CODE
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE WS-PARM   TO AL-PROGRAM-NAME
                   MOVE WS-USERID TO AL-USER-ID
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
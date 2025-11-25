       CBL LIB
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ANOMLOG.
      *****************************************************************
      * ANOMLOG - Anomaly and error logging programs                  *
      *                                                               *
      * PURPOSE: Centralize logging of errors and anomalies           *
      *          into journal dataset DDOUT2                          *
      *                                                               *
      * SETUP REQUIREMENTS:                                           *
      *  1. Compile: Use companomlog.jcl to compile and link          *
      *  2. Copybook: Add COPY ANOMLOG in WORKING-STORAGE             *
      *  3. Compiler: Add CBL LIB directive at top of calling program *
      *  4. DD Card: Define DDOUT2 in execution JCL:                  *
      *     //DDOUT2 DD DSN=userid.PROJET.JRN.DATA,                   *
      *     //         DISP=(MOD,CATLG,DELETE),                       *
      *     //         DCB=(RECFM=FB,LRECL=80,BLKSIZE=0),             *
      *     //         SPACE=(TRK,(1,1),RLSE),UNIT=SYSDA              *
      *                                                               *
      * CALLING SEQUENCE:                                             *
      *  MOVE 'ANOMALY' TO AL-MESSAGE-TYPE                            *
      *  MOVE 'MAINDRV'  TO AL-PROGRAM-NAME                           *
      *  MOVE 8          TO AL-RETURN-CODE                            *
      *  MOVE 'Error description' TO AL-DESCRIPTION                   *
      *  CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL             *
      *                                                               *
      * PARAM BLOCK (COPY ANOMLOG.CPY):                               *
      *  AL-MESSAGE-TYPE : TYPE ('ABEND','FATAL','ERROR','WARNING',   *
      *                    'ANOMALY','INFO')                          *
      *  AL-PROGRAM-NAME : 8-char caller/subprogram id                *
      *  AL-RETURN-CODE  : Return code associated with event          *
      *  AL-DESCRIPTION  : Free-form description (truncated to fit)   *
      *                                                               *
      * LOG FORMAT: (80 chars fixed-length)                           *
      *  YYYY-MM-DD HH:MM:SS | TYPE | PROGNAME | RC=nnnn | Desc...   *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO DDOUT2
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS WS-LOG-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE RECORDING MODE IS F RECORD CONTAINS 80 CHARACTERS.
       01  LOG-RECORD              PIC X(80).
       WORKING-STORAGE SECTION.
       01  WS-LOG-STATUS           PIC XX.
       01  WS-DATE-STR             PIC X(10).
       01  WS-TIME-STR             PIC X(8).
       01  WS-CURRENT-DATE.
           05  WS-YEAR          PIC 9(4).
           05  WS-MONTH         PIC 9(2).
           05  WS-DAY           PIC 9(2).
       01  WS-CURRENT-TIME.
           05  WS-HOUR          PIC 9(2).
           05  WS-MINUTE        PIC 9(2).
           05  WS-SECOND        PIC 9(2).
       01  WS-TEMP-MSG             PIC X(120).
       01  WS-RC-DISPLAY           PIC 9(4).
       LINKAGE SECTION.
       COPY ANOMLOG.
       PROCEDURE DIVISION USING ANOMLOG-PARM-BLOCK.
      * Retrieve timestamp components
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-YEAR '-' WS-MONTH '-' WS-DAY
                  DELIMITED BY SIZE INTO WS-DATE-STR
           STRING WS-HOUR ':' WS-MINUTE ':' WS-SECOND
                  DELIMITED BY SIZE INTO WS-TIME-STR
      * Open for append
           OPEN EXTEND LOG-FILE
           IF WS-LOG-STATUS NOT = '00'
               DISPLAY 'ANOMLOG OPEN FAILURE STATUS=' WS-LOG-STATUS
               GOBACK
           END-IF
      * Convert numeric RC to display format
           MOVE AL-RETURN-CODE TO WS-RC-DISPLAY
      * Branch on message type for formatting
           EVALUATE AL-MESSAGE-TYPE
             WHEN 'ANOMALY'
               STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                      AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                      'RC=' WS-RC-DISPLAY ' | '
                      AL-DESCRIPTION(1:24)
                      DELIMITED BY SIZE INTO LOG-RECORD
             WHEN 'ERROR'
               STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                      AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                      'RC=' WS-RC-DISPLAY ' | '
                      AL-DESCRIPTION(1:26)
                      DELIMITED BY SIZE INTO LOG-RECORD
             WHEN 'WARNING'
               STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                      AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                      'RC=' WS-RC-DISPLAY ' | '
                      AL-DESCRIPTION(1:24)
                      DELIMITED BY SIZE INTO LOG-RECORD
             WHEN 'FATAL'
               STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                      AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                      'RC=' WS-RC-DISPLAY ' | '
                      AL-DESCRIPTION(1:28)
                      DELIMITED BY SIZE INTO LOG-RECORD
             WHEN 'ABEND'
              STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                     AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                     'RC=' WS-RC-DISPLAY ' | '
                     AL-DESCRIPTION(1:28)
                     DELIMITED BY SIZE INTO LOG-RECORD         
             WHEN OTHER
               STRING WS-DATE-STR ' ' WS-TIME-STR ' | '
                      AL-MESSAGE-TYPE ' | ' AL-PROGRAM-NAME ' | '
                      AL-DESCRIPTION(1:35)
                      DELIMITED BY SIZE INTO LOG-RECORD
           END-EVALUATE
      * Write record
           WRITE LOG-RECORD
           IF WS-LOG-STATUS NOT = '00'
               DISPLAY 'ANOMLOG WRITE FAILURE STATUS=' WS-LOG-STATUS
               DISPLAY 'TEXT=' LOG-RECORD
           END-IF
      * Close
           CLOSE LOG-FILE
           GOBACK.
       END PROGRAM ANOMLOG.

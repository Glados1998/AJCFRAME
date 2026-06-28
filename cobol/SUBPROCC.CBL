       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROCC.
      *****************************************************************
      * SUBPROCC - list all employees and their associated departments *
      * show their commissions and the total per department            *
      * show a total of commissions and employees combined             *         
      *****************************************************************
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
           EXEC SQL
               INCLUDE DCLEMP
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLDEPT
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLORD
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLITEMS
           END-EXEC.
       
      *    Host variables for query results
       01  HV-DEPT-NO          PIC S9(4)V COMP-3.
       01  HV-DEPT-NAME        PIC X(20).
       01  HV-EMP-NO           PIC S9(2)V COMP-3.
       01  HV-LNAME            PIC X(20).
       01  HV-FNAME            PIC X(20).
       01  HV-COM-PCT          PIC SV9(2) COMP-3.
       01  HV-TOTAL-SALES      PIC S9(9)V9(2) COMP-3.
       01  HV-COMMISSION-AMT   PIC S9(9)V9(2) COMP-3.
       
      *    Working variables
       01  WS-DISPLAY-LINE     PIC X(80).
       01  WS-PREV-DEPT        PIC S9(4)V COMP-3 VALUE -1.
       01  WS-DEPT-EMP-COUNT   PIC 9(3) VALUE 0.
       01  WS-DEPT-COM-TOTAL   PIC S9(7)V9(2) COMP-3 VALUE 0.
       01  WS-TOTAL-EMP        PIC 9(5) VALUE 0.
       01  WS-TOTAL-COM        PIC S9(10)V9(2) COMP-3 VALUE 0.
       01  WS-FULL-NAME        PIC X(45).
       
      *    Edited fields for display
       01  ED-COMMISSION       PIC $99,999.99.
       01  ED-DEPT-TOTAL       PIC $99,999.99.
       01  ED-GRAND-TOTAL      PIC $99,999.99.
       01  ED-DEPT-NO          PIC ZZZ9.
       
      *    SQLCODE variable
       01  WS-SQLCODE-DISPLAY  PIC -999999999.
       
      *    CURSOR: Employees with departments and calculated commission
           EXEC SQL
               DECLARE EMPDEPT CURSOR FOR
               SELECT D.DEPT,
                      D.DNAME,
                      E.E_NO,
                      E.LNAME,
                      E.FNAME,
                      COALESCE(E.COM, 0) AS COM_PCT,
                      COALESCE(SUM(I.QUANTITY * I.PRICE), 0) 
                          AS TOTAL_SALES,
                      (COALESCE(SUM(I.QUANTITY * I.PRICE), 0) 
                          * COALESCE(E.COM, 0)) AS COMMISSION_AMT
               FROM API4.DEPTS D
               LEFT JOIN API4.EMPLOYEES E ON E.DEPT = D.DEPT
               LEFT JOIN API4.ORDERS O ON O.S_NO = E.E_NO
               LEFT JOIN API4.ITEMS I ON I.O_NO = O.O_NO
               GROUP BY D.DEPT, D.DNAME, E.E_NO, E.LNAME, 
                        E.FNAME, E.COM
               ORDER BY D.DEPT, E.LNAME, E.FNAME
           END-EXEC.
       
       LINKAGE SECTION.
       01  LS-RESULT-BUFFER    PIC X(100).
       
       PROCEDURE DIVISION USING LS-RESULT-BUFFER.
       
      *--- Open cursor ----------------------------------------------*
           EXEC SQL
               OPEN EMPDEPT
           END-EXEC.
           
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-SQLCODE-DISPLAY
               STRING 'SUBPROCC: Error opening cursor - SQLCODE: '
                      WS-SQLCODE-DISPLAY DELIMITED BY SIZE
                  INTO LS-RESULT-BUFFER
               END-STRING
               EXIT PROGRAM
           END-IF.
       
      *--- Fetch and display data -----------------------------------*
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH EMPDEPT
                   INTO :HV-DEPT-NO,
                        :HV-DEPT-NAME,
                        :HV-EMP-NO,
                        :HV-LNAME,
                        :HV-FNAME,
                        :HV-COM-PCT,
                        :HV-TOTAL-SALES,
                        :HV-COMMISSION-AMT
               END-EXEC
               
               IF SQLCODE = 0
                   PERFORM PROCESS-EMPLOYEE-RECORD
               ELSE
                   IF SQLCODE NOT = 100
                       DISPLAY 'SUBPROCC: Fetch error - SQLCODE: '
                               SQLCODE
                   END-IF
               END-IF
           END-PERFORM.
       
      *--- Display final department total if needed -----------------*
           IF WS-PREV-DEPT NOT = -1
               PERFORM DISPLAY-DEPT-TOTAL
           END-IF.
       
      *--- Display grand total --------------------------------------*
           PERFORM DISPLAY-GRAND-TOTAL.
       
      *--- Close cursor ---------------------------------------------*
           EXEC SQL
               CLOSE EMPDEPT
           END-EXEC.
       
      *--- Return summary to caller ---------------------------------*
           STRING 'SUBPROCC: Department listing complete - '
                  WS-TOTAL-EMP ' employees processed'
                  DELIMITED BY SIZE INTO LS-RESULT-BUFFER
           END-STRING.
           
           EXIT PROGRAM.
       
      *--- Process employee record ----------------------------------*
       PROCESS-EMPLOYEE-RECORD.
      *    Check if new department
           IF HV-DEPT-NO NOT = WS-PREV-DEPT
               IF WS-PREV-DEPT NOT = -1
                   PERFORM DISPLAY-DEPT-TOTAL
               END-IF
               PERFORM DISPLAY-DEPT-HEADER
               MOVE HV-DEPT-NO TO WS-PREV-DEPT
               MOVE 0 TO WS-DEPT-EMP-COUNT
               MOVE 0 TO WS-DEPT-COM-TOTAL
           END-IF.
           
      *    Display employee line with aligned commission
           ADD 1 TO WS-DEPT-EMP-COUNT
           ADD 1 TO WS-TOTAL-EMP
           ADD HV-COMMISSION-AMT TO WS-DEPT-COM-TOTAL
           ADD HV-COMMISSION-AMT TO WS-TOTAL-COM
           
           MOVE HV-COMMISSION-AMT TO ED-COMMISSION
           
      *    Build full name (first + last)
           MOVE SPACES TO WS-FULL-NAME
           STRING HV-FNAME DELIMITED BY '  '
                  ' ' DELIMITED BY SIZE
                  HV-LNAME DELIMITED BY '  '
                  INTO WS-FULL-NAME
           END-STRING
           
      *    Build display line with fixed positions
           MOVE SPACES TO WS-DISPLAY-LINE
           STRING '* ' DELIMITED BY SIZE
                  WS-FULL-NAME DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           STRING WS-DISPLAY-LINE(1:24) DELIMITED BY SIZE
                  '*     COM : ' DELIMITED BY SIZE
                  ED-COMMISSION DELIMITED BY SIZE
                  '   *' DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           DISPLAY WS-DISPLAY-LINE.
       
      *--- Display department header --------------------------------*
       DISPLAY-DEPT-HEADER.
           DISPLAY '**************************************************'.
           MOVE SPACES TO WS-DISPLAY-LINE
           STRING '*            DEPARTMENT : ' DELIMITED BY SIZE
                  HV-DEPT-NAME DELIMITED BY '  '
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           MOVE '*' TO WS-DISPLAY-LINE(50:1)
           
           DISPLAY WS-DISPLAY-LINE
           DISPLAY '**************************************************'.
           
       
      *--- Display department total ---------------------------------*
       DISPLAY-DEPT-TOTAL.
           DISPLAY '**************************************************'.
           MOVE WS-DEPT-COM-TOTAL TO ED-DEPT-TOTAL
           MOVE SPACES TO WS-DISPLAY-LINE
           STRING '* COUNT EMP : ' DELIMITED BY SIZE
                  WS-DEPT-EMP-COUNT DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           STRING WS-DISPLAY-LINE(1:24) DELIMITED BY SIZE
                  '*     COM : ' DELIMITED BY SIZE
                  ED-DEPT-TOTAL DELIMITED BY SIZE
                  '   *' DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           DISPLAY WS-DISPLAY-LINE
           DISPLAY '**************************************************'.
           DISPLAY ' '.
       
      *--- Display grand total --------------------------------------*
       DISPLAY-GRAND-TOTAL.
           DISPLAY '**************************************************'.
           DISPLAY '*               ALL DEPARTMENTS                  *'.
           DISPLAY '**************************************************'.
           MOVE WS-TOTAL-COM TO ED-GRAND-TOTAL
           MOVE SPACES TO WS-DISPLAY-LINE
           STRING '* COUNT EMP : ' DELIMITED BY SIZE
                  WS-TOTAL-EMP DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           STRING WS-DISPLAY-LINE(1:24) DELIMITED BY SIZE
                  '*     COM : ' DELIMITED BY SIZE
                  ED-GRAND-TOTAL DELIMITED BY SIZE
                  '   *' DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING
           
           DISPLAY WS-DISPLAY-LINE
           DISPLAY '**************************************************'.
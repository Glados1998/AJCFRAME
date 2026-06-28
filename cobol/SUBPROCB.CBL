       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROCB.
      *****************************************************************
      * SUBPROCB - Aggregate sales by city from CUSTOMERS/ORDERS      *
      * Calculates total sales amount per city                        *
      *****************************************************************
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
           EXEC SQL
               INCLUDE DCLICUST
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLITEMS
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLORD
           END-EXEC.
       
      *    Host variables for aggregated results
       01  HV-CITY             PIC X(20).
       01  HV-TOTAL-SALES      PIC S9(10)V9(2) COMP-3 VALUE 0.
       
      *    Display formatting variables
       01  WS-DISPLAY-LINE     PIC X(80).
       01  WS-CITY-COUNT       PIC 9(3) VALUE 0.
       01  ED-TOTAL-SALES      PIC ZZZ,ZZZ,ZZ9.99.
       01  WS-GRAND-TOTAL      PIC S9(10)V9(2) COMP-3 VALUE 0.
       01  ED-GRAND-TOTAL      PIC ZZ,ZZZ,ZZZ,ZZ9.99.

      *    SQLCODE variable
       01  WS-SQLCODE-DISPLAY  PIC -999999999.
       
      *    CURSOR: Aggregate sales by city
           EXEC SQL
               DECLARE CITYAGG CURSOR FOR 
               SELECT C.CITY,
                      COALESCE(SUM(COALESCE(I.QUANTITY,0)
                      * COALESCE(I.PRICE,0)),
                      DECIMAL(0,2)) AS TOTAL_SALES
               FROM API4.CUSTOMERS C
               LEFT JOIN API4.ORDERS O ON O.C_NO = C.C_NO
               LEFT JOIN API4.ITEMS I ON I.O_NO = O.O_NO
               GROUP BY C.CITY
               ORDER BY C.CITY
           END-EXEC.
       
       LINKAGE SECTION.
       01  LS-RESULT-BUFFER    PIC X(100).
       
       PROCEDURE DIVISION USING LS-RESULT-BUFFER.
       
      *--- Open cursor and display header ---------------------------*
           EXEC SQL
               OPEN CITYAGG
           END-EXEC.
           
           IF SQLCODE NOT = 0
               MOVE SQLCODE TO WS-SQLCODE-DISPLAY
               STRING 'SUBPROCB: Error opening cursor - SQLCODE: '
                      WS-SQLCODE-DISPLAY DELIMITED BY SIZE
                  INTO LS-RESULT-BUFFER
               END-STRING
               EXIT PROGRAM
           END-IF.

      *--- Display table header -------------------------------------*
           DISPLAY '*******************************************'.
           DISPLAY '* City               | Sales              *'.
           DISPLAY '*******************************************'.
       
      *--- Fetch and display aggregated data ------------------------*
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH CITYAGG 
                   INTO :HV-CITY, 
                        :HV-TOTAL-SALES 
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-CITY-COUNT
                   ADD HV-TOTAL-SALES TO WS-GRAND-TOTAL
                   
      *            Format sales amount for display
                   MOVE HV-TOTAL-SALES TO ED-TOTAL-SALES
                   
      *            Build and display city line
                   STRING '* ' DELIMITED BY SIZE 
                          HV-CITY DELIMITED BY SIZE
                          ' : $ ' DELIMITED BY SIZE
                          ED-TOTAL-SALES DELIMITED BY SIZE
                          ' *' DELIMITED BY SIZE
                          INTO WS-DISPLAY-LINE
                   END-STRING
                   DISPLAY WS-DISPLAY-LINE
               ELSE
                   IF SQLCODE NOT = 100
                       DISPLAY 'SUBPROCB: Fetch error - SQLCODE: ' 
                               SQLCODE
                   END-IF
               END-IF
           END-PERFORM.
       
      *--- Display grand total --------------------------------------*
           DISPLAY '*******************************************'.
           MOVE WS-GRAND-TOTAL TO ED-GRAND-TOTAL.
           MOVE SPACES TO WS-DISPLAY-LINE.
           STRING '* Total             : $ ' DELIMITED BY SIZE
                  ED-GRAND-TOTAL DELIMITED BY SIZE
                  ' *' DELIMITED BY SIZE
                  INTO WS-DISPLAY-LINE
           END-STRING.
           DISPLAY WS-DISPLAY-LINE.
           DISPLAY '*******************************************'.

      *--- Close cursor ---------------------------------------------*
           EXEC SQL
               CLOSE CITYAGG
           END-EXEC.

      *--- Return summary to MAINDRV ----------------------------*
           STRING 'SUBPROCB: Sales aggregation complete - '
                  WS-CITY-COUNT ' cities processed'
                  DELIMITED BY SIZE INTO LS-RESULT-BUFFER
           END-STRING.
           
           EXIT PROGRAM.      
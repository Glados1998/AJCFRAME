       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROCA.
      *****************************************************************
      * SUBPROCA - Aggregate quantities per product (last 3 years)   *
      * Uses actual years from ORDERS table: 2023, 2024, 2025        *
      *****************************************************************lll
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
           EXEC SQL
               INCLUDE DCLIPROD
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLITEMS
           END-EXEC.
           
           EXEC SQL
               INCLUDE DCLORD
           END-EXEC.
       
      *    Host variables for aggregated results
       01  HV-PRODUCT-NO       PIC X(3).
       01  HV-DESCRIPTION      PIC X(30).
       01  HV-QTY-2025         PIC S9(9) COMP-3 VALUE 0.
       01  HV-QTY-2024         PIC S9(9) COMP-3 VALUE 0.
       01  HV-QTY-2023         PIC S9(9) COMP-3 VALUE 0.
       
      *    Display formatting variables
       01  WS-DISPLAY-LINE     PIC X(80).
       01  WS-PRODUCT-COUNT    PIC 9(3) VALUE 0.
       01  ED-QTY-2025         PIC Z(2)9.
       01  ED-QTY-2024         PIC Z(2)9.
       01  ED-QTY-2023         PIC Z(2)9.

      *    SQLCODE variable
       01  WS-SQLCODE-DISPLAY  PIC -999999999.
       
      *    CURSOR: Aggregate quantities by product for years 2023-2025
           EXEC SQL
               DECLARE PRODAGG CURSOR FOR
               SELECT P.P_NO,
                      P.DESCRIPTION,
                      SUM(CASE WHEN YEAR(O.O_DATE) = 2025
                              THEN I.QUANTITY ELSE 0 END),
                      SUM(CASE WHEN YEAR(O.O_DATE) = 2024
                              THEN I.QUANTITY ELSE 0 END),
                      SUM(CASE WHEN YEAR(O.O_DATE) = 2023
                              THEN I.QUANTITY ELSE 0 END)
               FROM API4.PRODUCTS P
               LEFT JOIN API4.ITEMS I ON I.P_NO = P.P_NO  
               LEFT JOIN API4.ORDERS O ON O.O_NO = I.O_NO
               WHERE YEAR(O.O_DATE) BETWEEN 2023 AND 2025
               GROUP BY P.P_NO, P.DESCRIPTION
               ORDER BY P.DESCRIPTION
           END-EXEC.
       
       LINKAGE SECTION.
       01  LS-RESULT-BUFFER    PIC X(100).
       
       PROCEDURE DIVISION USING LS-RESULT-BUFFER.
       
      *--- Open cursor and display header ---------------------------*
           EXEC SQL
               OPEN PRODAGG
           END-EXEC.
           
                  IF SQLCODE NOT = 0
           MOVE SQLCODE TO WS-SQLCODE-DISPLAY
           STRING 'SUBPROCA: Error opening cursor - SQLCODE: '
                  WS-SQLCODE-DISPLAY DELIMITED BY SIZE
              INTO LS-RESULT-BUFFER
           END-STRING
           EXIT PROGRAM
           END-IF.

      *--- Write table header to stats file ------------------------*
      
           DISPLAY '//////////////////////////////////////////////////'.
           DISPLAY '// Product                 | 2025 | 2024 | 2023 //'.
           DISPLAY '//////////////////////////////////////////////////'.
       
      *--- Fetch and display aggregated data ------------------------*
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH PRODAGG
                   INTO :HV-PRODUCT-NO,
                        :HV-DESCRIPTION,
                        :HV-QTY-2025,
                        :HV-QTY-2024,
                        :HV-QTY-2023
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-PRODUCT-COUNT
                   
      *            Format quantities for display
                   MOVE HV-QTY-2025 TO ED-QTY-2025
                   MOVE HV-QTY-2024 TO ED-QTY-2024
                   MOVE HV-QTY-2023 TO ED-QTY-2023
                   
      *            Build and display product line (compact format)
                   STRING '// ' DELIMITED BY SIZE 
                          HV-DESCRIPTION(1:20) DELIMITED BY SIZE
                          ' - ' DELIMITED BY SIZE
                          HV-PRODUCT-NO DELIMITED BY SIZE
                          ' | ' DELIMITED BY SIZE
                          ED-QTY-2025 DELIMITED BY SIZE
                          ' | ' DELIMITED BY SIZE
                          ED-QTY-2024 DELIMITED BY SIZE
                          ' | ' DELIMITED BY SIZE
                          ED-QTY-2023 DELIMITED BY SIZE
                          ' //' DELIMITED BY SIZE
                          INTO WS-DISPLAY-LINE
                   END-STRING
                   DISPLAY WS-DISPLAY-LINE
               ELSE
                   IF SQLCODE NOT = 100
                     DISPLAY 'SUBPROCA: Fetch error - SQLCODE: ' SQLCODE
                   END-IF
               END-IF
           END-PERFORM.
       
      *--- Close cursor and build result message --------------------*
           EXEC SQL
               CLOSE PRODAGG
           END-EXEC.
           
           DISPLAY '//////////////////////////////////////////////////'.

      *--- Return summary to MAINDRV --------------------------------*
           STRING 'SUBPROCA: Quantity aggregation complete - '
                  WS-PRODUCT-COUNT ' products processed'
                  DELIMITED BY SIZE INTO LS-RESULT-BUFFER
           END-STRING.
           
           EXIT PROGRAM.
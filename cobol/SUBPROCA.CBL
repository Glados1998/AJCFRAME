       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUBPROCA.
      *****************************************************************
      * SUBPROCA - SELECT ALL RECORDS FROM API4.ITEMS TABLE          *
      * RETURNS FORMATTED RESULT TO MAINDRV FOR WRITING TO FILES     *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *    SQL COMMUNICATION AREA
           EXEC SQL
               INCLUDE SQLCA
           END-EXEC.
       
      *    INCLUDE DCLGEN FOR ITEMS TABLE
           EXEC SQL
               INCLUDE DCLITEMS
           END-EXEC.
       
       01  WS-RECORD-COUNT     PIC 9(5) VALUE 0.
       01  WS-RESULT-LINE      PIC X(100).
       
      *    CURSOR DECLARATION
           EXEC SQL
               DECLARE ITEMSCUR CURSOR FOR
               SELECT O_NO, P_NO, QUANTITY, PRICE
               FROM API4.ITEMS
           END-EXEC.
       
       LINKAGE SECTION.
       01  LS-RESULT-BUFFER    PIC X(100).
       
       PROCEDURE DIVISION USING LS-RESULT-BUFFER.
       
           PERFORM OPEN-CURSOR.
           IF SQLCODE = 0
               PERFORM FETCH-RECORDS
               PERFORM CLOSE-CURSOR
               STRING 'SUBPROCA: Processed ' WS-RECORD-COUNT 
                      ' records from API4.ITEMS'
                   DELIMITED BY SIZE INTO LS-RESULT-BUFFER
           ELSE
               STRING 'SUBPROCA: ERROR opening cursor - SQLCODE: '
                      SQLCODE
                   DELIMITED BY SIZE INTO LS-RESULT-BUFFER
           END-IF.
           
           EXIT PROGRAM.
       
       OPEN-CURSOR.
           EXEC SQL
               OPEN ITEMSCUR
           END-EXEC.
           IF SQLCODE NOT = 0
               DISPLAY 'ERROR OPENING CURSOR - SQLCODE: ' SQLCODE
           END-IF.
           EXIT.
       
       FETCH-RECORDS.
           PERFORM UNTIL SQLCODE NOT = 0
               EXEC SQL
                   FETCH ITEMSCUR
                   INTO :ITMO-NO,
                        :ITMP-NO,
                        :ITMQUANTITY,
                        :ITMPRICE
               END-EXEC
               
               IF SQLCODE = 0
                   ADD 1 TO WS-RECORD-COUNT
                   DISPLAY 'Order: ' ITMO-NO ' | ' 
                           'Part: ' ITMP-NO ' | ' 
                           'Qty: ' ITMQUANTITY ' | '
                           'Price: ' ITMPRICE
               ELSE
                   IF SQLCODE = 100
                       DISPLAY 'End of data - Total records: ' 
                               WS-RECORD-COUNT
                   ELSE
                       DISPLAY 'ERROR FETCHING - SQLCODE: ' SQLCODE
                   END-IF
               END-IF
           END-PERFORM.
           EXIT.
       
       CLOSE-CURSOR.
           EXEC SQL
               CLOSE ITEMSCUR
           END-EXEC.
           IF SQLCODE NOT = 0
               DISPLAY 'ERROR CLOSING CURSOR - SQLCODE: ' SQLCODE
           END-IF.
           EXIT.

       CBL LIB DYNAM
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TSTANOML.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 L-SEP           PIC X(50) VALUE ALL "*".
       01 TEST-NAME       PIC X(30).
       01 EXPECTED-RC     PIC S9(3)V99.
       01 ACTUAL-RC       PIC S9(3)V99.
       01 WS-LOG-RECORD   PIC X(80).
       01 WS-FILE-STATUS  PIC XX.

       COPY TESTCONT.
       COPY ANOMLOG.
       
       
       PROCEDURE DIVISION.
           DISPLAY L-SEP
           DISPLAY '*** ANOMLOG UNIT TESTS ***'
           DISPLAY L-SEP
      
           PERFORM TEST-ANOMALY-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-ERROR-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-WARNING-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-FATAL-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-ABEND-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-INFO-MESSAGE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-HIGH-RETURN-CODE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           PERFORM TEST-ZERO-RETURN-CODE
           DISPLAY 'RUN>', TESTS-RUN, ' OK>', PASSES, ' KO>', FAILURES
           DISPLAY L-SEP
      
           DISPLAY '*** ALL TESTS COMPLETED ***'
           DISPLAY 'TOTAL RUN: ', TESTS-RUN
           DISPLAY 'TOTAL PASSED: ', PASSES
           DISPLAY 'TOTAL FAILED: ', FAILURES
           DISPLAY L-SEP
      
           GOBACK.
      
       TEST-ANOMALY-MESSAGE.
           MOVE 'TEST-ANOMALY-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 8 TO AL-RETURN-CODE
           MOVE 'Database connection failed unexpectedly'
                TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
         TEST-ERROR-MESSAGE.
           MOVE 'TEST-ERROR-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'ERROR' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 12 TO AL-RETURN-CODE
           MOVE 'File not found error occurred'
                TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-WARNING-MESSAGE.
           MOVE 'TEST-WARNING-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'WARNING' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 4 TO AL-RETURN-CODE
           MOVE 'Memory usage high' TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-FATAL-MESSAGE.
           MOVE 'TEST-FATAL-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'FATAL' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 16 TO AL-RETURN-CODE
           MOVE 'Critical system failure detected'
                TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-ABEND-MESSAGE.
           MOVE 'TEST-ABEND-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'ABEND' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 999 TO AL-RETURN-CODE
           MOVE 'Program abnormal termination S0C4'
                TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-INFO-MESSAGE.
           MOVE 'TEST-INFO-MESSAGE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'INFO' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 0 TO AL-RETURN-CODE
           MOVE 'Processing started successfully'
                TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-HIGH-RETURN-CODE.
           MOVE 'TEST-HIGH-RETURN-CODE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'ERROR' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 9999 TO AL-RETURN-CODE
           MOVE 'Maximum return code test' TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
       TEST-ZERO-RETURN-CODE.
           MOVE 'TEST-ZERO-RETURN-CODE' TO TEST-NAME
           DISPLAY '******** ', TEST-NAME, ' *********'
      
           
      
           MOVE 'INFO' TO AL-MESSAGE-TYPE
           MOVE 'TESTANOML' TO AL-PROGRAM-NAME
           MOVE 'TESTUSER' TO AL-USER-ID
           MOVE 0 TO AL-RETURN-CODE
           MOVE 'Process completed normally' TO AL-DESCRIPTION
      
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
      
           MOVE 0 TO EXPECTED-RC
           MOVE 0 TO ACTUAL-RC
      
           CALL 'ASSEQ' USING TEST-CONTEXT, TEST-NAME,
                              EXPECTED-RC, ACTUAL-RC.
      
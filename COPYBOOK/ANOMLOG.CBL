      * ANOMLOG.CPY - Parameter block for ANOMLOG logging subprogram
      * 'ABEND','FATAL','ERROR','WARNING', 'ANOMALY','INFO'
                                 
       01  ANOMLOG-PARM-BLOCK.
           05  AL-MESSAGE-TYPE    PIC X(8).    
           05  AL-PROGRAM-NAME    PIC X(8).
           05  AL-USER-ID         PIC X(8).
           05  AL-RETURN-CODE     PIC 9(4).
           05  AL-DESCRIPTION     PIC X(100).

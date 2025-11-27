       CBL DYNAM, LIB
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PROJET1.
       AUTHOR. JCH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NEWPRODS ASSIGN TO NPDATA
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-NPRODDATA.

           SELECT ERRORLOG ASSIGN TO ERRLOG
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-ERRLOG.

           SELECT CHANGEFILE ASSIGN TO CHNGFILE
           ACCESS MODE IS RANDOM
           ORGANIZATION IS INDEXED
           RECORD KEY IS DEVISE
           FILE STATUS IS FS-CHNGFILE.
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  NEWPRODS.
       01  ENR-NEWPRODS.
           05 PROD-LINE              PIC         X(50).

       FD  ERRORLOG.
       01  ENR-ERRORLOG.
           05 ERROR-LINE             PIC         X(1000).

       FD  CHANGEFILE.
       01  ENR-CHNGLINE.
      *    05 DEVISE                 PIC         X(3).
      *    05 TAUX                   PIC         X(5).
           05 DEVISE                 PIC         X(3).
           05 TAUX                   PIC         X(7).
      *****************************************************************
       WORKING-STORAGE SECTION.

            EXEC SQL
                INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
                INCLUDE PRODUCTS
            END-EXEC.

       77 FS-NPRODDATA           PIC 99.
       77 FS-ERRLOG              PIC 99.
       77 FS-CHNGFILE            PIC 99.

       77 WS-EOF-NPRODDATA         PIC X(1) VALUE 'N'.
       77 WS-EOF-CHNGFILE          PIC X(1) VALUE 'N'.
       77 WS-ANO-STR               PIC X(3).
       77 WS-ANO-TRUNC-STR         PIC X(2).
       77 DESCRIPTION-STR          PIC X(27).
       77 PRICE-STR                PIC X(6).
       77 STOCK-STR                PIC X(3).
       77 DEVISE-FIC-STR           PIC X(3).

       77 COUNTDIGITS              PIC 9(3).
       77 COUNTSINGLEDIGIT         PIC 9(3).
       77 COUNTDOTS                PIC 9(3).
       77 COUNTLETTERS             PIC 9(3).

       77 WS-ANO               PIC 999  VALUE ZERO.
       77 DESCRIPTION          PIC X(27).
       77 PRICE                PIC 9(5)v99.
       77 STOCK                PIC 9(3)v9.

       77 CHNG                 PIC 999v99  VALUE 1.

       77 INSERTOK             PIC X(1).


       01  ANOMLOG-PARM-BLOCK.
           05  AL-MESSAGE-TYPE    PIC X(8).
           05  AL-PROGRAM-NAME    PIC X(8) VALUE 'PROJET1'.
           05  AL-RETURN-CODE     PIC 9(4).
           05  AL-DESCRIPTION     PIC X(100).

       PROCEDURE DIVISION.

           OPEN INPUT NEWPRODS.
           OPEN OUTPUT ERRORLOG.
           OPEN INPUT CHANGEFILE.

      *    READ MJBASE

           IF FS-NPRODDATA = 0 AND FS-ERRLOG = 0 AND FS-CHNGFILE = 0
               DISPLAY " fichier lus: " WS-EOF-NPRODDATA
               PERFORM READPRODUCTFILE UNTIL WS-EOF-NPRODDATA = 'Y'
               DISPLAY " FIN " WS-EOF-NPRODDATA
           ELSE

           EVALUATE FS-NPRODDATA
      *       WHEN ZERO
      *          DISPLAY "FILE NEWPRODS OPENED"
             WHEN 23
                DISPLAY "FILE NEWPRODS NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE NEWPRODS NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE NEWPRODS ' FS-NPRODDATA
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE 'ERR READING FILE NEWPRODS ' TO AL-DESCRIPTION     CRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

           EVALUATE FS-ERRLOG
      *       WHEN ZERO
      *          DISPLAY "FILE ERRORLOG OPENED"
             WHEN 23
                DISPLAY "FILE ERRORLOG NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE ERRORLOG NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE ERRORLOG ' FS-ERRLOG
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE 'ERR READ FILE ERRORLOG ' TO AL-DESCRIPTION        ON
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

           EVALUATE FS-CHNGFILE
      *       WHEN ZERO
      *          DISPLAY "FILE CHNGFILE OPENED"
             WHEN 23
                DISPLAY "FILE CHNGFILE NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE CHNGFILE NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE CHNGFILE ' FS-CHNGFILE
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE 'ERR READ FILE CHNGFILE ' TO AL-DESCRIPTION        TION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

            PERFORM ABEND-PROG

           END-IF.

           CLOSE NEWPRODS.
           IF FS-NPRODDATA NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE NPRODATA : ' FS-NPRODDATA
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE 'ERR CLOSE NPRODATA : ' TO AL-DESCRIPTION           ON
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.
           CLOSE ERRORLOG.
           IF FS-ERRLOG NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE NPRODATA : ' FS-ERRLOG
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE 'ERR CLOSE NPRODATA : ' TO AL-DESCRIPTION
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.

           CLOSE CHANGEFILE.
           IF FS-CHNGFILE NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE NPRODATA : ' FS-CHNGFILE
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE 'ERR CLOSE NPRODATA : ' TO AL-DESCRIPTION           N
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.

           GOBACK.


       READPRODUCTFILE.
           READ NEWPRODS
                   AT END MOVE 'Y' TO WS-EOF-NPRODDATA
           END-READ
      *    RECUPERATION DE LA CHAINE PRINCIPALE
      *    EXPLOSION EN SOUS CHAINE  AVEC ;
      *     DISPLAY 'readproductfile: ' PROD-LINE
           UNSTRING PROD-LINE
           DELIMITED BY ';'
           INTO   WS-ANO-STR,
                  DESCRIPTION-STR,
                  PRICE-STR,
                  DEVISE-FIC-STR,
                  STOCK-STR
           END-UNSTRING
      *    DISPLAY WS-ANO-STR ';' DESCRIPTION-STR ';' DEVISE-FIC-STR
      *    DISPLAY PRICE-STR ';' STOCK-STR
      *    CONVERSION DE LA MONNAIE EN $

           MOVE 0 TO COUNTDIGITS
           MOVE 0 TO COUNTLETTERS
           MOVE 0 TO COUNTDOTS
           MOVE 'Y' TO INSERTOK
           INSPECT PRICE-STR TALLYING COUNTDIGITS FOR
           ALL "1"
           ALL "2"
           ALL "3"
           ALL "4"
           ALL "5"
           ALL "6"
           ALL "7"
           ALL "8"
           ALL "9"
           ALL "0"

           INSPECT PRICE-STR TALLYING COUNTLETTERS FOR
           ALL "A"
           ALL "B"
           ALL "C"
           ALL "D"
           ALL "E"
           ALL "F"
           ALL "G"
           ALL "H"
           ALL "I"
           ALL "J"
           ALL "K"
           ALL "L"
           ALL "M"
           ALL "O"
           ALL "P"
           ALL "Q"
           ALL "R"
           ALL "S"
           ALL "T"
           ALL "U"
           ALL "V"
           ALL "W"
           ALL "X"
           ALL "Y"
           ALL "Z"
           ALL "a"
           ALL "b"
           ALL "c"
           ALL "d"
           ALL "e"
           ALL "f"
           ALL "g"
           ALL "h"
           ALL "i"
           ALL "j"
           ALL "k"
           ALL "l"
           ALL "m"
           ALL "n"
           ALL "o"
           ALL "p"
           ALL "q"
           ALL "r"
           ALL "s"
           ALL "t"
           ALL "u"
           ALL "v"
           ALL "w"
           ALL "x"
           ALL "y"
           ALL "z"


           INSPECT PRICE-STR TALLYING COUNTDOTS FOR
           ALL "."
           INSPECT PRICE-STR REPLACING ALL "." BY ","
      *    DISPLAY 'COUNT DIGITS' COUNTDIGITS ';DOTS' COUNTDOTS
      *    DISPLAY PRICE-STR
           IF COUNTDIGITS > 0 AND COUNTLETTERS = 0
                         COMPUTE PRICE = FUNCTION NUMVAL-C(PRICE-STR)

      *                  DISPLAY "PRIX: " PRICE
                         PERFORM CHANGE

      *                  DISPLAY "CHNG" CHNG
                         COMPUTE PRICE = PRICE * CHNG

           ELSE
               STRING 'ERROR : PRIX ' DELIMITED BY SIZE,
                      PRICE-STR DELIMITED BY SIZE,
                      ' AU MAUVAIS FORMAT ' DELIMITED BY SIZE
               INTO ERROR-LINE
               END-STRING
               WRITE ENR-ERRORLOG
               MOVE 'N' TO INSERTOK
           END-IF



      *    MISE EN MINUSCULE (EXCEPTE PREMIERE LETTRE) DES NOMS

           MOVE FUNCTION LOWER-CASE(DESCRIPTION-STR) TO DESCRIPTION.


           INSPECT DESCRIPTION REPLACING
           ALL " a" BY " A"
           ALL " b" BY " B"
           ALL " c" BY " C"
           ALL " c" BY " C"
           ALL " d" BY " D"
           ALL " e" BY " E"
           ALL " f" BY " F"
           ALL " g" BY " G"
           ALL " h" BY " H"
           ALL " i" BY " I"
           ALL " k" BY " K"
           ALL " l" BY " L"
           ALL " m" BY " M"
           ALL " n" BY " N"
           ALL " o" BY " O"
           ALL " p" BY " P"
           ALL " q" BY " Q"
           ALL " r" BY " R"
           ALL " s" BY " S"
           ALL " t" BY " T"
           ALL " u" BY " U"
           ALL " v" BY " V"
           ALL " w" BY " W"
           ALL " x" BY " X"
           ALL " y" BY " Y"
           ALL " z" BY " Z"

      *    INSERTION EN BASE
           MOVE 0 TO COUNTDIGITS
           INSPECT WS-ANO-STR TALLYING COUNTDIGITS FOR
           ALL "1"
           ALL "2"
           ALL "3"
           ALL "4"
           ALL "5"
           ALL "6"
           ALL "7"
           ALL "8"
           ALL "9"
           ALL "0"
      *    DISPLAY WS-ANO-STR(1:1) ";" COUNTDIGITS
           IF COUNTDIGITS NOT = 2 OR WS-ANO-STR(1:1) NOT = 'P'
              STRING 'ERROR : IDENTIFIANT' DELIMITED BY SIZE,
                     WS-ANO-STR DELIMITED BY SIZE,
                     ' AU MAUVAIS FORMAT ' DELIMITED BY SIZE
              INTO ERROR-LINE
              END-STRING
              WRITE ENR-ERRORLOG
              MOVE 'N' TO INSERTOK
           END-IF


           MOVE 0 TO COUNTDIGITS
           INSPECT STOCK-STR TALLYING COUNTDIGITS FOR
           ALL "1"
           ALL "2"
           ALL "3"
           ALL "4"
           ALL "5"
           ALL "6"
           ALL "7"
           ALL "8"
           ALL "9"
           ALL "0"

           MOVE 0 TO COUNTLETTERS
           INSPECT STOCK-STR TALLYING COUNTLETTERS FOR
           ALL "A"
           ALL "B"
           ALL "C"
           ALL "D"
           ALL "E"
           ALL "F"
           ALL "G"
           ALL "H"
           ALL "I"
           ALL "J"
           ALL "K"
           ALL "L"
           ALL "M"
           ALL "N"
           ALL "O"
           ALL "P"
           ALL "Q"
           ALL "R"
           ALL "S"
           ALL "T"
           ALL "U"
           ALL "V"
           ALL "W"
           ALL "X"
           ALL "Y"
           ALL "Z"
           ALL "a"
           ALL "b"
           ALL "c"
           ALL "d"
           ALL "e"
           ALL "f"
           ALL "g"
           ALL "h"
           ALL "i"
           ALL "j"
           ALL "k"
           ALL "l"
           ALL "m"
           ALL "n"
           ALL "o"
           ALL "p"
           ALL "q"
           ALL "r"
           ALL "s"
           ALL "t"
           ALL "u"
           ALL "v"
           ALL "w"
           ALL "x"
           ALL "y"
           ALL "z"

      *    DISPLAY STOCK-STR ';' COUNTDIGITS ';' COUNTDOTS
           IF COUNTDIGITS > 0 AND COUNTLETTERS = 0
              COMPUTE STOCK = FUNCTION NUMVAL(STOCK-STR)

           ELSE
              STRING 'ERROR : STOCK' DELIMITED BY SIZE,
                      STOCK-STR DELIMITED BY SIZE,
                    ' AU MAUVAIS FORMAT ' DELIMITED BY SIZE
              INTO ERROR-LINE
              END-STRING
              WRITE ENR-ERRORLOG
              MOVE 'N' TO INSERTOK
           END-IF
      *     DISPLAY "LIGNE a INSERER :"
      *     DISPLAY WS-ANO ' ; ' DESCRIPTION ' ; ' PRICE ' ; ' STOCK
      *     DISPLAY "INSERT: " INSERTOK.
           IF INSERTOK = 'Y'
               PERFORM SQL-ADD
           END-IF.


       CHANGE.
            MOVE DEVISE-FIC-STR TO DEVISE
            READ CHANGEFILE
                 KEY IS DEVISE
            END-READ
      *     DISPLAY "Devises : " DEVISE ";TAUX " TAUX
            INSPECT TAUX REPLACING ALL "." BY ","
            COMPUTE CHNG = FUNCTION NUMVAL-C(TAUX).


       SQL-ADD.
            MOVE WS-ANO-STR TO PRODUCTS-P-NO
            MOVE DESCRIPTION TO PRODUCTS-DESCRIPTION-TEXT
            MOVE 30 TO PRODUCTS-DESCRIPTION-LEN
            MOVE PRICE TO PRODUCTS-PRICE
            MOVE STOCK TO PRODUCTS-STOCK
      *     DISPLAY "SQL-ADD EN COURS"
            EXEC SQL
               INSERT INTO API3.products VALUES
               ( :PRODUCTS-P-NO,
                 :PRODUCTS-DESCRIPTION,
                 :PRODUCTS-PRICE,
                 :PRODUCTS-STOCK
               )
            END-EXEC
            DISPLAY "INSERTION"
            DISPLAY SQLCODE ';' PRODUCTS-P-NO ';' PRODUCTS-DESCRIPTION
                    ';' PRODUCTS-PRICE ';' PRODUCTS-STOCK

              STRING 'REQUETE:INSERT INTO' DELIMITED BY SIZE,
                               'API3.PRODUCT.VALUE(' DELIMITED BY SIZE,
                                PRODUCTS-P-NO DELIMITED BY SIZE,
                                ',',
                                PRODUCTS-DESCRIPTION DELIMITED BY SIZE,
                                ',',
                                PRICE-STR DELIMITED BY SIZE,
                                ',',
                                STOCK-STR DELIMITED BY SIZE,
                                ').' DELIMITED BY SIZE,
                                INTO ERROR-LINE
              END-STRING



            EVALUATE SQLCODE
                WHEN ZERO
                    EXEC SQL
                      COMMIT
                    END-EXEC
                    DISPLAY " LIGNE AJOUTEE AVEC SUCCES ! "


                WHEN +100
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR 100 : la donnée existe déja'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG


                WHEN -305
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -305 : Une des valeurs est a NULL  '
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG

                WHEN -310
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -310:Donnees num au mauvais format'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG


                WHEN -313
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -313 : Incoherence entre le nombre '
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG

                   MOVE 'de variable et celui de colonnes'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG

                WHEN -502
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -502 : Curseur non ouvert'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG
                   PERFORM ABEND-PROG

                WHEN -503
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -503 : Curseur déja ouvert '
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG
                   PERFORM ABEND-PROG

                WHEN -532
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR -532 : Violation de clé étrangère'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG


                WHEN -803
                   DISPLAY "ERREUR:803"
                   WRITE ENR-ERRORLOG
                   MOVE 'ERROR 803 : Enregistrements en doublons'
                   TO ERROR-LINE
                   WRITE ENR-ERRORLOG


                WHEN OTHER
                   WRITE ENR-ERRORLOG
                   STRING 'AUTRE ERREUR:' DELIMITED BY SIZE
                   INTO ERROR-LINE
                   END-STRING
                   WRITE ENR-ERRORLOG
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE ERROR-LINE TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG

            END-EVALUATE.


       100-EXIT.
           EXIT.


       TRT-INEX.
           DISPLAY 'FICHIER MISE A JOUR INEXISTANT.'.

       ABEND-PROG.
           DISPLAY 'ABENDED!!!'
           STRING 'ABENDED' DELIMITED BY SIZE
           INTO ERROR-LINE
           END-STRING
           WRITE ENR-ERRORLOG
           MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
           MOVE 12 TO AL-RETURN-CODE
           MOVE 'ABENDED' TO AL-DESCRIPTION
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           COMPUTE WS-ANO = 1 / WS-ANO.

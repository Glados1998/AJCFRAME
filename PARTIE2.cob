       CBL DYNAM, LIB
       IDENTIFICATION DIVISION.
       PROGRAM-ID.   PROJET2.
       AUTHOR. JCH.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VENTESAS ASSIGN TO VTSAS
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-VENTESAS.

           SELECT VENTESEU ASSIGN TO VTEU
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-VENTESEU.

           SELECT STOCKS ASSIGN TO STCKS
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-STOCKS.

           SELECT REAPPRO ASSIGN TO REAPPRO
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-REAPPRO.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD  VENTESAS.
       01  ENR-VENTESAS.
           05 NCOMMANDEAS             PIC         9(3).
           05 DATECOMMANDEAS          PIC         X(10).
           05 NEMPLOYEAS              PIC         9(2).
           05 NCLIENTAS               PIC         9(4).
           05 NPRODUITAS              PIC         X(3).
           05 PRIXAS                  PIC         9(3)V99.
           05 QUANTITECOMMANDEEAS     PIC         9(2).
           05 RESERVEDAS              PIC         X(6).

       FD  VENTESEU.
       01  ENR-VENTESEU.
           05 NCOMMANDEEU             PIC         9(3).
           05 DATECOMMANDEEU          PIC         X(10).
           05 NEMPLOYEEU              PIC         9(2).
           05 NCLIENTEU               PIC         9(4).
           05 NPRODUITEU              PIC         X(3).
           05 PRIXEU                  PIC         9(3)V99.
           05 QUANTITECOMMANDEEEU     PIC         9(2).
           05 RESERVEDEU              PIC         X(6).

       FD  STOCKS.
       01  ENR-STOCKS.
           05 SNUMPRODUIT             PIC         X(3).
           05 SSEUILREAPPRO           PIC         9(3).
           05 SQUANTITEACOMMANDER     PIC         9(2).
           05 STAUX                   PIC         X(7).

       FD  REAPPRO.
       01  ENR-REAPPRO.
           05 RNUMPRODUIT            PIC         X(3).
           05 RQUANTITEACOMMANDER    PIC         9(3).
           05 FILLER                 PIC         X(9).
      *****************************************************************
       WORKING-STORAGE SECTION.

            EXEC SQL
                INCLUDE SQLCA
            END-EXEC.

            EXEC SQL
                INCLUDE PRODUCTS
            END-EXEC.

            EXEC SQL
                INCLUDE CUST
            END-EXEC.

            EXEC SQL
                INCLUDE ORDERS
            END-EXEC.

            EXEC SQL
                INCLUDE ITEMS
            END-EXEC.

            EXEC SQL
             DECLARE CBALANCE CURSOR FOR
                SELECT BALANCE,C_NO
                  FROM API3.CUSTOMERS
            END-EXEC.

            EXEC SQL
             DECLARE CSTOCKS CURSOR FOR
                SELECT stock,P_NO
                  FROM API3.PRODUCTS
            END-EXEC.

            EXEC SQL
             DECLARE CPRODUITS CURSOR FOR
                SELECT PRICE,P_NO
                  FROM API3.PRODUCTS
            END-EXEC.

       77 FS-VENTESAS           PIC 99.
       77 FS-VENTESEU           PIC 99.
       77 FS-REAPPRO            PIC 99.
       77 FS-STOCKS             PIC 99.

       77 WS-EOF-VENTESAS          PIC X(1).
       77 WS-EOF-VENTESEU          PIC X(1).
       77 WS-EOF-REAPPRO           PIC X(1).
       77 WS-EOF-STOCKS            PIC X(1).

       77 STOCK-PRODUIT            PIC 9(3).

       77 IDPRODREAPPRO            PIC X(3).
       77 QUANTITEACOMMANDER       PIC 9(4).
       77 IDCLIENTMAJSOLDE         PIC 9(4).
       77 SOLDECOMMANDE            PIC 9(5)V99.
       77 BALANCE                  PIC 9(10)V99.
       77 IDPRODUITSTOCKS          PIC X(3).

       77 STOCKPRODUITENCOURS      PIC 9(3).
       77 IDPRODUITENCOURS         PIC X(3).
       77 QCPRODUITENCOURS         PIC 9(2).

       77 WS-ANO                   PIC 999  VALUE ZERO.
       77 DATECONV                 PIC X(10).

       77 CONTINUEINSERT           PIC X(1) VALUE 'Y'.

       01  ANOMLOG-PARM-BLOCK.
           05  AL-MESSAGE-TYPE    PIC X(8).
           05  AL-PROGRAM-NAME    PIC X(8) VALUE 'PROJET2'.
           05  AL-RETURN-CODE     PIC 9(4).
           05  AL-DESCRIPTION     PIC X(100).

       77 PRIXCONV  PIC 999.99.
       77 PRIXSTR PIC X(6).

       PROCEDURE DIVISION.

           OPEN INPUT VENTESAS.
           OPEN INPUT VENTESEU.
           OPEN INPUT STOCKS.
           OPEN OUTPUT REAPPRO.

      *    READ MJBASE

           IF FS-VENTESAS = 0 AND FS-VENTESEU = 0 AND FS-STOCKS = 0
      *        DISPLAY " fichier lus: " WS-EOF-VENTESAS
               DISPLAY " LECTURE FICHIER ASIE "
               PERFORM READASFILE UNTIL WS-EOF-VENTESAS = 'Y'
               DISPLAY " LECTURE FICHIER EUROPE "
               PERFORM READEUFILE UNTIL WS-EOF-VENTESEU = 'Y'
               DISPLAY " FIN " WS-EOF-VENTESAS
           ELSE

           EVALUATE FS-VENTESAS
      *       WHEN ZERO
      *          DISPLAY "FILE NEWPRODS OPENED"
             WHEN 23
                DISPLAY "FILE NEWPRODS NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE NEWPRODS NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE NEWPRODS ' FS-VENTESAS
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE NEWPRODS NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

           EVALUATE FS-VENTESEU
      *       WHEN ZERO
      *          DISPLAY "FILE ERRORLOG OPENED"
             WHEN 23
                DISPLAY "FILE ERRORLOG NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE ERRORLOG NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE ERRORLOG ' FS-VENTESEU
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "ERR READ FILE ERRORLOG" TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

           EVALUATE FS-STOCKS
      *       WHEN ZERO
      *          DISPLAY "FILE CHNGFILE OPENED"
             WHEN 23
                DISPLAY "FILE CHNGFILE NONEXISTENT "
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "FILE CHNGFILE NONEXISTENT " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
             WHEN OTHER
                DISPLAY 'ERR READ FILE CHNGFILE ' FS-STOCKS
                MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                MOVE 12 TO AL-RETURN-CODE
                MOVE "ERR READ FILE CHNGFILE " TO AL-DESCRIPTION
                CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           END-EVALUATE

            PERFORM ABEND-PROG

           END-IF.

           CLOSE VENTESAS.
           IF FS-VENTESAS NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE VENTESAS : ' FS-VENTESAS
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE "ERR CLOSE VENTESAS :  " TO AL-DESCRIPTION
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.
           CLOSE VENTESEU.
           IF FS-VENTESEU NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE VENTESEU : ' FS-VENTESEU
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE "ERR CLOSE VENTESEU :  " TO AL-DESCRIPTION
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.

           CLOSE STOCKS.
           IF FS-STOCKS NOT EQUAL ZERO THEN
               DISPLAY 'ERR CLOSE STOCKS : ' FS-STOCKS
               MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
               MOVE 12 TO AL-RETURN-CODE
               MOVE "ERR CLOSE STOCKS :  " TO AL-DESCRIPTION
               CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
               PERFORM ABEND-PROG
           END-IF.

           GOBACK.


       READASFILE.
           MOVE 'Y' TO CONTINUEINSERT
      * LECTURE FICHIER
           READ VENTESAS
                   AT END MOVE 'Y' TO WS-EOF-VENTESAS
           END-READ
      * AJOUT DES VENTES EN BASE
      *01  ENR-VENTESAS.
      *     05 NCOMMANDEAS             PIC         9(3).
      *     05 DATECOMMANDEAS          PIC         X(10).
      *     05 NEMPLOYEAS              PIC         9(2).
      *     05 NCLIENTAS               PIC         9(4).
      *     05 NPRODUITAS              PIC         X(3).
      *     05 PRIXAS                  PIC         9(3)V99.
      *     05 QUANTITECOMMANDEEAS     PIC         9(2).
      *     05 RESERVEDAS              PIC         X(6).
             MOVE NCOMMANDEAS TO ORDERS-O-NO
             MOVE NCOMMANDEAS TO ITEMS-O-NO
             MOVE NEMPLOYEAS  TO ORDERS-S-NO
             MOVE NCLIENTAS TO ORDERS-C-NO
             MOVE QUANTITECOMMANDEEAS  TO ITEMS-QUANTITY
             MOVE PRIXAS TO ITEMS-PRICE
             MOVE NPRODUITAS TO IDPRODUITENCOURS
             MOVE NPRODUITAS TO ITEMS-P-NO

              MOVE PRIXAS TO PRIXCONV
              MOVE PRIXCONV TO PRIXSTR
      *       DISPLAY IDPRODUITENCOURS ':'  PRIXSTR
             IF PRIXSTR = '000.00'

                PERFORM CHECKPRICE

             END-IF

             STRING DATECOMMANDEAS(7:) DELIMITED BY SIZE,
                    '-' DELIMITED BY SIZE,
                    DATECOMMANDEAS(4:2) DELIMITED BY SIZE,
                    '-' DELIMITED BY SIZE,
                    DATECOMMANDEAS(1:2) DELIMITED BY SIZE,
               INTO DATECONV
             END-STRING
             MOVE DATECONV TO ORDERS-O-DATE

             DISPLAY DATECONV
             DISPLAY 'ASIE: ' NCOMMANDEAS ';' DATECOMMANDEAS
             DISPLAY NCLIENTAS ';' NPRODUITAS ';' ITEMS-PRICE
             DISPLAY NEMPLOYEAS ';' QUANTITECOMMANDEEAS
             EXEC SQL
                INSERT INTO API3.ORDERS VALUES
                ( :ORDERS-O-NO,
                  :ORDERS-S-NO,
                  :ORDERS-C-NO,
                  :ORDERS-O-DATE
                )
             END-EXEC
             PERFORM SQL-VERIFY
             DISPLAY "ASIE:INSERTION DANS LA TABLE ORDERS"
      ***    DISPLAY "ITEMS:" ITEMS-O-NO ";" ITEMS-P-NO
      *****  DISPLAY  ITEMS-QUANTITY ";" ITEMS-PRICE
             IF CONTINUEINSERT = 'Y'
              EXEC SQL
                INSERT INTO API3.ITEMS VALUES
                ( :ITEMS-O-NO,
                  :ITEMS-P-NO,
                  :ITEMS-QUANTITY,
                  :ITEMS-PRICE
                )
              END-EXEC
              PERFORM SQL-VERIFY
              DISPLAY "ASIE:INSERTION DANS LA TABLE ITEMS"
             END-IF

      * AUGMENTATION SOLDE CLIENT
            IF CONTINUEINSERT = 'Y'
               DISPLAY "ASIE:MAJ DU SOLDE CLIENT"
               MOVE ORDERS-C-NO TO IDCLIENTMAJSOLDE
               MOVE ITEMS-PRICE TO SOLDECOMMANDE
               PERFORM MAJSOLDECLIENT
            END-IF

      * GESTION DU STOCK

            IF CONTINUEINSERT = 'Y'
               DISPLAY "ASIE:GESTION DU STOCK"
               MOVE NPRODUITAS TO IDPRODUITENCOURS
               MOVE QUANTITECOMMANDEEAS TO QCPRODUITENCOURS
               PERFORM STOCKSMGMT
            END-IF.

       READEUFILE.
            MOVE 'Y' TO CONTINUEINSERT
      *        LECTURE FICHIER
            READ VENTESEU
                    AT END MOVE 'Y' TO WS-EOF-VENTESEU
            END-READ
      * AJOUT DES VENTES EN BASE

             MOVE NCOMMANDEEU TO ORDERS-O-NO
             MOVE NCOMMANDEEU TO ITEMS-O-NO
             MOVE NEMPLOYEEU  TO ORDERS-S-NO
             MOVE NCLIENTEU TO ORDERS-C-NO
             MOVE QUANTITECOMMANDEEEU  TO ITEMS-QUANTITY
             MOVE PRIXEU TO ITEMS-PRICE
             MOVE NPRODUITEU TO IDPRODUITENCOURS
             MOVE NPRODUITEU TO ITEMS-P-NO

             MOVE PRIXEU TO PRIXCONV
             MOVE PRIXCONV TO PRIXSTR
      *      DISPLAY IDPRODUITENCOURS ':'  PRIXSTR
             IF PRIXSTR = '000.00'
      *          DISPLAY "EUROPE:RECUPERATION DU PRIX EN BASE"
                 PERFORM CHECKPRICE

             END-IF

            STRING DATECOMMANDEEU(7:) DELIMITED BY SIZE,
                   '-' DELIMITED BY SIZE,
                   DATECOMMANDEEU(4:2) DELIMITED BY SIZE,
                   '-' DELIMITED BY SIZE,
                   DATECOMMANDEEU(1:2) DELIMITED BY SIZE,
                INTO DATECONV
            END-STRING
            MOVE DATECONV TO ORDERS-O-DATE


            DISPLAY 'EUROPE: ' NCOMMANDEEU ';' DATECOMMANDEEU ';'
            DISPLAY NCLIENTEU ';' NPRODUITEU ';' ITEMS-PRICE ';'
            DISPLAY QUANTITECOMMANDEEEU ';' NEMPLOYEEU
            IF CONTINUEINSERT = 'Y'
             EXEC SQL
                INSERT INTO API3.ORDERS VALUES
                ( :ORDERS-O-NO,
                  :ORDERS-S-NO,
                  :ORDERS-C-NO,
                  :ORDERS-O-DATE
                )
             END-EXEC
             PERFORM SQL-VERIFY
             DISPLAY "EUROPE:INSERTION DANS LA TABLE ORDERS"
            END-IF

      *      DISPLAY "INSERT" ITEMS-O-NO ";" ITEMS-P-NO

            IF CONTINUEINSERT = 'Y'
                 EXEC SQL
                    INSERT INTO API3.ITEMS VALUES
                    ( :ITEMS-O-NO,
                      :ITEMS-P-NO,
                      :ITEMS-QUANTITY,
                      :ITEMS-PRICE
                    )
                END-EXEC
                PERFORM SQL-VERIFY
                DISPLAY "EUROPE:INSERTION DANS LA TABLE ITEMS"
            END-IF

      * AUGMENTATION SOLDE CLIENT
            IF CONTINUEINSERT = 'Y'
                DISPLAY "EUROPE:MAJ DU SOLDE CLIENT"
                MOVE  ORDERS-C-NO TO IDCLIENTMAJSOLDE
                MOVE  ITEMS-PRICE TO SOLDECOMMANDE
                PERFORM MAJSOLDECLIENT
            END-IF
      * GESTION DU STOCK
            IF CONTINUEINSERT = 'Y'
                DISPLAY "EUROPE:GESTION DU STOCK"
                MOVE NPRODUITEU TO IDPRODUITENCOURS
                MOVE QUANTITECOMMANDEEEU TO QCPRODUITENCOURS
                PERFORM STOCKSMGMT
            END-IF.

       STOCKSMGMT.
      *RECUPERATION DU STOCK PRODUIT
      *    EXEC SQL
      *       SELECT stock
      *          into :PRODUCTS-STOCK
      *       FROM API3.PRODUCTS
      *       WHERE PNO = IDPRODUITSTOCKS
      *    END-EXEC
      *      PERFORM SQL-VERIFY
      *RECUPERATION DES SEUIL STOCK PRODUIT

           EXEC SQL
                   OPEN CSTOCKS
           END-EXEC.

           PERFORM UNTIL SQLCODE NOT EQUAL ZERO

               EXEC SQL
                 FETCH CSTOCKS
                     INTO :PRODUCTS-STOCK,
                          :PRODUCTS-P-NO
               END-EXEC

               EVALUATE SQLCODE
                    WHEN 0
                       IF PRODUCTS-P-NO =  IDPRODUITENCOURS
                           MOVE PRODUCTS-STOCK TO STOCKPRODUITENCOURS
                           PERFORM UPDATESTOCK
                       END-IF

                    WHEN 100
                         CONTINUE

                    WHEN OTHER
                        DISPLAY "DB2 ERROR CSTOCKS :" SQLCODE

               END-EVALUATE

           END-PERFORM

           EXEC SQL
                   CLOSE CSTOCKS
           END-EXEC.

       CHECKPRICE.
           EXEC SQL
                   OPEN CPRODUITS
           END-EXEC

           DISPLAY "CHECKPRICE"
           PERFORM UNTIL SQLCODE NOT EQUAL ZERO

               EXEC SQL
                 FETCH CPRODUITS
                     INTO :PRODUCTS-PRICE,
                          :PRODUCTS-P-NO
               END-EXEC

               EVALUATE SQLCODE
                    WHEN 0
      *                DISPLAY "0K; " PRODUCTS-P-NO ";" IDPRODUITENCOURS
                       IF PRODUCTS-P-NO = IDPRODUITENCOURS
                           MOVE PRODUCTS-PRICE TO ITEMS-PRICE
                           DISPLAY "O" ITEMS-PRICE ";" IDPRODUITENCOURS
                       END-IF

                    WHEN 100
                         CONTINUE

                    WHEN OTHER
                        DISPLAY "DB2 ERROR CPRODUITS:" SQLCODE

               END-EVALUATE

           END-PERFORM

           EXEC SQL
                   CLOSE CPRODUITS
           END-EXEC.


       UPDATESTOCK.
      *    MODIF STOCKS APRES COMMANDE
           DISPLAY "ID : " IDPRODUITENCOURS
           DISPLAY "STOCK : " STOCKPRODUITENCOURS
           SUBTRACT QCPRODUITENCOURS  FROM STOCKPRODUITENCOURS

           MOVE IDPRODUITENCOURS  TO PRODUCTS-P-NO
           MOVE STOCKPRODUITENCOURS TO PRODUCTS-STOCK

            EXEC SQL
               UPDATE API3.PRODUCTS
               SET STOCK = :PRODUCTS-STOCK
               WHERE P_NO =  :PRODUCTS-P-NO
            END-EXEC.
            PERFORM SQL-VERIFY-UPDATE

           DISPLAY "STOCKRESTANT : " PRODUCTS-STOCK

           IF CONTINUEINSERT = 'Y'
               PERFORM MANAGEREAPPRO UNTIL WS-EOF-STOCKS = 'Y'
           END-IF.

       MANAGEREAPPRO.
           DISPLAY "ECRITURE FICHIER REAPPRO"
      *LECTURE FICHIER
           READ STOCKS
                AT END MOVE 'Y' TO WS-EOF-STOCKS
           END-READ
      *
      *      DISPLAY "IDPRODUITENCOURS " IDPRODUITENCOURS
      *      DISPLAY "IDPRODUITSTOCKS" SNUMPRODUIT
      *      DISPLAY "STOCKPRODUIT" STOCKPRODUITENCOURS
      *      DISPLAY "SEUILREAPPRO" SSEUILREAPPRO
            IF IDPRODUITENCOURS = SNUMPRODUIT
             IF STOCKPRODUITENCOURS <= SSEUILREAPPRO

      *     05 NUMPRODUIT             PIC         X(3).
      *     05 SEUILREAPPRO           PIC         9(3).
      *     05 QUANTITEACOMMANDER     PIC         9(3).
      *     05 FILLER                 PIC         X(7).
      *    FD  REAPPRO
                    MOVE SNUMPRODUIT TO RNUMPRODUIT
                    MOVE SQUANTITEACOMMANDER TO RQUANTITEACOMMANDER
                    DISPLAY "QUANTITEACOMMANDER : " SQUANTITEACOMMANDER
                    WRITE ENR-REAPPRO
             END-IF
             END-IF.


       MAJSOLDECLIENT.
      * RECUPERATION DE l'EXISTANT
      *    EXEC SQL
      *       SELECT balance
      *          into :CUST-BALANCE
      *       FROM API3.CUSTOMERS
      *       WHERE CNO = :ORDERS-C-NO
      *    END-EXEC
           EXEC SQL
                   OPEN CBALANCE
           END-EXEC

           PERFORM UNTIL SQLCODE NOT EQUAL ZERO

               EXEC SQL
                 FETCH CBALANCE
                     INTO :CUST-BALANCE,
                          :CUST-C-NO
               END-EXEC

               EVALUATE SQLCODE
                    WHEN 0
                       IF CUST-C-NO = ORDERS-C-NO
                           MOVE CUST-BALANCE TO BALANCE
                           PERFORM UPDATECLIENTBALANCE
                       END-IF

                    WHEN 100
                         CONTINUE

                    WHEN OTHER
                        DISPLAY "DB2 ERROR CBALANCE:" SQLCODE

               END-EVALUATE

           END-PERFORM

           EXEC SQL
                   CLOSE CBALANCE
           END-EXEC.



       UPDATECLIENTBALANCE.
      *    PERFORM SQL-VERIFY
      *    MOVE  CUST-BALANCE TO BALANCE
           DISPLAY "UPDATE CLIENT BALANCE"
           DISPLAY "BALANCE AVANT" BALANCE
           ADD SOLDECOMMANDE TO BALANCE

           MOVE BALANCE TO  CUST-BALANCE
           DISPLAY 'BALANCE APRES' CUST-BALANCE
           DISPLAY 'CLIENT : ' IDCLIENTMAJSOLDE
           MOVE IDCLIENTMAJSOLDE TO CUST-C-NO
            EXEC SQL
               UPDATE API3.CUSTOMERS
               SET BALANCE = :CUST-BALANCE
               WHERE C_NO =  :CUST-C-NO
            END-EXEC
            PERFORM SQL-VERIFY-UPDATE.



       SQL-VERIFY.

           EVALUATE SQLCODE
                WHEN ZERO
                    EXEC SQL
                      COMMIT
                    END-EXEC
                    DISPLAY " LIGNE AJOUTEE AVEC SUCCES ! "


                WHEN +100
                   DISPLAY 'ERROR 100 : la donnée existe déja'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -305
                   DISPLAY 'ERROR -305 : Une des valeurs est a NULL  '
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -310
                   DISPLAY 'ERROR -310:Donnees num au mauvais format'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -313
                   DISPLAY 'ERROR -313 : Incoherence entre le nombre '
                   DISPLAY 'de variable et celui de colonnes'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -502
                   DISPLAY 'ERROR -502 : Curseur non ouvert'
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERREUR SQL FATALE:' TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -503
                   DISPLAY 'ERROR -503 : Curseur déja ouvert '
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERREUR SQL FATALE:' TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG

                WHEN -532
                   DISPLAY 'ERROR -532 : Violation de clé étrangère'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -803
                   DISPLAY "ERREUR:803"
                   DISPLAY 'ERROR 803 : Enregistrements en doublons'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN OTHER
                   DISPLAY 'AUTRE ERREUR:' SQLCODE
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERREUR SQL FATALE:' TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG

            END-EVALUATE.



       SQL-VERIFY-UPDATE.

           EVALUATE SQLCODE
                WHEN ZERO
                    EXEC SQL
                      COMMIT
                    END-EXEC
                    DISPLAY " LIGNE MISE A JOUR AVEC SUCCES!   "

                WHEN +100
      *             EXEC SQL
      *               COMMIT
      *             END-EXEC
                    DISPLAY "ERROR 100 : ENREGISTREMENT INTROUVABLE !"


                WHEN -305
                   DISPLAY 'ERROR -305 : Une des valeurs est a NULL  '
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -310
                   DISPLAY 'ERROR -310:Donnees num au mauvais format'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -313
                   DISPLAY 'ERROR -313 : Incoherence entre le nombre '
                   DISPLAY 'de variable et celui de colonnes'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -502
                   DISPLAY 'ERROR -502 : Curseur non ouvert'
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERROR -502 ' TO AL-DESCRIPTION                 
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -503
                   DISPLAY 'ERROR -503 : Curseur déja ouvert '
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERROR -503 : ' TO AL-DESCRIPTION               
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG

                WHEN -532
                   DISPLAY 'ERROR -532 : Violation de clé étrangère'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN -803
                   DISPLAY "ERREUR:803"
                   DISPLAY 'ERROR 803 : Enregistrements en doublons'
                   MOVE 'N' TO CONTINUEINSERT

                WHEN OTHER
                   DISPLAY 'AUTRE ERREUR:' SQLCODE
                   MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
                   MOVE 12 TO AL-RETURN-CODE
                   MOVE 'ERREUR SQL FATALE:' TO AL-DESCRIPTION
                   CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
                   PERFORM ABEND-PROG

            END-EVALUATE.



        100-EXIT.
           EXIT.



        TRT-INEX.
           DISPLAY 'FICHIER MISE A JOUR INEXISTANT.'.



        ABEND-PROG.
           DISPLAY 'ANOMALIE !!!'
           MOVE 'ANOMALY' TO AL-MESSAGE-TYPE
           MOVE 12 TO AL-RETURN-CODE
           MOVE 'ABNORMAL END !!!' TO AL-DESCRIPTION
           CALL 'ANOMLOG' USING ANOMLOG-PARM-BLOCK END-CALL
           COMPUTE WS-ANO = 1 / WS-ANO.

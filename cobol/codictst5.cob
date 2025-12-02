       IDENTIFICATION DIVISION.
       PROGRAM-ID. CODICTST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

       * Contexte de tests unitaires
       COPY TESTCONT.

       01  TEST-NAME           PIC X(30).
       01  EXPECTED            PIC S9(3)V99.
       01  ACTUAL              PIC S9(3)V99.

       * Données pour appeler COBG53DT
       01  WS-DATE-TEXT        PIC X(40).

       * Données pour construire la date attendue
       01  WS-DATE-6           PIC 9(6).
       01  WS-YY               PIC 99.
       01  WS-MM               PIC 99.
       01  WS-DD               PIC 99.
       01  WS-DOW              PIC 9.
       01  WS-YYYY             PIC 9(4).
       01  WS-YYYY-TXT         PIC X(4).
       01  WS-DD-TEXT.
           05 WS-DD-TEXT-CHARS PIC XX.
           05 WS-DD-LEN        PIC 9.
       01  WS-DAY-NAME         PIC X(9).
       01  WS-MONTH-NAME       PIC X(9).
       01  WS-EXPECTED-TEXT    PIC X(40).

       PROCEDURE DIVISION.

       MAIN-SECTION.
           PERFORM INIT-TEST-CONTEXT
           PERFORM TEST-DATE-TODAY
           PERFORM DISPLAY-SUMMARY
           STOP RUN
           .

       * Initialisation des compteurs de tests
       INIT-TEST-CONTEXT.
           MOVE 0 TO TESTS-RUN
           MOVE 0 TO PASSES
           MOVE 0 TO FAILURES
           .

       * Affichage d’un petit récap (optionnel)
       DISPLAY-SUMMARY.
           DISPLAY 'TESTS RUN : ' TESTS-RUN
           DISPLAY 'PASSES    : ' PASSES
           DISPLAY 'FAILURES  : ' FAILURES
           .

       * Test principal : vérifier que COBG53DT formate bien
       * la date du jour dans WS-DATE-TEXT.
       TEST-DATE-TODAY.
           MOVE 'COBG53DT-TODAY-DATE' TO TEST-NAME

           * Construire la valeur attendue en reproduisant
           * l’algorithme de COBG53DT
           PERFORM BUILD-EXPECTED-DATE

           * Appel du sous-programme à tester
           CALL 'COBG53DT' USING WS-DATE-TEXT

           * Comparaison : 0 = OK, 1 = KO
           IF WS-DATE-TEXT = WS-EXPECTED-TEXT
              MOVE 0 TO ACTUAL
           ELSE
              MOVE 1 TO ACTUAL
              DISPLAY 'VALEUR ATTENDUE : ' WS-EXPECTED-TEXT
              DISPLAY 'VALEUR OBTENUE : ' WS-DATE-TEXT
           END-IF

           MOVE 0 TO EXPECTED

           CALL 'ASSEQ' USING TEST-CONTEXT,
                             TEST-NAME,
                             EXPECTED,
                             ACTUAL
           .

       * Reproduction du comportement de COBG53DT
       * (même façon de lire la date, de calculer le jour,
       *  de formater l’année, le mois et le jour du mois).
       BUILD-EXPECTED-DATE.
           * Récupèration de la date et du jour de la semaine
           ACCEPT WS-DATE-6 FROM DATE
           ACCEPT WS-DOW    FROM DAY-OF-WEEK

           MOVE WS-DATE-6(1:2) TO WS-YY
           MOVE WS-DATE-6(3:2) TO WS-MM
           MOVE WS-DATE-6(5:2) TO WS-DD

           * Année sur 4 chiffres : même simplification
           * que dans COBG53DT (2000 + YY)
           COMPUTE WS-YYYY = 2000 + WS-YY
           MOVE WS-YYYY TO WS-YYYY-TXT

           * Jour de la semaine (même EVALUATE que COBG53DT)
           PERFORM BUILD-DAY-NAME

           * Mois (même EVALUATE que COBG53DT)
           PERFORM BUILD-MONTH-NAME

           * Jour du mois en texte, sans zéro de tête
           PERFORM BUILD-DAY-OF-MONTH-TEXT

           * Construction finale de la chaîne attendue
           MOVE SPACES TO WS-EXPECTED-TEXT
           STRING
               WS-DAY-NAME          DELIMITED BY SPACE
               ", "                 DELIMITED BY SIZE
               WS-MONTH-NAME        DELIMITED BY SPACE
               " "                  DELIMITED BY SIZE
               WS-DD-TEXT-CHARS(1:WS-DD-LEN) DELIMITED BY SIZE
               ", "                 DELIMITED BY SIZE
               WS-YYYY-TXT          DELIMITED BY SIZE
           INTO WS-EXPECTED-TEXT
           END-STRING
           .

       * Jour de la semaine attendu
       BUILD-DAY-NAME.
           EVALUATE WS-DOW
              WHEN 1 MOVE "Monday   "  TO WS-DAY-NAME
              WHEN 2 MOVE "Tuesday  "  TO WS-DAY-NAME
              WHEN 3 MOVE "Wednesday"  TO WS-DAY-NAME
              WHEN 4 MOVE "Thursday "  TO WS-DAY-NAME
              WHEN 5 MOVE "Friday   "  TO WS-DAY-NAME
              WHEN 6 MOVE "Saturday "  TO WS-DAY-NAME
              WHEN 7 MOVE "Sunday   "  TO WS-DAY-NAME
              WHEN OTHER MOVE "UNKNOWN " TO WS-DAY-NAME
           END-EVALUATE
           .

       * Mois attendu
       BUILD-MONTH-NAME.
           EVALUATE WS-MM
              WHEN 1  MOVE "January  " TO WS-MONTH-NAME
              WHEN 2  MOVE "February " TO WS-MONTH-NAME
              WHEN 3  MOVE "March    " TO WS-MONTH-NAME
              WHEN 4  MOVE "April    " TO WS-MONTH-NAME
              WHEN 5  MOVE "May      " TO WS-MONTH-NAME
              WHEN 6  MOVE "June     " TO WS-MONTH-NAME
              WHEN 7  MOVE "July     " TO WS-MONTH-NAME
              WHEN 8  MOVE "August   " TO WS-MONTH-NAME
              WHEN 9  MOVE "September" TO WS-MONTH-NAME
              WHEN 10 MOVE "October  " TO WS-MONTH-NAME
              WHEN 11 MOVE "November " TO WS-MONTH-NAME
              WHEN 12 MOVE "December " TO WS-MONTH-NAME
              WHEN OTHER MOVE "UNKNOWN " TO WS-MONTH-NAME
           END-EVALUATE
           .

       * Jour du mois, sans zéro de tête (même logique que COBG53DT)
       BUILD-DAY-OF-MONTH-TEXT.
           MOVE SPACES        TO WS-DD-TEXT-CHARS
           MOVE 2             TO WS-DD-LEN
           MOVE WS-DD         TO WS-DD-TEXT-CHARS
           IF WS-DD-TEXT-CHARS(1:1) = '0'
              MOVE WS-DD-TEXT-CHARS(2:1)
                   TO WS-DD-TEXT-CHARS(1:1)
              MOVE 1 TO WS-DD-LEN
           END-IF
           .

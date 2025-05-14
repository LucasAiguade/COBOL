       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL00SRT.
       AUTHOR. LUCASAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PF01
              ASSIGN TO DATABASE-PF01
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EXTERNALLY-DESCRIBED-KEY.

           SELECT DSPF01
               ASSIGN TO WORKSTATION-DSPF01
               ORGANIZATION IS TRANSACTION.

       DATA DIVISION.
       FILE SECTION.
       FD PF01 LABEL RECORD IS STANDARD.
       01 REG-PF01.
          COPY DDS-ALL-FORMAT OF PF01.

       FD DSPF01 LABEL RECORD IS OMITTED.
       01 REG-DSPF01.
           COPY DDS-ALL-FORMAT OF DSPF01.

       WORKING-STORAGE SECTION.
       01 AREA-INDICADORES.
          COPY DDS-ALL-FORMAT-INDIC OF DSPF01.

       01 WP1.
           05 WP1COD PIC 9(4).
           05 WP1NOM PIC X(30).
           05 WP1PRE PIC 9(5)V99.
           05 WP1PLAT PIC 9(3).
           05 WP1ERR PIC X(40).

       PROCEDURE DIVISION.

       MAIN.
           OPEN I-O DSPF01
           OPEN I-O PF01
           INITIALIZE WP1
           PERFORM AFEGIR-DESTINACIO.

       AFEGIR-DESTINACIO.
           PERFORM MUESTRO-P1 THRU FIN-MUESTRO-P1

           IF IN03 OF P1-I-INDIC = B"1"
              GO FI
           END-IF.

           IF WP1COD = ZEROS
               MOVE 'El camp codi no pot estar buid' TO WP1ERR
               GO AFEGIR-DESTINACIO
           ELSE IF WP1NOM = SPACES
                MOVE 'El camp nom no pot estar buid' TO WP1ERR
                GO AFEGIR-DESTINACIO
           ELSE IF WP1PRE = ZEROS
                MOVE 'El camp preu no pot estar buid' TO WP1ERR
                GO AFEGIR-DESTINACIO
           ELSE IF WP1PLAT = ZEROS
                MOVE 'El camp places totals no pot estar buid' TO WP1ERR
                GO AFEGIR-DESTINACIO
           ELSE
                MOVE WP1COD TO PF1COD
                READ PF01 INVALID KEY
                     PERFORM DE-PANTALLA-A-ARXIU THRU
                         FIN-DE-PANTALLA-A-ARXIU
                NOT INVALID KEY
                     PERFORM EXISTEIX THRU FIN-EXISTEIX
                END-READ
           END-IF.
           GO AFEGIR-DESTINACIO.

       MUESTRO-P1.
           WRITE REG-DSPF01 FROM WP1 FORMAT "P1".

           READ  DSPF01     INTO WP1 FORMAT "P1"
                            INDICATORS ARE P1-I-INDIC.
       FIN-MUESTRO-P1. EXIT.

       DE-PANTALLA-A-ARXIU.
           MOVE WP1COD TO PF1COD.
           MOVE WP1NOM TO PF1NOM.
           MOVE WP1PRE TO PF1PRE.
           MOVE WP1PLAT TO PF1PLAT.
           MOVE WP1PLAT TO PF1PLAD.
           WRITE REG-PF01.
           MOVE 'Sha fet correctament el registre' to WP1ERR.
           MOVE ZEROS TO WP1COD.
           MOVE SPACES TO WP1NOM.
           MOVE SPACES TO WP1PRE.
           MOVE SPACES TO WP1PLAT.
       FIN-DE-PANTALLA-A-ARXIU. EXIT.

       EXISTEIX.
           MOVE 'Ja existeix un registre amb aquest codi' TO WP1ERR.
           MOVE ZEROS TO WP1COD.
           PERFORM AFEGIR-DESTINACIO.
       FIN-EXISTEIX. EXIT.

       FI.
           CLOSE DSPF01.
           CLOSE PF01.
           GOBACK.

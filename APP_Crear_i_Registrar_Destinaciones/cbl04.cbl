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
       01 WP4.
           05 WP4COD PIC 9(4).
           05 WP4NOM PIC X(30).
           05 WP4PRE PIC 9(5)V99.
           05 WP4PLAT PIC 9(3).
           05 WP4PLAD PIC 9(3).
           05 WP4ERR PIC X(40).


       PROCEDURE DIVISION.

       MAIN.
           OPEN I-O DSPF01
           OPEN I-O PF01
           INITIALIZE WP4
           PERFORM MODIFICAR-RESERVA.

       MODIFICAR-RESERVA.
           MOVE ZEROS TO WP4COD,
           MOVE B"1" TO IN26.
           MOVE B"0" TO IN27.
           PERFORM MUESTRO-P4 THRU FIN-MUESTRO-P4

           IF IN03 OF P4-I-INDIC = B"1"
              PERFORM FI
           END-IF.

           MOVE WP4COD TO REG-PF01 PF1COD
           READ PF01 INVALID KEY
                     PERFORM NO-EXISTEIX THRU FIN-NO-EXISTEIX

                NOT INVALID KEY
                     PERFORM EXISTEIX THRU FIN-EXISTEIX

           GO TO MODIFICAR-RESERVA.

        MUESTRO-P4.
           WRITE REG-DSPF01 FROM WP4 FORMAT "P4"
                            INDICATORS ARE P4-O-INDIC.
           READ  DSPF01     INTO WP4 FORMAT "P4"
                            INDICATORS ARE P4-I-INDIC.
       FIN-MUESTRO-P4. EXIT.

       EXISTEIX.
            MOVE B"0" TO IN26.
            MOVE B"1" TO IN27.
            MOVE 'Mostrant Registre' to WP4ERR.
            PERFORM ARXIU-A-PANTALLA THRU
                FIN-ARXIU-A-PANTALLA.
            PERFORM EXISTEIX-010.

       EXISTEIX-010.
            PERFORM MUESTRO-P4 THRU FIN-MUESTRO-P4

            IF IN03 OF P4-I-INDIC = B"1"
               PERFORM FI
            ELSE IF IN07 OF P4-I-INDIC = B"1"
               MOVE 'Sha cancel·lat la modificació' to WP4ERR
               PERFORM MODIFICAR-RESERVA
            END-IF.
            MOVE 'Sha actualitzat correctament el registre' to WP4ERR.
            PERFORM PANTALLA-A-ARXIU THRU
                   FIN-PANTALLA-A-ARXIU.
            PERFORM MODIFICAR-RESERVA.
       FIN-EXISTEIX. EXIT.

       ARXIU-A-PANTALLA.
            MOVE PF1NOM to WP4NOM.
            MOVE PF1PRE to WP4PRE.
            MOVE PF1PLAT to WP4PLAT.
            MOVE PF1PLAD to WP4PLAD.
       FIN-ARXIU-A-PANTALLA. EXIT.

       PANTALLA-A-ARXIU.
            IF WP4PLAT < (PF1PLAT - PF1PLAD)
               MOVE 'Les places son insuficients, posan més' to WP4ERR
               PERFORM EXISTEIX-010
            END-IF.
            COMPUTE PF1PLAD = WP4PLAT - (PF1PLAT - PF1PLAD)
            MOVE WP4NOM to PF1NOM .
            MOVE WP4PRE to PF1PRE.
            MOVE WP4PLAT to PF1PLAT.
            REWRITE REG-PF01.
       FIN-PANTALLA-A-ARXIU. EXIT.

       NO-EXISTEIX.
            IF WP4COD = ZEROS
               MOVE 'Introdueix un codi si us plau' to WP4ERR
               PERFORM MODIFICAR-RESERVA
            END-IF.
            MOVE 'No existeix cap registre amb aquest codi' to WP4ERR.
            MOVE ZEROS TO WP4COD.
            PERFORM MODIFICAR-RESERVA.
       FIN-NO-EXISTEIX. EXIT.

       FI.
           CLOSE DSPF01.
           CLOSE PF01.
           GOBACK.






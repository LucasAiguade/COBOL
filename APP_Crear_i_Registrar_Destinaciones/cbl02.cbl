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

       01 AREA-INDICADORES.
          COPY DDS-ALL-FORMAT-INDIC OF DSPF01.

       01 WP2.
           05 WP2COD PIC 9(4).
           05 WP2ERR PIC X(40).

       PROCEDURE DIVISION.

       MAIN.
           OPEN I-O DSPF01
           OPEN I-O PF01

           INITIALIZE WP2
           PERFORM REGISTRAR-RESERVA.

       REGISTRAR-RESERVA.
           MOVE SPACES TO WP2COD

           PERFORM MUESTRO-P2 THRU FIN-MUESTRO-P2

           IF IN03 OF P2-I-INDIC = B"1"
              GO FI
           END-IF.

           MOVE WP2COD TO PF1COD

           READ PF01 INVALID KEY
                     PERFORM NO-EXISTEIX THRU FIN-NO-EXISTEIX

                NOT INVALID KEY
                     PERFORM EXISTEIX THRU FIN-EXISTEIX

           GO TO REGISTRAR-RESERVA.

        MUESTRO-P2.
           WRITE REG-DSPF01 FROM WP2 FORMAT "P2".

           READ  DSPF01     INTO WP2 FORMAT "P2"
                            INDICATORS ARE P2-I-INDIC.
       FIN-MUESTRO-P2. EXIT.

       NO-EXISTEIX.
           MOVE 'NO EXISTEIX CAP REGISTRE AMB AQUEST CODI' TO WP2ERR.
           PERFORM REGISTRAR-RESERVA
       FIN-NO-EXISTEIX. EXIT.

       EXISTEIX.
           IF PF1PLAD > 0
               SUBTRACT 1 FROM PF1PLAD
               REWRITE REG-PF01
               MOVE 'Sha fet la reserva perfectament' TO WP2ERR
           ELSE IF PF1PLAD = 0
               MOVE 'No queden places disponibles' TO WP2ERR
           END-IF.

           PERFORM REGISTRAR-RESERVA
       FIN-EXISTEIX. EXIT.

       FI.
           CLOSE DSPF01.
           CLOSE PF01.
           GOBACK.






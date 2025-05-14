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

       01 WP3.
           05 WP3COD PIC 9(4).
           05 WP3ERR PIC X(40).
           05 WP3PLAD PIC 9(3).

       PROCEDURE DIVISION.

       MAIN.
           OPEN I-O DSPF01
           OPEN I-O PF01
           INITIALIZE WP3
           PERFORM CANCELAR-RESERVA.

       CANCELAR-RESERVA.
           MOVE SPACES TO WP3COD
           PERFORM MUESTRO-P3 THRU FIN-MUESTRO-P3

           IF IN03 OF P3-I-INDIC = B"1"
              GO FI
           END-IF.

           MOVE WP3COD TO REG-PF01 PF1COD
           READ PF01 INVALID KEY
                     PERFORM NO-EXISTEIX THRU FIN-NO-EXISTEIX
                NOT INVALID KEY
                     PERFORM EXISTEIX THRU FIN-EXISTEIX

           GO CANCELAR-RESERVA.

        MUESTRO-P3.
           WRITE REG-DSPF01 FROM WP3 FORMAT "P3".

      *Enlazamos los indicadores al record que abrimos
           READ  DSPF01     INTO WP3 FORMAT "P3"
                            INDICATORS ARE P3-I-INDIC.
       FIN-MUESTRO-P3. EXIT.

       NO-EXISTEIX.
           MOVE 'NO EXISTEIX CAP REGISTRE AMB AQUEST CODI' TO WP3ERR.
           PERFORM CANCELAR-RESERVA
       FIN-NO-EXISTEIX. EXIT.

       EXISTEIX.
           IF PF1PLAD = PF1PLAT
               MOVE 'No nhi han reserves per aquest codi' TO WP3ERR
           ELSE IF PF1PLAD < PF1PLAT
               ADD 1 TO PF1PLAD
               REWRITE REG-PF01
               MOVE 'Sha fet la cancel·lació perfectament' TO WP3ERR
           END-IF.

           PERFORM CANCELAR-RESERVA
       FIN-EXISTEIX. EXIT.

       FI.
           CLOSE DSPF01.
           CLOSE PF01.
           GOBACK.

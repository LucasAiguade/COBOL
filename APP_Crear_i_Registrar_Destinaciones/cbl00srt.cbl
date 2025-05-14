       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBL00SRT.
       AUTHOR. LUCASAM.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DSPF01
               ASSIGN TO WORKSTATION-DSPF01
               ORGANIZATION IS TRANSACTION.

       DATA DIVISION.
       FILE SECTION.
       FD DSPF01 LABEL RECORD IS OMITTED.
       01 REG-DSPF01.
           COPY DDS-ALL-FORMAT OF DSPF01.

       WORKING-STORAGE SECTION.
       01 AREA-INDICADORES.
          COPY DDS-ALL-FORMAT-INDIC OF DSPF01.

       01 WPM.
           05 WPMOPC  PIC 9.
           05 WPMERR  PIC X(40).

       PROCEDURE DIVISION.

       MAIN.
           OPEN I-O DSPF01
           INITIALIZE WPM
           PERFORM MENU.
       MENU.
           MOVE SPACES TO WPMOPC
           PERFORM MUESTRO-MENU THRU FIN-MUESTRO-MENU

           IF IN03 OF PM-I-INDIC = B"1"
              GO FI
           END-IF.

           EVALUATE WPMOPC
               WHEN 1
                   CALL 'CBL01'
               WHEN 2
                   CALL 'CBL02'
               WHEN 3
                   CALL 'CBL03'
               WHEN 4
                   CALL 'CBL04'
               WHEN 5
                   CALL 'CBL05'
               WHEN 6
                   GO FI
               WHEN OTHER
                   MOVE 'OPCIO INVÀLIDA' TO WPMERR
           END-EVALUATE.

           GO MENU.

       MUESTRO-MENU.
           WRITE REG-DSPF01 FROM WPM FORMAT "PM".

           READ  DSPF01     INTO WPM FORMAT "PM"
                            INDICATORS ARE PM-I-INDIC.

       FIN-MUESTRO-MENU. EXIT.

       FI.
           CLOSE DSPF01
           STOP RUN.

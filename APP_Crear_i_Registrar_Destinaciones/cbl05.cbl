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

       01 WP5.
           05 WP5COD PIC 9(4).
           05 WP5NOM PIC X(30).
           05 WP5PRE PIC 9(5)V99.
           05 WP5PLAT PIC 9(3).
           05 WP5PLAD PIC 9(3).
           05 WP5ERR PIC X(40).


        PROCEDURE DIVISION.

        MAIN.
            OPEN I-O DSPF01
            OPEN I-O PF01
            INITIALIZE WP5.
            PERFORM MOSTRAR-REGISTRO.

        MOSTRAR-REGISTRO.
            PERFORM MUESTRO-P5 THRU FIN-MUESTRO-P5.

            IF IN03 OF P5-I-INDIC = B"1"
               PERFORM FI
            END-IF.

           MOVE WP5COD TO REG-PF01 PF1COD
           READ PF01 INVALID KEY
                     PERFORM NO-EXISTEIX THRU FIN-NO-EXISTEIX

                NOT INVALID KEY
                     PERFORM EXISTEIX THRU FIN-EXISTEIX
            GO TO MOSTRAR-REGISTRO.

        NO-EXISTEIX.
            IF WP5COD = ZEROS
               MOVE 'Introdueix un codi si us plau' to WP5ERR
               PERFORM MOSTRAR-REGISTRO
            END-IF.
            MOVE ZEROS to WP5PRE WP5PLAT WP5PLAD.
            MOVE SPACES to WP5NOM.
            MOVE 'No existeix cap registre amb aquest codi' to WP5ERR.
            PERFORM MOSTRAR-REGISTRO.
        FIN-NO-EXISTEIX. EXIT.

        EXISTEIX.
             MOVE 'Mostrant Registre' to WP5ERR.
             PERFORM ARXIU-A-PANTALLA THRU
                 FIN-ARXIU-A-PANTALLA.

        PERFORM MOSTRAR-REGISTRO.
        FIN-EXISTEIX. EXIT.

        ARXIU-A-PANTALLA.
            MOVE PF1NOM to WP5NOM.
            MOVE PF1PRE to WP5PRE.
            MOVE PF1PLAT to WP5PLAT.
            MOVE PF1PLAD to WP5PLAD.
        FIN-ARXIU-A-PANTALLA. EXIT.

        MUESTRO-P5.
            WRITE REG-DSPF01 FROM WP5 FORMAT "P5"

            READ  DSPF01     INTO WP5 FORMAT "P5"
                             INDICATORS ARE P5-I-INDIC.
        FIN-MUESTRO-P5. EXIT.

        FI.
           CLOSE DSPF01.
           GOBACK.


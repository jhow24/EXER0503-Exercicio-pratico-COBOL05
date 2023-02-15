      *================================================================*
       IDENTIFICATION                  DIVISION.
      *================================================================*
       PROGRAM-ID. PSDC2EX1.
       AUTHOR.     JOHNATHAN.
      *================================================================*
      *              C A P G E M I N I - S I S T E M A S               *
      *================================================================*
      *    PROGRAMA....: PSDC2EX1
      *    PROGRAMADOR.: JOHNATHAN
      *    ANALISTA....: ARI BORGES                                *
      *    DATA........: 26/01/2023                                    *
      *----------------------------------------------------------------*
      *    OBJETIVO....:   CRIAR CURSOR
      *                    FAZER LEITURA DE TABELA DB2
      *----------------------------------------------------------------*
      *    ARQUIVOS:                                                   *
      *       DDNAME                                 INCLUDE/BOOK      *
      *      ARQSAI01                                  SAI04103
      *      INFO_PSSOA                                CADUB069
      *----------------------------------------------------------------*
      *    ROTINAS.....:                                               *
      *                                                                *
      *================================================================*

      *================================================================*
      *                                                                *
      *================================================================*
       ENVIRONMENT                     DIVISION.
      *================================================================*
      *                                                                *
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
      *
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *
      *----------------------------------------------------------------
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
      *
       FILE-CONTROL.
      *
           SELECT ARQSAI01 ASSIGN       TO UT-S-ARQSAI01
                      FILE STATUS      IS WRK-FS-ARQSAI01.
      *
      *================================================================*
       DATA                            DIVISION.
      *================================================================
      *                                                                *
      *----------------------------------------------------------------
       FILE                            SECTION.
      *----------------------------------------------------------------*

      *----------------------------------------------------------------*
      *   OUTPUT:     ARQUIVO DE SAIDA                                *
      *               ORG. SEQUENCIAL   -   LRECL = 153               *
      *---------------------------------------------------------------*

       FD  ARQSAI01
           RECORDING MODE IS F
           LABEL RECORD IS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01 FD-ARQSAI01             PIC X(153).

      *
      *
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
       77 FILLER                  PIC  X(050) VALUE
             'PSDC2EX1 - INICIO DA AREA DE WORKING'.
      *----------------------------------------------------------------*
      *
       77 WRK-PROGRAMA            PIC  X(008) VALUE 'PSDC2EX1'.
       77 WRK-MASK                PIC ZZZ.ZZZ.ZZ9 VALUE ZEROS.
       77 WRK-TABELA              PIC  X(010) VALUE SPACES.
       77 WRK-SQLCODE             PIC  -99999.
       77 WRK-FIM-CSR1            PIC  X(001) VALUE SPACES.
      *
       01 WRK-ACUMULADORES.
           03 ACU-LIDOS           PIC  9(005) COMP-3 VALUE ZEROS.
           03 ACU-GRAVA-ARQSAI01  PIC  9(005) VALUE ZEROS.
      *
       01 WRK-CABEC.
           05 WRL-CABEC-ARQSAI01  PIC  X(015) VALUE
              'COD;NOME;EMAIL'.
      *
       77 WRK-ARQUIVO             PIC  X(008) VALUE SPACES.
          88 WRK-CN-ARQSAI01      VALUE 'SAI04103'.

       77 WRK-COMANDO             PIC  X(005) VALUE SPACES.
          88 WRK-CN-OPEN          VALUE 'OPEN '.
          88 WRK-CN-CLOSE         VALUE 'CLOSE'.
          88 WRK-CN-READ          VALUE 'READ '.
          88 WRK-CN-WRITE         VALUE 'WRITE'. 

                  
           
      *----------------------------------------------------------------
       01 FILLER                  PIC  X(050) VALUE
             'AREA PARA TRATAMENTO DE FILE-STATUS'.
      *----------------------------------------------------------------*
      *
       01 WRK-AREA-FS.      
          05 WRK-FS-ARQSAI01         PIC  X(002) VALUE SPACES.
             88 WRK-FS-SAI01-OK                  VALUE '00'.

          05 WRK-FS-DISPLAY          PIC  X(002) VALUE SPACES.
      *
      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
             'AREA DOS BOOKS DOS ARQUIVOS DE ENTRADA E SAIDA'.
      *----------------------------------------------------------------*
      *
      **** AREA ARQUIVO DE SAIDA

          01 ARQSAI01-REGISTRO.
             03 ARQSAI01-COD-CLI      PIC 9.999.999.999.
             03 ARQSAI01-NOME         PIC X(70).
             03 ARQSAI01-EMAIL        PIC X(70).

      *----------------------------------------------------------------*
       01 FILLER                  PIC  X(050) VALUE
              'AREA PARA DB2'.
      *----------------------------------------------------------------*
           EXEC SQL 
              INCLUDE SQLCA
           END-EXEC.

      *    DB2PRD.INFO_PSSOA                                           *
           EXEC SQL
              INCLUDE CADUB069
           END-EXEC.

           EXEC SQL DECLARE CSR-B069 CURSOR WITH  HOLD FOR
               SELECT CCLUB, IPSSOA_COPLT, EEMAIL_PSSOA
               FROM DB2PRD.INFO_PSSOA
               WHERE CSGL_UF          = 'BA'
               AND CID_TPO_PSSOA      = 'F'
               AND CSEXO              = 'F'
               AND CPTCAO_ESPAC_TBELA = 1
               AND EEMAIL_PSSOA IS NOT NULL 
             ORDER BY CCLUB
           END-EXEC.

      *----------------------------------------------------------------*
       01 FILLER                PIC X(050) VALUE
              'ENT0403 - FIM DA AREA DE WORKING'.
      *================================================================*
       PROCEDURE                       DIVISION.
      *================================================================*
      *
      *----------------------------------------------------------------*
      *    ROTINA PRINCIPAL DO PROGRAMA                                *
      *----------------------------------------------------------------*
       0000-PRINCIPAL SECTION.
      *----------------------------------------------------------------
      *    

           PERFORM 1000-INICIALIZAR
      *
           PERFORM 3000-PROCESSAR UNTIL WRK-FIM-CSR1 EQUAL 'S'
      *
           PERFORM 9900-FINALIZAR
           .
      *
      *----------------------------------------------------------------*
       0000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------
      *    ROTINA DE INICIALIZACAO DO PROGRAMA
      *----------------------------------------------------------------*
       1000-INICIALIZAR SECTION.
      *----------------------------------------------------------------*
      *    
           CALL 'CKRS1000'
           CALL 'CKRS1050'
           
           EXEC SQL
              OPEN CSR-B069
           END-EXEC.

           IF SQLCODE EQUAL +0
              PERFORM 3900-GRAVAR-ARQSAI01
           ELSE 
              PERFORM 9100-ERROS-ARQUIVOS   
           END-IF

           OPEN OUTPUT ARQSAI01

           IF NOT WRK-FS-SAI01-OK
              MOVE WRK-FS-ARQSAI01           TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           PERFORM 3800-LER-CURSOR

           IF SQLCODE EQUAL +100 
              DISPLAY '************************************************'
              DISPLAY '*       ARQUIVO DE ENTRADA VAZIO               *'
              DISPLAY '* ' WRK-PROGRAMA '  CANCELADO                  *'
              DISPLAY '************************************************'
              PERFORM 9900-FINALIZAR
           END-IF 

      * 
           SET WRK-CN-ARQSAI01                TO TRUE
           SET WRK-CN-OPEN                    TO TRUE
           
           SET WRK-CN-WRITE                   TO TRUE

           WRITE FD-ARQSAI01 FROM WRK-CABEC.

           IF NOT WRK-FS-SAI01-OK 
              MOVE WRK-FS-SAI01-OK           TO WRK-FS-DISPLAY
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF      

           .
      *----------------------------------------------------------------*
       1000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *        ROTINA DE PROCESSAMENTO PRINCIPAL                       *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
       3000-PROCESSAR SECTION.
      *----------------------------------------------------------------*

           MOVE CCLUB               TO ARQSAI01-COD-CLI 
           MOVE IPSSOA-COPLT        TO ARQSAI01-NOME
           MOVE EEMAIL-PSSOA        TO ARQSAI01-EMAIL

           PERFORM 3900-GRAVAR-ARQSAI01 

           PERFORM 3800-LER-CURSOR 

           .
      *----------------------------------------------------------------*
       3000-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
      *

      *----------------------------------------------------------------*
      *    CURSOR
      *----------------------------------------------------------------*
       3800-LER-CURSOR SECTION.
      *----------------------------------------------------------------*
           EXEC SQL
              FETCH CSR-B069 INTO 
                      :CADUB069.CCLUB
                     ,:CADUB069.IPSSOA-COPLT
                     ,:CADUB069.EEMAIL-PSSOA
           END-EXEC
      *
           EVALUATE TRUE
               WHEN SQLCODE EQUAL +100
                    MOVE 'S'           TO WRK-FIM-CSR1
               WHEN SQLCODE EQUAL ZEROS
                    ADD 1              TO ACU-LIDOS
               WHEN OTHER
                    PERFORM 9100-ERROS-ARQUIVOS
           END-EVALUATE
      *
           .
      *
      *----------------------------------------------------------------*
       3800-99-FIM.                     
           EXIT.
      *----------------------------------------------------------------*
      *----------------------------------------------------------------
       3900-GRAVAR-ARQSAI01 SECTION.
      *----------------------------------------------------------------*
           

           SET WRK-CN-WRITE           TO TRUE

           WRITE FD-ARQSAI01 FROM ARQSAI01-REGISTRO

           IF NOT WRK-FS-SAI01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           ADD 1                      TO ACU-GRAVA-ARQSAI01

           INITIALIZE                  ARQSAI01-REGISTRO 
           .
           
      *----------------------------------------------------------------*
       3900-99-FIM.                    
           EXIT.
      *----------------------------------------------------------------*
       9100-ERROS-ARQUIVOS SECTION .
      *----------------------------------------------------------------*
           DISPLAY '*****************************'
           DISPLAY '*     ERRO COM ARQUIVOS     *'
           DISPLAY '* COMANDO: ' WRK-COMANDO
                                   '            *'
           DISPLAY '* ARQUIVO: ' WRK-ARQUIVO
                                      '         *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' CANCELADO        *'
           DISPLAY '*****************************'

           PERFORM 9900-FINALIZAR.
      *----------------------------------------------------------------*
       9100-99-FIM.
           EXIT.
      *----------------------------------------------------------------*
       9900-FINALIZAR SECTION.
      *----------------------------------------------------------------*
           SET WRK-CN-CLOSE TO TRUE.

           EXEC SQL
              CLOSE CSR-B069
           END-EXEC.
           IF SQLCODE NOT EQUAL 0
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           CLOSE ARQSAI01.
           IF NOT WRK-FS-SAI01-OK
              PERFORM 9100-ERROS-ARQUIVOS
           END-IF

           DISPLAY '***************************************************'
           MOVE ACU-GRAVA-ARQSAI01     TO WRK-MASK
           DISPLAY '* QTDE REGISTROS GRAVADOS: ' WRK-MASK
                                                    '                 *'
           DISPLAY '*                                                 *'
           DISPLAY '* ' WRK-PROGRAMA
                             ' FIM NORMAL                             *'
           DISPLAY '***************************************************'

             STOP RUN.
           
          END PROGRAM PSDC2EX1.
      *----------------------------------------------------------------*
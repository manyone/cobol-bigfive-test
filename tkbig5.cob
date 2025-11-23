       IDENTIFICATION DIVISION.
       PROGRAM-ID. BIGFIVE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. PC    WITH DEBUGGING MODE.
       SOURCE-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT BIGFIVE-FILE
               ASSIGN TO UT-S-STMTS
               ACCESS MODE IS SEQUENTIAL.

           SELECT TRAITS-FILE
               ASSIGN TO UT-S-TRAITS
               ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       FD  BIGFIVE-FILE
              RECORD CONTAINS 80 CHARACTERS
              BLOCK CONTAINS 0 RECORDS
              RECORDING MODE IS F.

       01  BIGFIVE-RECORD.
           03 FILLER PIC X(80).

       FD   TRAITS-FILE
              RECORD CONTAINS 150 CHARACTERS
              BLOCK CONTAINS 0 RECORDS
              RECORDING MODE IS F.

       01  TRAITS-RECORD.
           03 FILLER PIC X(150).

       WORKING-STORAGE SECTION.
       01 DEBUG-MODE-SW PIC 9 VALUE 0.
           88 DEBUG-MODE VALUE 1.
       01 EOF-FLAGS.
           03 NO-MORE-BIGFIVE-SW PIC X VALUE SPACE.
               88 NO-MORE-BIGFIVE VALUE 'Y'.
           03 NO-MORE-TRAITS-SW PIC X VALUE SPACE.
               88 NO-MORE-TRAITS VALUE 'Y'.
       01  WS-FS-BIGFIVE   PIC X(02).
       01  WS-FS-TRAITS   PIC X(02).
       01  MSG1.
       03  FILLER PIC X(33) VALUE 'BIG FIVE PERSONALITY TEST'.
       01  MSG2.
       03  FILLER PIC X(33) VALUE 'Please respond to all of the stat'.
       03  FILLER PIC X(33) VALUE 'ements and answer in sequence.  '.
       01  MSG3.
       03  FILLER PIC X(33) VALUE 'For each statement choose the res'.
       03  FILLER PIC X(33) VALUE 'ponse that best represents your o'.
       03  FILLER PIC X(33) VALUE 'pinion:'.
       01  BFX PIC 99 VALUE 0.
        01  BF-TABLE-AREA.
            03  BF-COUNT    PIC 99 VALUE 0.
            03  BF-TABLE-DEF.
                05  BF-REC OCCURS 50 TIMES.
                  07  BF-TRAIT-SCX   PIC 9.
                  07  BF-NEG-FLAG    PIC X.
                  07    FILLER      PIC X.
                  07  BF-QUESTION   PIC X(60).
                  07  FILLER        PIC X(17).
       01  ANS-TABLE-AREA.
           03  ANS-VALUE   PIC 9 OCCURS 50 TIMES.
       01  PRX PIC 9.
       01  SCX PIC 9.
       01  SEQ-ID  PIC 99.
       01  SUM-TRAIT-TABLE-AREA.
         03 FILLER PIC X(20) VALUE 'EXTRAVERSION'.
           03  SUM-EXTRAVERSION       PIC 999.
               03  PCT-EXTRAVERSION       PIC 999V9.
          03 FILLER PIC X(20) VALUE 'AGGREABLENESS'.
           03  SUM-AGREEABLENESS      PIC 999.
               03  PCT-AGREEABLENESS      PIC 999V9.
          03 FILLER PIC X(20) VALUE 'CONSCIENTIOUSNESS'.
           03  SUM-CONSCIENTIOUS        PIC 999.
               03  PCT-CONSCIENTIOUS        PIC 999V9.
          03 FILLER PIC X(20) VALUE 'EMOTIONAL STABILITY'.
           03  SUM-EMOTIONAL            PIC 999.
               03  PCT-EMOTIONAL            PIC 999V9.
          03 FILLER PIC X(20) VALUE 'INTELLECT'.
           03  SUM-INTELLECT           PIC 999.
               03  PCT-INTELLECT           PIC 999V9.
       01  FILLER REDEFINES SUM-TRAIT-TABLE-AREA.
           03  FILLER OCCURS 5 TIMES.
               05 LBL-TRAIT                PIC X(20).
               05 SUM-TRAIT                 PIC 999.
               05 PCT-TRAIT                 PIC 999V9.
       01   DSP-TRAIT                 PIC ZZ9.9.
   	   01  TR-RECORD.
           03  TR-REC-ID   PIC 9.
           03  TR-REC-LVL PIC 9.
           03  TR-REC-DESC      PIC X(128).
           03  FILLER  PIC X(20).
       01  LVX PIC 9.
       01  TR-TRAITS-TABLE-AREA.
           03  TR-COUNT    PIC 9.
           03  TR-TRAIT OCCURS 5 TIMES.
               05  TR-TRAIT-DESC PIC X(128).
               05 TR-TRAIT-LEVEL-DESC  PIC X(128) OCCURS 3 TIMES.
        01 SEQNO       PIC Z9..
        01 CHOICES PIC X(72) VALUE '1=Very Inaccurate  2=Inaccurate  3=N
      -    'eutral  4=Accurate  5=Very Accurate'.
       01  ANS-X PIC X.
       01  ANS REDEFINES ANS-X PIC 9.

       01  INPSTR-TEXT PIC X(1024) VALUE 'YOUR-GENERATED-INPSTR-HERE'.
       01  FILLER REDEFINES INPSTR-TEXT.
           03  INPS-CH PIC X OCCURS 128.
       01  INPSTR-LENGTH PIC 9(4) VALUE 128.
       01  INDEX-POS PIC 9(4) VALUE 1.
       01  LINE-BUFFER PIC X(80).
       01  FILLLER REDEFINES LINE-BUFFER.
           03  LBUF-CH PIC X OCCURS 80.
       01  REMAINING-LEN PIC 9(4).
       01  COPY-LEN PIC 9(4).
       01  SPACE-POS PIC 9(4).
       01  DISP-LEN PIC 99 VALUE 80.
       01  CCX      PIC 9999.
       01  LCX      PIC 9999.
       01  WS-TIME     PIC X(06).
       01  FILLER REDEFINES WS-TIME.
           03  WS-HH   PIC 99.
           03  WS-MM   PIC 99.
           03  WS-SS   PIC 99.
       01  WS-TOT-SECS  PIC S9(7) COMP-3.
       01  WS-SEED  PIC S9(5) COMP-3.
       01  WS-CURR-VALUE   PIC S9(13) COMP-3.
       01  TEMP1               PIC S9(13) COMP-3.
       01  TEMP2              PIC S9(13) COMP-3.
       01  TEMP3             PIC S9(13) COMP-3.
       01  RESULT             PIC S9(13) COMP-3.
       01  NEXT-STATE          PIC S9(13) COMP-3.
       01  CURR-VAL            PIC S9(13) COMP-3.
       01  RAND-AREA.
        05  MULT-FACTOR        PIC 9(5)   VALUE 7.
   	    05  INCRVAL            PIC 9(5)   VALUE 3.
   	    05  MODULUS-VAL      PIC 9(6)   VALUE 11.
   	    05  MOD5               PIC 9(1)   VALUE 5.
       01  REDISP-X PIC X.
       01  FILLER REDEFINES REDISP-X.
         03  REDISP PIC 9,

       PROCEDURE DIVISION.
      *    MOVE 1 TO  DEBUG-MODE-SW.
           PERFORM LOAD-BIGFIVE.
           PERFORM LOAD-TRAITS.
      *   IF DEBUG-MODE
      *    PERFORM RAND-ANSWERS
      *    ELSE
           PERFORM ACCEPT-ANSWERS.
           PERFORM COMPUTE-SCORES.
           PERFORM SHOW-SCORES.
           DISPLAY 'RE-DISPLAY?(1=YES)'.
           ACCEPT REDISP-X.
           PERFORM RE-DISPLAY
           UNTIL NOT (REDISP-X NUMERIC AND REDISP= 1).
           GOBACK.
       RE-DISPLAY.
           PERFORM SHOW-SCORES.
           DISPLAY 'RE-DISPLAY?(1=YES)'.
           ACCEPT REDISP-X.
       LOAD-BIGFIVE.
            OPEN INPUT BIGFIVE-FILE.
            MOVE 0 TO BF-COUNT.
            PERFORM READ-BIGFIVE.
            PERFORM PROCESS-BIGFIVE UNTIL NO-MORE-BIGFIVE.
            CLOSE BIGFIVE-FILE.
       PROCESS-BIGFIVE.
           ADD 1 TO BF-COUNT.
           MOVE BIGFIVE-RECORD TO BF-REC (BF-COUNT).
           PERFORM READ-BIGFIVE.

       READ-BIGFIVE.
           READ BIGFIVE-FILE
           AT END MOVE 'Y' TO NO-MORE-BIGFIVE-SW.

       LOAD-TRAITS.
            OPEN INPUT TRAITS-FILE.
            MOVE 0 TO TR-COUNT.

            PERFORM READ-TRAITS.
            PERFORM PROCESS-TRAITS
               UNTIL NO-MORE-TRAITS.

            CLOSE TRAITS-FILE.
       PROCESS-TRAITS.
           ADD 1 TO TR-COUNT
      *    DISPLAY TR-REC-ID ' ' TR-REC-LVL ' ' TR-REC-DESC.
           IF TR-REC-LVL = 0
               MOVE TR-REC-DESC    TO TR-TRAIT-DESC (TR-REC-ID)
           ELSE
                MOVE TR-REC-DESC
               TO TR-TRAIT-LEVEL-DESC (TR-REC-ID, TR-REC-LVL).

           PERFORM READ-TRAITS.
       READ-TRAITS.
           READ TRAITS-FILE INTO TR-RECORD
           AT END MOVE 'Y' TO NO-MORE-TRAITS-SW.

       RAND-ANSWERS.
      *    ACCEPT WS-TIME FROM TIME.
           MOVE TIME-OF-DAY TO WS-TIME.
           DISPLAY WS-TIME.
      *    DISPLAY 'DEBUGGING MODE'.
           COMPUTE WS-TOT-SECS = WS-HH * 3600 + WS-MM * 60 + WS-SS.
           DIVIDE WS-TOT-SECS BY MODULUS-VAL
               GIVING TEMP1 REMAINDER WS-SEED.
           MOVE WS-SEED TO CURR-VAL.
      *    MOVE 0 TO CURR-VAL
           PERFORM GET-NEXT-RAND
                VARYING BFX FROM 1 BY +1
                UNTIL BFX >    BF-COUNT.


       GET-NEXT-RAND.
           COMPUTE NEXT-STATE =
           (CURR-VAL * MULT-FACTOR) + INCRVAL.
           DIVIDE NEXT-STATE BY MODULUS-VAL GIVING TEMP1 REMAINDER TEMP2.
           DIVIDE TEMP2 BY MOD5 GIVING TEMP3 REMAINDER RESULT.
           ADD 1 TO RESULT.

	       MOVE RESULT TO ANS.
	       MOVE TEMP2 TO CURR-VAL.
           MOVE ANS TO ANS-VALUE (BFX).

       ACCEPT-ANSWERS.
           DISPLAY MSG1 ' (' BF-COUNT ' items)'.
           DISPLAY MSG2.
           DISPLAY MSG3.

           PERFORM GET-STMT-ANSWER VARYING BFX FROM 1 BY +1
           UNTIL BFX > BF-COUNT.
       GET-STMT-ANSWER.
            PERFORM GET-ANSWER.
            PERFORM GET-ANSWER
                UNTIL ANS-X NUMERIC AND NOT (ANS<1 OR ANS>5).

           MOVE ANS TO ANS-VALUE(BFX).

       GET-ANSWER.
               MOVE BFX TO SEQNO .
               DISPLAY ' '.
               DISPLAY SEQNO  ' ' BF-QUESTION (BFX).
               DISPLAY CHOICES.
               DISPLAY 'Select the answer that best applies to you'.
               ACCEPT ANS-X.

       COMPUTE-SCORES.
           PERFORM CLEAR-SUM-TRAIT
             VARYING SCX FROM 1 BY +1 UNTIL SCX > 5.


           PERFORM SCORE-SUM-TRAIT VARYING BFX FROM 1 BY +1
             UNTIL BFX > BF-COUNT.

       CLEAR-SUM-TRAIT.
           MOVE ZEROES TO SUM-TRAIT (SCX).
       SCORE-SUM-TRAIT.
              MOVE BF-TRAIT-SCX (BFX) TO SCX.

               IF BF-NEG-FLAG (BFX) = ' '
                   MOVE ANS-VALUE (BFX) TO TEMP2
                   ADD TEMP2 TO SUM-TRAIT(SCX)
               ELSE
                   SUBTRACT ANS-VALUE(BFX) FROM 6 GIVING TEMP2
                   ADD TEMP2 TO SUM-TRAIT(SCX).

      *        DISPLAY 'BFC=' BFX ' SEQ=' SEQ-ID
      *        ' SCX=' SCX ' scor=' ANS-VALUE(BFX) ' ADJS=' TEMP2.
       SHOW-SCORES.
           PERFORM SHOW-TRAIT-SCORE
           VARYING SCX FROM 1 BY +1 UNTIL SCX>5.
       SHOW-TRAIT-SCORE.
            COMPUTE  PCT-TRAIT (SCX)
               = SUM-TRAIT (SCX) / BF-COUNT * 100.

               IF PCT-TRAIT(SCX) < 34.0
                   MOVE 1 TO LVX
               ELSE
                   IF PCT-TRAIT (SCX) > 66.0
                       MOVE 3 TO LVX
                   ELSE
                       MOVE 2 TO LVX.

               MOVE PCT-TRAIT (SCX) TO DSP-TRAIT .
      *       DISPLAY SCX ':' SUM-TRAIT (SCX) ' ' PCT-TRAIT (SCX)
      *       ' ' DSP-TRAIT  '%'
      *        ' LVL=' LVX.
               DISPLAY ' '.
               DISPLAY LBL-TRAIT(SCX) ' ' DSP-TRAIT  '%'
      *        DISPLAY TR-TRAIT-DESC (SCX).
      *        DISPLAY TR-TRAIT-LEVEL-DESC(SCX,LVX).
               MOVE TR-TRAIT-DESC (SCX) TO INPSTR-TEXT
               PERFORM WORD-WRAP-INPSTR.
               MOVE TR-TRAIT-LEVEL-DESC(SCX,LVX) TO INPSTR-TEXT.
               PERFORM WORD-WRAP-INPSTR.

       WORD-WRAP-INPSTR.
            MOVE 1 TO INDEX-POS.
           PERFORM WORD-WRAP UNTIL INDEX-POS > INPSTR-LENGTH.

       WORD-WRAP.
           COMPUTE REMAINING-LEN = INPSTR-LENGTH - INDEX-POS + 1.
           MOVE 1 TO LCX.
           MOVE INDEX-POS TO CCX.
           PERFORM INPSTR-TO-LINE UNTIL LCX > DISP-LEN,
           IF REMAINING-LEN > DISP-LEN
               MOVE  DISP-LEN TO COPY-LEN
               PERFORM FIND-SPACE-BACKWARDS
           ELSE
               MOVE REMAINING-LEN TO COPY-LEN.
           MOVE SPACES TO LINE-BUFFER.
           MOVE 1 TO LCX.
           MOVE INDEX-POS TO CCX.
           PERFORM INPSTR-TO-LINE UNTIL LCX > COPY-LEN.
           DISPLAY LINE-BUFFER.
           ADD COPY-LEN TO INDEX-POS.
       INPSTR-TO-LINE.
           MOVE INPS-CH (CCX) TO LBUF-CH (LCX).
           ADD 1 TO LCX.
           ADD 1 TO CCX.

       FIND-SPACE-BACKWARDS.
           MOVE  DISP-LEN TO SPACE-POS,
           PERFORM BACK-SPACE
           UNTIL SPACE-POS < 1 OR LBUF-CH(SPACE-POS)  = ' '.

           IF SPACE-POS > 1
               MOVE SPACE-POS TO COPY-LEN.

       BACK-SPACE.
           SUBTRACT 1 FROM SPACE-POS.
       SKIP-TO-NEXT-WORD.
           IF INDEX-POS < INPSTR-LENGTH AND
              INPS-CH(INDEX-POS) IS NOT EQUAL TO ' '
               ADD 1 TO INDEX-POS
           ELSE
              NEXT SENTENCE.

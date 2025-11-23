       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEXT-ADVENTURE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *SOURCE-COMPUTER. PC    WITH DEBUGGING MODE.
       SOURCE-COMPUTER. PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      
           SELECT BIGFIVE-FILE
               ASSIGN TO "big5test.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-BIGFIVE.
  
           SELECT TRAITS-FILE
               ASSIGN TO "big5defs.dat"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-FS-traits.   
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
           03 FILLER PIC X(01) VALUE SPACE.
               88 NO-MORE-BIGFIVE VALUE 'Y'.
             03 FILLER PIC X(01) VALUE SPACE.
               88 NO-MORE-TRAITS VALUE 'Y'.
       01 WS-FS-BIGFIVE   PIC X(02).
       01 WS-FS-TRAITS   PIC X(02).
       01  MSG1.
.      03  FILLER PIC X(33) VALUE 'BIG FIVE PERSONALITY TEST'.
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
           03  SUM-Extraversion       PIC 999.
               03  PCT-Extraversion       PIC 999V9.
          03 FILLER PIC X(20) VALUE 'AGGREABLENESS'.
           03  SUM-Agreeableness      pic 999.
               03  PCT-Agreeableness      pic 999V9.
          03 FILLER PIC X(20) VALUE 'CONSCIENTIOUSNESS'.     
           03  SUM-Conscientiousness    PIC 999.
               03  PCT-Conscientiousness    PIC 999V9.
          03 FILLER PIC X(20) VALUE 'EMOTIONAL STABILITY'. 
           03  SUM-Emotional-Stability  PIC 999.
               03  PCT-Emotional-Stability  PIC 999V9.
          03 FILLER PIC X(20) VALUE 'INTELLECT'. 
           03  SUM-Intellect           PIC 999.
               03  PCT-Intellect           PIC 999V9.
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

       01  INPSTR-TEXT PIC X(1024) VALUE "YOUR-GENERATED-INPSTR-HERE".
       01  INPSTR-LENGTH PIC 9(4) VALUE 128.
       01  INDEX-POS PIC 9(4) VALUE 1.
       01  LINE-BUFFER PIC X(80).
       01  REMAINING-LEN PIC 9(4).
       01  COPY-LEN PIC 9(4).
       01  SPACE-POS PIC 9(4).
       01  DISP-LEN PIC 99 VALUE 80.

       01  WS-TIME     PIC X(08).
       01  FILLER REDEFINES WS-TIME.
           03  WS-HH   PIC 99.
           03  WS-MM   PIC 99.
           03  WS-SS   PIC 99.
           03  WS-CC   PIC 99.
       01  WS-TOT-SECS  PIC S9(7) COMP-3.
       01  WS-SEED  PIC S9(5) COMP-3.
       01  WS-CURR-VALUE   PIC S9(13) COMP-3.
       
       01  TEMP1               PIC S9(13) COMP-3.
       01  TEMP2              PIC S9(13) COMP-3.
       01  TEMP3             PIC S9(13) COMP-3.
       01  RESULT             PIC S9(13) COMP-3.
       01  NEXT-STATE          PIC S9(13) COMP-3.
       01  CURRENT-VALUE       PIC S9(13) COMP-3.
       01  RAND-AREA.
        05  MULTIPLIER         PIC 9(5)   VALUE 7.
	    05  INCREMENT          PIC 9(5)   VALUE 3.
	    05  MODULUS            PIC 9(6)   VALUE 11.
	    05  MOD5               PIC 9(1)   VALUE 5.

       PROCEDURE DIVISION.
      D    MOVE 1 TO  DEBUG-MODE-SW
           PERFORM LOAD-BIGFIVE
           PERFORM LOAD-TRAITS
      D   IF DEBUG-MODE
      D    PERFORM RAND-ANSWERS
      D    else
           PERFORM ACCEPT-ANSWERS.
           PERFORM COMPUTE-SCORES
           PERFORM SHOW-SCORES
           GOBACK.
       LOAD-BIGFIVE.
            OPEN INPUT BIGFIVE-FILE
            MOVE 0 TO BF-COUNT
            PERFORM READ-BIGFIVE
            PERFORM UNTIL NO-MORE-BIGFIVE
                    ADD 1 TO BF-COUNT
                MOVE BIGFIVE-RECORD TO BF-REC (BF-COUNT)
                PERFORM READ-BIGFIVE
            END-PERFORM

            CLOSE BIGFIVE-FILE 
            CONTINUE.
       READ-BIGFIVE.
           READ BIGFIVE-FILE AT END SET NO-MORE-BIGFIVE TO TRUE
           END-READ
           CONTINUE. 
       LOAD-TRAITS.
            OPEN INPUT TRAITS-FILE
            MOVE 0 TO TR-COUNT
                     
            PERFORM READ-TRAITS
            PERFORM UNTIL NO-MORE-TRAITS
               ADD 1 TO TR-COUNT
      *        DISPLAY TR-REC-ID ' ' TR-REC-LVL ' ' TR-REC-DESC
               IF TR-REC-LVL = 0
                   MOVE TR-REC-DESC    TO TR-TRAIT-DESC (TR-REC-ID)
               ELSE
                  MOVE TR-REC-DESC 
                  TO TR-TRAIT-LEVEL-DESC (TR-REC-ID, TR-REC-LVL)
               END-IF
                PERFORM READ-TRAITS
            END-PERFORM
            CLOSE TRAITS-FILE 
            CONTINUE.
       READ-TRAITS.
           READ TRAITS-FILE INTO TR-RECORD
           AT END SET NO-MORE-TRAITS TO TRUE
           END-READ
           CONTINUE. 

       RAND-ANSWERS.
           ACCEPT WS-TIME FROM time
           DISPLAY WS-TIME
      D    DISPLAY 'HELLO'
           COMPUTE WS-TOT-SECS = WS-HH * 3600 + WS-MM * 60 + WS-SS
           DIVIDE WS-TOT-SECS BY MODULUS GIVING TEMP1 REMAINDER WS-SEED
           MOVE WS-SEED TO CURRENT-VALUE
      *    MOVE 0 TO CURRENT-VALUE
            PERFORM VARYING BFX FROM 1 BY +1 
            UNTIL BFX >    BF-COUNT 
      *        MOVE BF-ORIGSEQ (BFX) TO SEQNO 
      *         DISPLAY ' '
      *         DISPLAY SEQNO  ' ' BF-QUESTION (BFX)
      *         DISPLAY CHOICES
      *         DISPLAY 'Select the answer that bests applies to you' 
                PERFORM GET-NEXT-RAND
      *          DISPLAY ANS
                 MOVE ANS TO ANS-VALUE (BFX)
           END-PERFORM
  
            CONTINUE. 
       GET-NEXT-RAND.
           COMPUTE NEXT-STATE = (CURRENT-VALUE * MULTIPLIER) + INCREMENT
           DIVIDE NEXT-STATE BY MODULUS GIVING TEMP1 REMAINDER TEMP2
           DIVIDE TEMP2 BY MOD5 GIVING TEMP3 REMAINDER RESULT
           ADD 1 TO RESULT

	       MOVE RESULT TO ANS
	       MOVE TEMP2 TO CURRENT-VALUE

             CONTINUE.

       ACCEPT-ANSWERS.
           display MSG1 ' (' BF-COUNT ' items)'
           DISPLAY MSG2
           DISPLAY MSG3
         
           PERFORM VARYING BFX FROM 1 BY +1
           UNTIL BFX > BF-COUNT
            PERFORM TEST AFTER 
                UNTIL ANS-X NUMERIC AND NOT (ANS<1 OR ANS>5)
                MOVE BFX TO SEQNO 
                DISPLAY ' '
                DISPLAY SEQNO  ' ' BF-QUESTION (BFX)
                DISPLAY CHOICES
                DISPLAY 'Select the answer that bests applies to you' 
                ACCEPT ANS-X
           END-PERFORM
           MOVE ANS TO ANS-VALUE(BFX)
           END-PERFORM
  
            CONTINUE. 
       COMPUTE-SCORES.
           PERFORM VARYING SCX FROM 1 BY +1 UNTIL SCX > 5
               MOVE ZEROES TO SUM-TRAIT (SCX)
           END-PERFORM
           PERFORM VARYING BFX FROM 1 BY +1           
             UNTIL BFX > BF-COUNT
               MOVE BF-TRAIT-SCX (BFX) TO SCX

               IF BF-NEG-FLAG (BFX) = ' '
                   MOVE ANS-VALUE (BFX) TO TEMP2
                   ADD TEMP2 TO SUM-TRAIT(SCX)
               ELSE
                   SUBTRACT ANS-VALUE(BFX) FROM 6 GIVING TEMP2
                   ADD TEMP2 TO SUM-TRAIT(SCX)
               END-IF      
      *        DISPLAY 'BFC=' BFX ' SEQ=' SEQ-ID
      *        ' SCX=' SCX ' scor=' ANS-VALUE(BFX) ' ADJS=' TEMP2
               END-PERFORM
               CONTINUE.
       SHOW-SCORES.
           PERFORM VARYING SCX FROM 1 BY +1 UNTIL SCX>5
               COMPUTE  PCT-TRAIT (SCX) 
               = SUM-TRAIT (SCX) / BF-COUNT * 100
                  
               IF PCT-TRAIT(SCX) < 34.0  
                   MOVE 1 TO LVX
               ELSE
                   IF PCT-TRAIT (SCX) > 66.0
                       MOVE 3 TO LVX
                   else
                       MOVE 2 TO LVX
                   end-if
               end-if
               MOVE PCT-TRAIT (SCX) TO DSP-TRAIT   
      *       DISPLAY SCX ':' SUM-TRAIT (SCX) ' ' PCT-TRAIT (SCX) 
      *       ' ' DSP-TRAIT  '%' 
      *        ' LVL=' LVX
               DISPLAY ' '
               DISPLAY LBL-TRAIT(SCX) ' ' DSP-TRAIT  '%'
      *        DISPLAY TR-TRAIT-DESC (SCX) 
      *        DISPLAY TR-TRAIT-LEVEL-DESC(SCX,LVX)
               MOVE TR-TRAIT-DESC (SCX) TO INPSTR-TEXT
               PERFORM WORD-WRAP-INPSTR
               MOVE TR-TRAIT-LEVEL-DESC(SCX,LVX) TO INPSTR-TEXT
               PERFORM WORD-WRAP-INPSTR


           end-perform
           CONTINUE.
       WORD-WRAP-INPSTR.
           MOVE 1 TO INDEX-POS
           PERFORM UNTIL INDEX-POS > INPSTR-LENGTH
               COMPUTE REMAINING-LEN = INPSTR-LENGTH - INDEX-POS + 1
               MOVE INPSTR-TEXT(INDEX-POS:DISP-LEN) TO LINE-BUFFER
               IF REMAINING-LEN > DISP-LEN
                   MOVE  DISP-LEN TO COPY-LEN
                   PERFORM FIND-SPACE-BACKWARDS
               ELSE
                   MOVE REMAINING-LEN TO COPY-LEN
               END-IF
               MOVE SPACES TO LINE-BUFFER
               MOVE INPSTR-TEXT(INDEX-POS:COPY-LEN) TO LINE-BUFFER
               DISPLAY LINE-BUFFER
               ADD COPY-LEN TO INDEX-POS

      *        SUBTRACT 1 FROM INDEX-POS
      *        PERFORM SKIP-TO-NEXT-WORD
           END-PERFORM
           CONTINUE.

       FIND-SPACE-BACKWARDS.
           MOVE  DISP-LEN TO SPACE-POS
           PERFORM UNTIL SPACE-POS < 1 OR LINE-BUFFER(SPACE-POS:1) = ' '
               SUBTRACT 1 FROM SPACE-POS
           END-PERFORM
           IF SPACE-POS > 1
               MOVE SPACE-POS TO COPY-LEN
           END-IF.

       SKIP-TO-NEXT-WORD.
           IF INDEX-POS < INPSTR-LENGTH AND
              INPSTR-TEXT(INDEX-POS:1) IS NOT EQUAL TO ' '
               ADD 1 TO INDEX-POS
           END-IF.

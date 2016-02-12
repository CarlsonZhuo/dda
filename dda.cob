000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. DDA.
000300*---------------------------------------------------------------
000400* I declare that the assignment here submitted is original 
000500* except for sourcematerial explicitly acknowledged. I also
000600* acknowledge that I am aware of University policy and regulations
000700* on honesty in academic work, and of the disciplinary guidelines
000800* and procedures applicable to breaches of such policy and
000900* regulations, as contained in the website
001000* http://www.cuhk.edu.hk/policy/academichonesty/
001100* Assignment 1
001200* Name: ZHUO JIACHENG
001300* Student ID: 1155058590
001400* Email Addr: ZHUOJCH@LINK.CUHK.EDU.HK(IN LOWERCASE)
001500* 
001600*---------------------------------------------------------------
001700 ENVIRONMENT DIVISION.
001800 
001900 INPUT-OUTPUT SECTION.
002000 FILE-CONTROL.
002100     SELECT OUTPUT-FILE
002200          ASSIGN TO DISK
002300          ORGANIZATION IS LINE SEQUENTIAL.
002400
002500     SELECT INPUT-FILE
002600          ASSIGN TO DISK
002700          ORGANIZATION IS LINE SEQUENTIAL
002800          FILE STATUS IS FILE-OPEN-STATUS.
002900
003000 DATA DIVISION.
003100 FILE SECTION.
003200 FD  OUTPUT-FILE
003300     LABEL RECORDS ARE STANDARD
003400     VALUE OF FILE-ID IS "output.txt".
003500 01  OUTPUT-RECORD.
003600     03  OUTPUT-POINTS OCCURS 79 TIMES.
003700     05  OUTPUT-POINT                PIC X.
003800
003900 FD  INPUT-FILE
004000     LABEL RECORDS ARE STANDARD
004100     VALUE OF FILE-ID IS "input.txt".
004200 01  INPUT-RECORD.
004300     03  POINTX OCCURS 2 TIMES.
004400     05  DIGIT-OF-POINTX             PIC 9.
004500     03  POINTY OCCURS 3 TIMES.
004600     05  DIGIT-OF-POINTY             PIC 9.
004700
004800*===============================================================
004900*-----------------------START DEFINING VAR----------------------
005000*===============================================================
005100 WORKING-STORAGE SECTION.
005200* THE FOLLOWING TWO VAR ARE FOR TABLE OPERATION.
005300 01  CUR-ROW                         PIC 99 VALUE 1.
005400 01  CUR-COL                         PIC 99 VALUE 1.
005500* THE FOLLOWING FIVE VAR ARE FOR FILE READIN.
005600 01  FILE-OPEN-STATUS                PIC XX.
005700 01  NUM-OF-INPUT                    PIC 99 VALUE 1.
005800 01  POINT1X                         PIC 99 VALUE 1.
005900 01  POINT1Y                         PIC 99 VALUE 1.
006000 01  POINT2X                         PIC 99 VALUE 1.
006100 01  POINT2Y                         PIC 99 VALUE 1.
006200* THE FOLLOWING VAR ARE FOR DRAWING LINES IN BUFFER
006300 01  ROW-DRAWON                      PIC 99.
006400 01  COL-DRAWON                      PIC 99.
006500 01  SLOPE                           PIC S9(9)V9(9).
006600 01  ABS-SLO                         PIC 9(9)V9(9).
006700 01  CNT                             PIC 99 VALUE 1.
006800 01  SIGNX                           PIC S9 VALUE 1.
006900 01  SIGNY                           PIC S9 VALUE 1.
007000* THIS IS THE BUFFER TABLE. IT IS CONTROLLED BY CUR-ROW CUR-COL.
007100* THROUGHOUT THE ENTIRE PROGRAM, BF IS SHORT FOR BUFFER.
007200 01  BF-RECORD.
007300     03 BF-ROWS OCCURS 23 TIMES.
007400          05 BF-POINTS OCCURS 79 TIMES.
007500          07 BF-POINT                PIC X.
007600
007700*===============================================================
007800*------------------START MAIN PARAGRAPH-------------------------
007900* THE BASIC IDEA IS AS FOLLOW.
008000* READIN LOOP.(USING GO TO)
008100*     AFTER READIN A PAIR OF POINTS, CALL THE DRAW LINE FUNCTION.
008200*     THE FUNCTION MARKS CORRESPONDING POINTS '*' IN THE BUFFER.
008300* END READIN LOOP. (JUDGE USING NUM-OF-INPUT)
008400* WRITE THE BUFFER TO FILE.
008500*===============================================================
008600 PROCEDURE DIVISION.
008700 MAIN-LOGIC SECTION.
008800 PROGRAM-BEGIN.
008900      PERFORM OPENING-PROCEDURE.
009000      PERFORM INIT-TABLE.
009100
009200*-----PREPARE FOR THE READIN LOOP
009300 READIN-NUM-OF-INPUT.
009400      READ INPUT-FILE.
009500      MOVE DIGIT-OF-POINTX(2) TO NUM-OF-INPUT.
009600      IF DIGIT-OF-POINTX(1) NOT = SPACE
009700      COMPUTE NUM-OF-INPUT=DIGIT-OF-POINTX(1)*10 + NUM-OF-INPUT.
009800*     IS IT SAFE TO ASSUME NO MORE THAN 100 INPUT?
009900*-----PERFORM THE READIN LOOP.
010000 KEEP-READIN.
010100      READ INPUT-FILE.
010200      PERFORM LOAD-READIN-TO-POINT2XY.
010300      IF CUR-ROW = 1 GO TO NEW-TO-OLD.
010400      MOVE 1 TO CNT.
010500      PERFORM ADD-LINE-TO-BUFFER.
010600 NEW-TO-OLD.
010700      MOVE POINT2X TO POINT1X.
010800      MOVE POINT2Y TO POINT1Y.
010900      ADD 1 TO CUR-ROW.
011000      IF CUR-ROW < NUM-OF-INPUT + 1 GO TO KEEP-READIN.
011100*-----END LOOP.
011200 
011300 OUTPUT-TO-FILE.
011400      MOVE 23 TO CUR-ROW.
011500      PERFORM WRITE-TO-FILE.
011600      PERFORM CLOSING-PROCEDURE.
011700
011800 PROGRAM-DONE.
011900      STOP RUN.
012000
012100*===============================================================
012200*-----------------------UTILITY PARAGRAPH-----------------------
012300*===============================================================
012400 OPENING-PROCEDURE.
012500      OPEN OUTPUT OUTPUT-FILE.
012600      OPEN INPUT INPUT-FILE.
012700      IF FILE-OPEN-STATUS NOT = '00' GO TO OPEN-ERR-HANDLING.
012800
012900 
013000 CLOSING-PROCEDURE.
013100      CLOSE OUTPUT-FILE.
013200      CLOSE INPUT-FILE.
013300 
013400 
013500*-----AFTER INIT-TABLE, THE FIRST COL OF THE TABLE ARE ALL '|'
013600*-----THE FIRST ROW OF THE TABLE ARE ALL '-'
013700*-----THE ORIGIN POINT IS '+'
013800 INIT-TABLE.
013900      PERFORM INIT-COL.
014000      PERFORM INIT-ROW.
014100      MOVE '+' TO BF-POINT(1, 1).
014200 INIT-COL.
014300      MOVE '|' TO BF-POINT(CUR-ROW, 1).
014400      COMPUTE CUR-ROW = CUR-ROW + 1.
014500      IF CUR-ROW < 24 GO TO INIT-COL.
014600      MOVE 1 TO CUR-ROW.
014700 INIT-ROW.
014800      MOVE '-' TO BF-POINT(1, CUR-COL).
014900      COMPUTE CUR-COL = CUR-COL + 1.
015000      IF CUR-COL < 80 GO TO INIT-ROW.
015100      MOVE 1 TO CUR-COL.
015200 
015300
015400*-----AFTER LOAD-READIN-TO-POINT2XY
015500*-----(1) THE NEXT (X,Y) COORDINATE IS READIN,
015600*-----    AND STORED AT (POINT2X, POINT2Y).
015700*-----(2) THE CURSOR OF THE INPUT-FILE IS MOVED ONE LINE DOWNWARD.
015800 LOAD-READIN-TO-POINT2XY.
015900      MOVE DIGIT-OF-POINTX(2) TO POINT2X.
016000      IF DIGIT-OF-POINTX(1) NOT = SPACE
016100            COMPUTE POINT2X = DIGIT-OF-POINTX(1) * 10 + POINT2X.
016200      MOVE DIGIT-OF-POINTY(3) TO POINT2Y.
016300      IF DIGIT-OF-POINTY(2) NOT = SPACE
016400            COMPUTE POINT2Y = DIGIT-OF-POINTY(2) * 10 + POINT2Y.
016500      ADD 1 TO POINT2X.
016600      ADD 1 TO POINT2Y.
016700 
016800 
016900*-----AFTER ADD-LINE-TO-BUFFER THE BUFFER(BF-POINT) ARE MODIFIED. 
017000*-----(1) (POINT1X, POINT1Y) (POINT2X, POINT2Y) ARE CONNECTED VIA
017100*-----    DDA METHOD.
017200*-----(2) THE CORRESPONDING POINTS ON THE LINE IS MARKED AS '*'.
017300*-----IMPLEMENTATION NOTE:
017400*-----    BUFFER.ROW -> COORDINATE.Y
017500*-----    BUFFER.COL -> COORDINATE.X
017600 ADD-LINE-TO-BUFFER.
017700      MOVE POINT1X TO COL-DRAWON.
017800      MOVE POINT1Y TO ROW-DRAWON.
017900      IF POINT2X - POINT1X < 0 MOVE -1 TO SIGNX.
018000      IF POINT2X - POINT1X NOT < 0 MOVE 1 TO SIGNX.
018100      IF POINT2Y - POINT1Y < 0 MOVE -1 TO SIGNY.
018200      IF POINT2Y - POINT1Y NOT < 0 MOVE 1 TO SIGNY.
018300      COMPUTE SLOPE = (POINT2Y - POINT1Y) / (POINT2X - POINT1X).
018400      IF SLOPE > 0 MOVE SLOPE TO ABS-SLO.
018500      IF SLOPE NOT > 0 COMPUTE ABS-SLO = SLOPE * -1.
018600      PERFORM KEEP-DRAWING.
018700 KEEP-DRAWING.
018800      MOVE '*' TO BF-POINT(ROW-DRAWON, COL-DRAWON).
018900      IF SLOPE NOT > 1 AND SLOPE NOT < -1
019000            COMPUTE ROW-DRAWON ROUNDED = POINT1Y +
019100                                         CNT * ABS-SLO * SIGNY
019200            COMPUTE COL-DRAWON = POINT1X + CNT * SIGNX.
019300      IF SLOPE > 1 OR SLOPE < -1
019400            COMPUTE ROW-DRAWON = POINT1Y + CNT * SIGNY
019500            COMPUTE COL-DRAWON ROUNDED = POINT1X +
019600                                         CNT * SIGNX / ABS-SLO.
019700      ADD 1 TO CNT.
019800      IF ROW-DRAWON NOT = POINT2Y OR COL-DRAWON NOT = POINT2X
019900          GO TO KEEP-DRAWING.
020000      
020100 
020200 WRITE-TO-FILE.
020300      WRITE OUTPUT-RECORD FROM BF-ROWS(CUR-ROW).
020400      COMPUTE CUR-ROW = CUR-ROW - 1.
020500      IF CUR-ROW > 0 GO TO WRITE-TO-FILE.
020600 
020700 
020800 OPEN-ERR-HANDLING.
020900      DISPLAY 'File Open Error. Program terminated.'.
021000      STOP RUN.


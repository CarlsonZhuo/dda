c ===========================================================
c I declare that the assignment here submitted is original 
c except for sourcematerial explicitly acknowledged. I also
c acknowledge that I am aware of University policy and regula-
c tions on honesty in academic work, and of the disciplinary 
c guidelines and procedures applicable to breaches of such  
c policy and regulations, as contained in the website
c http://www.cuhk.edu.hk/policy/academichonesty/
c Assignment 1
c Name: ZHUO JIACHENG
c Student ID: 1155058590
c Email Addr: ZHUOJCH@LINK.CUHK.EDU.HK(IN LOWERCASE)
c ===========================================================
c ---------------------SUBROUTINE DEF------------------------
c ===========================================================
c THIS SUBROUTINE CONVERTS CHAR INTO INT.(TYPE CONVERT)
c INPUT:      CHARTWO(CHAR WITH LENTH 2)
c OUTPUT:     INT_RESULT(AN INTEGER THAT CORRESPONDING TO)
c ===========================================================
      SUBROUTINE CHARTWO2INT(CHARTWO, INT_RESULT)
      CHARACTER*2 CHARTWO
      INTEGER INT_RESULT, INT_RESULT1, INT_RESULT2

      INT_RESULT1 = INT(CHARTWO(1:1)) - INT('0')
      INT_RESULT2 = INT(CHARTWO(2:2)) - INT('0')
      INT_RESULT = 0

      IF (INT_RESULT1 .EQ. -16) GOTO 1
            INT_RESULT = INT_RESULT1 * 10
 1    INT_RESULT = INT_RESULT + INT_RESULT2
      RETURN
      END
c ===========================================================
c THIS SUBROUTINE RENDERS/PLOTS RESULT(2D ARRAY) TO CONSOLE
c NOTICE THAT RESULT.ROW -> COORDINATE.Y
c             RESULT.COL -> COORDINATE.X
c AND THE SEQUENCE OF ROW OF RESULT SHOULD BE REVERSED.
c INPUT:      RESULT.
c OUTPUT:     NONE. (PLOT RESULT TO CONSOLE).
c ===========================================================
      SUBROUTINE RENDER(RESULT)
      INTEGER CUR_ROW, CUR_COL
      CHARACTER RESULT(23, 79)

      CUR_ROW = 23
 13   IF (CUR_ROW .EQ. 0) GOTO 14
            CUR_COL = 1
 11         IF (CUR_COL .GT. 79) GOTO 12
                  WRITE(*, 50) RESULT(CUR_ROW, CUR_COL)
                  CUR_COL = CUR_COL + 1
                  GOTO 11
 12         WRITE (*, 60) ''
            CUR_ROW = CUR_ROW - 1
            GOTO 13

 10   FORMAT(I,I)
 50   FORMAT(A, $)
 60   FORMAT(A)
 14   RETURN
      END
c ===========================================================
c GIVEN A(AX,AY), B(BX,BY), THIS SUBROUTINE(AKA DDA)
c (1) FINDS OUT THE POINTS THAT CONNECT AB INTO A LINE.
c (2) MODIFIES THE CORRESPONDING PLACE IN RESULT TO '*'.
c INPUT:    AX, AY, BX, BY(COORDINATE OF A AND B)
c OUTPUT:   RESULT(MODEIFED)
c ===========================================================
      SUBROUTINE INSERT_TWO_POINTS(AX, AY, BX, BY, RESULT)
      INTEGER AX, AY, BX, BY, CUR_X, CUR_Y, CNT
      CHARACTER RESULT(23, 79)
      REAL M

      CUR_X = AX
      CUR_Y = AY
      CNT = 1

      M = REAL(BY - AY) / (BX - AX)

 104  IF (CUR_X .EQ. BX .AND. CUR_Y .EQ. BY) GOTO 101
            RESULT(CUR_Y + 1, CUR_X + 1) = '*'
            IF (ABS(M) .GT. 1) GOTO 102
                  CUR_X = AX + SIGN(CNT, (BX - AX))
                  CUR_Y = NINT(AY + SIGN((CNT * M), (BY - AY)))
                  GOTO 103
 102              CUR_X = NINT(AX + SIGN((CNT / M), (BX - AX)))
                  CUR_Y = AY + SIGN(CNT, (BY - AY))
                  GOTO 103
 103              CNT = CNT + 1
                  GOTO 104
 101  RESULT(BY + 1,BX + 1) = '*'

 10   FORMAT(F)
 20   FORMAT(I, I)
      RETURN
      END     
c ===========================================================


      PROGRAM READIN
      IMPLICIT NONE

c ------------VARIABLE DECLARATION------------
c (1) ITER_TIME_STR ITER_TIME: NUM OF INPUT POINTS(n IN SPEC)
c (2) CUR_POINT: THE CURRENT PROCESSING INPUT POINT.
c (3) ERR: THE ERROR MESSAGE PRINTED WHEN 'OPEN_FILE' FAILS.
      CHARACTER*2 ITER_TIME_STR
      CHARACTER*2 X_COR_TEMP_STR, Y_COR_TEMP_STR
      INTEGER ITER_TIME,  CUR_POINT
      INTEGER X_COR_TEMP, Y_COR_TEMP, PREV_X, PREV_Y
      INTEGER CUR_COL, CUR_ROW
      INTEGER IO_STAT
      CHARACTER*20 ERR
      CHARACTER RESULT(23, 79)

c ---------------VARIABLE INIT---------------
      ERR = 'File Open Error'
      CUR_POINT = 0
      CUR_COL = 1
      CUR_ROW = 1
c-----INIT THE RESULT(2D ARRAY)(1)(2)(3)(4)
c-----(1) ALL INIT AS EMPTY SPACES.
 25   IF (CUR_ROW .GT. 23) GOTO 24
            CUR_COL = 1
 27         IF (CUR_COL .GT. 79) GOTO 26
                  RESULT(CUR_ROW, CUR_COL) = ' '
                  CUR_COL = CUR_COL + 1
                  GOTO 27
 26         CUR_ROW = CUR_ROW + 1
            GOTO 25

 24   CUR_COL = 1
      CUR_ROW = 1
c-----(2) COVER THE FIRST ROW BY '-'
 21   IF (CUR_COL .GT. 79) GOTO 22
           RESULT(1, CUR_COL) = '-'
           CUR_COL = CUR_COL + 1
           GOTO 21
c-----(3) COVER THE FIRST COL BY '|'
 22   IF (CUR_ROW .GT. 23) GOTO 23
            RESULT(CUR_ROW, 1) = '|'
            CUR_ROW = CUR_ROW + 1
            GOTO 22
c-----(4) COVER THE ORIGIN POINT WITH '+'
 23   RESULT(1, 1) = '+'

c -----------------READ FILE-----------------
      OPEN(UNIT=2, ERR=99, IOSTAT = IO_STAT,
     +     FILE='input.txt', STATUS='OLD')

c-----READIN THE NUM OF INPUT POINTS TO ITER_TIME.
      READ(2, 30) ITER_TIME_STR
      CALL CHARTWO2INT(ITER_TIME_STR, ITER_TIME)

c-----READIN THE (X,Y) OF ONE POINT
 2    IF (CUR_POINT .EQ. ITER_TIME ) GOTO 3
      READ(2, 40) X_COR_TEMP_STR, Y_COR_TEMP_STR    
      CALL CHARTWO2INT(X_COR_TEMP_STR, X_COR_TEMP)
      CALL CHARTWO2INT(Y_COR_TEMP_STR, Y_COR_TEMP)

      IF (CUR_POINT .EQ. 0) GOTO 4

c-----PERFORM DDA.
      CALL INSERT_TWO_POINTS(X_COR_TEMP, Y_COR_TEMP 
     + , PREV_X, PREV_Y, RESULT)

c-----MOVE TO THE NEXT POINT.
 4    PREV_X = X_COR_TEMP
      PREV_Y = Y_COR_TEMP
      CUR_POINT = CUR_POINT + 1
      GOTO 2

 3    CALL RENDER(RESULT)

      CLOSE(2)


 10   FORMAT(I)
 99   IF (IO_STAT .NE. 0) GOTO 98
            GOTO 97
 98   WRITE (*, 50) ERR 
 30   FORMAT(A2)
 40   FORMAT(A2, A3)
 50   FORMAT(A)
      
 97   STOP
      END

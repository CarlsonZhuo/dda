
c -----------------------------------------------------------
      SUBROUTINE CHARTWO2INT(CHARTWO, INT_RESULT)
      CHARACTER*2 CHARTWO
      INTEGER INT_RESULT, INT_RESULT1, INT_RESULT2

      INT_RESULT1 = INT(CHARTWO(1:1)) - INT('0')
      INT_RESULT2 = INT(CHARTWO(2:2)) - INT('0')
      INT_RESULT = 0

c     If the empty space in the first character
      IF (INT_RESULT1 .EQ. -16) GOTO 1
            INT_RESULT = INT_RESULT1 * 10
 1    INT_RESULT = INT_RESULT + INT_RESULT2
      RETURN
      END
c -----------------------------------------------------------
 
c ---------------------------------------------------     
      SUBROUTINE RENDER(RESULT)
      INTEGER CUR_ROW, CUR_COL
      CHARACTER RESULT(23, 79)

      CUR_ROW = 23
 13   IF (CUR_ROW .EQ. 0) GOTO 14
            CUR_COL = 1
 11         IF (CUR_COL .GT. 79) GOTO 12
C                 WRITE(*, 10) CUR_ROW, CUR_COL
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
c ---------------------------------------------------     

c ----------------------------------------------------
      SUBROUTINE INSERT_TWO_POINTS(AX, AY, BX, BY, RESULT)
      INTEGER AX, AY, BX, BY, CUR_X, CUR_Y, CNT
      CHARACTER RESULT(23, 79)
      REAL M

      CUR_X = AX
      CUR_Y = AY
      CNT = 1

      M = REAL(BY - AY) / (BX - AX)
C     WRITE(*, 10) M

 104  IF (CUR_X .EQ. BX .AND. CUR_Y .EQ. BY) GOTO 101
            RESULT(CUR_Y + 1, CUR_X + 1) = '*'
C           WRITE(*, 20) CUR_X, CUR_Y
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
c ----------------------------------------------------


      PROGRAM READIN
      IMPLICIT NONE

c Variables declaration
c ITER_TIME_STR and ITER_TIME record the number of input points
c CUR_POINT record the current processing point
c ERR is the error message printed when OPEN_FILE fails.
      CHARACTER*2 ITER_TIME_STR
      CHARACTER*2 X_COR_TEMP_STR, Y_COR_TEMP_STR
      INTEGER ITER_TIME,  CUR_POINT
      INTEGER X_COR_TEMP, Y_COR_TEMP, PREV_X, PREV_Y
      INTEGER CUR_COL, CUR_ROW
      CHARACTER*20 ERR
      CHARACTER RESULT(23, 79)

c Variables init
      ERR = 'File Open Error'
      CUR_POINT = 0
      CUR_COL = 1
      CUR_ROW = 1

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

 21   IF (CUR_COL .GT. 79) GOTO 22
           RESULT(1, CUR_COL) = '-'
           CUR_COL = CUR_COL + 1
           GOTO 21

 22   IF (CUR_ROW .GT. 23) GOTO 23
            RESULT(CUR_ROW, 1) = '|'
            CUR_ROW = CUR_ROW + 1
            GOTO 22

 23   RESULT(1, 1) = '+'


c Read file
      OPEN(UNIT=2, ERR=20, FILE='input.txt', STATUS='OLD')

c     Read in the iteration time, ITER_TIME
      READ(2, 30) ITER_TIME_STR
      CALL CHARTWO2INT(ITER_TIME_STR, ITER_TIME)
c     WRITE (*, 10) ITER_TIME

 2    IF (CUR_POINT .EQ. ITER_TIME ) GOTO 3
      READ(2, 40) X_COR_TEMP_STR, Y_COR_TEMP_STR    
      CALL CHARTWO2INT(X_COR_TEMP_STR, X_COR_TEMP)
      CALL CHARTWO2INT(Y_COR_TEMP_STR, Y_COR_TEMP)

      IF (CUR_POINT .EQ. 0) GOTO 4
      CALL INSERT_TWO_POINTS(X_COR_TEMP, Y_COR_TEMP 
     + , PREV_X, PREV_Y, RESULT)
 4    PREV_X = X_COR_TEMP
      PREV_Y = Y_COR_TEMP
      CUR_POINT = CUR_POINT + 1
      GOTO 2

 3    CALL RENDER(RESULT)
      

 10   FORMAT(I)
 20   WRITE (*, 10) ERR
 30   FORMAT(A2)
 40   FORMAT(A2, A3)
 50   FORMAT(A)

      STOP
      END

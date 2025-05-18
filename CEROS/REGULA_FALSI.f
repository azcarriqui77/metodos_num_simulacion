      PROGRAM REGULA_FALSI
              IMPLICIT NONE
              REAL*8 A, B, FA, FB, C, FC, CANT, FUNCION
              REAL*8 EPSILON
              INTEGER I

              A=1
              B=3

              WRITE(*,*) "EPSILON: "
              READ(*,*) EPSILON

              C=1
              CANT=2
              I=0

              DO WHILE (ABS(C-CANT).GT.EPSILON)
                 CANT=C
                 
                 FA=FUNCION(A)
                 FB=FUNCION(B)
                 
                 C=(A*FB-B*FA)/(FB-FA)
                 FC=FUNCION(C)
                 
                 IF(FC*FA.EQ.0) THEN
                     C=C
                 ELSE IF(FC*FA.GT.0) THEN
                     A=C
                 ELSE IF(FC*FA.LT.0) THEN
                     B=C
                 END IF
                 I=I+1
              END DO

              WRITE(*,*)" TRAS ", I, " ITERACIONES, LA SOLUCION ES: "
              WRITE(*,*) C
              
              PAUSE
              STOP
      END PROGRAM
      
      REAL*8 FUNCTION FUNCION (X)
             REAL*8 X, F
             F=X**3-2*X-5
             FUNCION=F
             RETURN
      END FUNCTION FUNCION

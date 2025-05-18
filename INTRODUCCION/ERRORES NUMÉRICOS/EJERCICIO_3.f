      PROGRAM EJERCICIO_3
              IMPLICIT NONE
              
              REAL*8 Y,ERROR,TAYLOR
              REAL*8 PASO,X
              INTEGER N
              
              OPEN(UNIT=200,FILE="ejercicio2-3.dat")
              WRITE(*,*) "PASO: "
              READ(*,*) PASO
              X=0.0D0
              DO WHILE (X.LE.10)
                Y=TAYLOR(N,X)
                ERROR=ABS(Y-1.0D0*EXP(-X))
                WRITE(200,*) X,Y,N
                WRITE(*,*) X,Y,EXP(-X),ERROR,N
                X=X+PASO
              END DO
              CLOSE(200)


              PAUSE
              STOP
      END PROGRAM EJERCICIO_3
      
      REAL*8 FUNCTION TAYLOR (N,X)
             REAL*8 X
             INTEGER N,I
             REAL*8 FACTORIAL
             I=0
             DO WHILE((X**I/FACTORIAL(I)).GT.(1.0D-7))
                TAYLOR=TAYLOR+(-X)**I/FACTORIAL(I)
                I=I+1
             END DO
             N=I-1

             RETURN
      END FUNCTION TAYLOR

      REAL*8 FUNCTION FACTORIAL(A)
              INTEGER A
              INTEGER J
              J=1
              FACTORIAL=1.0D0
              IF (A.EQ.0) THEN
                 FACTORIAL=1.0D0
              ELSE IF(A.GT.0) THEN
                   DO J=1,A
                      FACTORIAL=FACTORIAL*J
                   END DO
              END IF

              RETURN
      END FUNCTION FACTORIAL

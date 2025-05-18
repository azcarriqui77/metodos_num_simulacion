      PROGRAM BISECCION
              IMPLICIT NONE
              REAL*8 A,B,FA,FB,C,FC,CANT,FUNCION
              REAL*8 EPSILON
              INTEGER I
              
              !WRITE(*,*) "A: "
              !READ(*,*) A
              !WRITE(*,*) "B: "
              !READ(*,*) B
              A=1
              B=2
              
              WRITE(*,*) "EPSILON: "
              READ(*,*) EPSILON
              
              FA=FUNCION(A)
              FB=FUNCION(B)
              
              C=1
              CANT=2
              I=0
              
              DO WHILE (ABS(C-CANT).GT.EPSILON)
                 CANT=C
                 C=(A+B)/2
                 FC=FUNCION(C)
                 IF(FC*FA.EQ.0) THEN
                     C=C
                 ELSE IF(FC*FA.GT.0) THEN
                     A=C
                     FA=FC
                 ELSE IF(FC*FA.LT.0) THEN
                     B=C
                     FB=FC
                 END IF
                 I=I+1
              END DO
              
              WRITE(*,*)" TRAS ", I, " ITERACIONES, LA SOLUCION ES: "
              WRITE(*,*) C
              
              PAUSE
              STOP
      END PROGRAM
      
      REAL*8 FUNCTION FUNCION (X)
              REAL*8 X, FX
              FX=X**3+2*X**2+10*X-20
              FUNCION=FX
              
              RETURN
      END FUNCTION FUNCION
                 
              
              

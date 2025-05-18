      PROGRAM NEWTON_RAPHSON
              IMPLICIT NONE
              REAL*8 FUNCION, DERIVADA
              REAL*8 X, C, F, D
              REAL*8 EPSILON
              INTEGER I

              WRITE(*,*) "EPSILON: "
              READ(*,*) EPSILON

              C=5
              F=FUNCION(C)
              D=DERIVADA(C)
              X=C-F/D

              I=1

              DO WHILE ((ABS(C-X)).GT.EPSILON)
                 C=X
                 F=FUNCION(C)
                 D=DERIVADA(C)
                 X=C-F/D
                 I=I+1
              END DO

              WRITE(*,*)" TRAS ", I, " ITERACIONES, LA SOLUCION ES: "
              WRITE(*,*) X

              PAUSE
              STOP
      END PROGRAM

      REAL*8 FUNCTION FUNCION (X)
              REAL*8 X, FX
              FX=70*EXP(-1.5*X)+25*EXP(-0.075*X)-9
              FUNCION=FX

              RETURN
      END FUNCTION FUNCION
      
      REAL*8 FUNCTION DERIVADA (X)
             REAL*8 X, D
             D=-105*EXP(-1.5*X)-1.875*EXP(-0.075*X)
             DERIVADA=D
             RETURN
      END FUNCTION DERIVADA

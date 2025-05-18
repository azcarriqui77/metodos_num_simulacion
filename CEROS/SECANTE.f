      PROGRAM SECANTE
              IMPLICIT NONE
              REAL*8 FUNCION
              REAL*8 C(3),F(3)
              REAL*8 EPSILON
              INTEGER I

              WRITE(*,*) "EPSILON: "
              READ(*,*) EPSILON

              C(2)=1.5
              C(3)=2

              F(2)=FUNCION(C(2))
              F(3)=FUNCION(C(3))

              C(1)=C(2)-F(2)*((C(2)-C(3))/(F(2)-F(3)))
              I=1

              DO WHILE (ABS(C(1)-C(2)).GT.EPSILON)
                 C(3)=C(2)
                 C(2)=C(1)
                 
                 F(2)=FUNCION(C(2))
                 F(3)=FUNCION(C(3))
                 
                 C(1)=C(2)-F(2)*((C(2)-C(3))/(F(2)-F(3)))
                 I=I+1
              END DO

              WRITE(*,*)" TRAS ", I, " ITERACIONES, LA SOLUCION ES: "
              WRITE(*,*) C(1)

              PAUSE
              STOP
      END PROGRAM

      REAL*8 FUNCTION FUNCION (X)
              REAL*8 X, FX
              FX=X**3+2*X**2+10*X-20
              FUNCION=FX

              RETURN
      END FUNCTION FUNCION




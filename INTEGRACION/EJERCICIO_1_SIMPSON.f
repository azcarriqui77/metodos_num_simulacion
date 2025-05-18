      PROGRAM INTEGRALES
              IMPLICIT NONE
              REAL*8 A,B,I
              REAL*8 ERROR
              REAL*8 SIMPSON

              WRITE(*,*) "A:"
              READ(*,*) A
              WRITE(*,*) "B:"
              READ(*,*) B
              WRITE(*,*) "ERROR: "
              READ(*,*) ERROR

              I=SIMPSON(A,B,ERROR)
              WRITE(*,*) "EL VALOR DE LA INTEGRAL ES: ", I

              PAUSE
              STOP
      END PROGRAM

      REAL*8 FUNCTION SIMPSON(A,B,ERROR)
             REAL*8 A,B,ERROR,H,D
             REAL*8 I1,I2,EPSILON
             REAL*8 SUMA1,SUMA2,SUMA3
             INTEGER N,I
             OPEN(UNIT=201,FILE="pto-error2.dat")
             N=2
             H=(B-A)/N
             SUMA1=FUNCION(A)+FUNCION(B)
             SUMA2=FUNCION(A+H)
             SUMA3=FUNCION(A+H/2)+FUNCION(A+3*H/2)
             I1=H/3*(SUMA1+4*SUMA2)
             I2=H/6*(SUMA1+2*SUMA2+4*SUMA3)
             EPSILON=ABS(I2-I1)
             
             DO WHILE(ABS(EPSILON).GT.ERROR)
                N=2*N
                H=(B-A)/N
                I1=I2
                SUMA2=SUMA2+SUMA3
                SUMA3=0.0
                D=H/2
                
                DO I=1,N
                   SUMA3=SUMA3+FUNCION(A+(2*I-1)*D)
                END DO
                
                I2=H/6*(SUMA1+2*SUMA2+4*SUMA3)
                EPSILON=ABS(I2-I1)
                WRITE(201,*) N,EPSILON,I1
                WRITE(*,*) N,EPSILON,I1
                
                END DO
             CLOSE(201)
             SIMPSON=I1
      END FUNCTION

      REAL*8 FUNCTION FUNCION(X)
             REAL*8 X
             REAL*8 Y
             Y=EXP(-X)*SIN(X)
             FUNCION=Y
      END FUNCTION





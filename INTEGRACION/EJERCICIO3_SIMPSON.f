      PROGRAM INTEGRALES
              IMPLICIT NONE
              REAL*8 A,B,X,I
              REAL*8 ERROR
              REAL*8 SIMPSON
              OPEN(UNIT=201,FILE="datos.dat")
              A=0
              B=3.1415926536
              ERROR=1E-6
              X=-0.04
              DO WHILE (X.LE.0.04)
                 I=SIMPSON(A,B,ERROR,X)
                 I=I*(0.25*0.03**3)/2
                 WRITE(201,*) X,I
                 X=X+0.001
              END DO
              CLOSE(201)

              PAUSE
              STOP
      END PROGRAM

      REAL*8 FUNCTION SIMPSON(A,B,ERROR)
             REAL*8 A,B,ERROR,H,D,X
             REAL*8 I1,I2,EPSILON
             REAL*8 SUMA1,SUMA2,SUMA3
             INTEGER N,I

             N=2
             H=(B-A)/N
             SUMA1=FUNCION(A,X)+FUNCION(B,X)
             SUMA2=FUNCION(A+H,X)
             SUMA3=FUNCION(A+H/2,X)+FUNCION(A+3*H/2,X)
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
                   SUMA3=SUMA3+FUNCION(A+(2*I-1)*D,X)
                END DO

                I2=H/6*(SUMA1+2*SUMA2+4*SUMA3)
                EPSILON=ABS(I2-I1)

             END DO
             SIMPSON=I1
      END FUNCTION

      REAL*8 FUNCTION FUNCION(X,M)
             REAL*8 X
             REAL*8 Y
             Y=(SIN(X))**3
             Y=Y/((M*M+0.03**2-0.06*M*COS(X))**(3/2))
             FUNCION=Y
      END FUNCTION





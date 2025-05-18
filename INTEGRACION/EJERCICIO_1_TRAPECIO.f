      PROGRAM INTEGRALES
              IMPLICIT NONE
              REAL*8 A,B,I
              REAL*8 ERROR
              REAL*8 CHECK_ERROR
              
              WRITE(*,*) "A:"
              READ(*,*) A
              WRITE(*,*) "B:"
              READ(*,*) B
              WRITE(*,*) "ERROR: "
              READ(*,*) ERROR
              
              I=CHECK_ERROR(A,B,ERROR)
              WRITE(*,*) "EL VALOR DE LA INTEGRAL ES: ", I
              
              PAUSE
              STOP
      END PROGRAM

      REAL*8 FUNCTION CHECK_ERROR(A,B,ERROR)
             REAL*8 A,B,ERROR,TRAPECIO
             REAL*8 I1,I2,EPSILON
             INTEGER N
             OPEN(UNIT=201,FILE="pto-error1.dat")
             N=1
             I2=1.0
             I1=0.5
             EPSILON=ABS(I2-I1)
             DO WHILE(ABS(EPSILON).GT.ERROR)
                N=2*N
                I1=TRAPECIO(A,B,N)
                I2=TRAPECIO(A,B,2*N)
                EPSILON=ABS(I2-I1)
                WRITE(201,*) N,EPSILON,I1
                WRITE(*,*) N,EPSILON,I1
                END DO
             CLOSE(201)
             CHECK_ERROR=I1
      END FUNCTION
      
      REAL*8 FUNCTION TRAPECIO(A,B,N)
             REAL*8 A,B
             REAL*8 INTEGRAL,H
             REAL*8 FUNCION
             INTEGER N
             INTEGER I
             H=(B-A)/N
             INTEGRAL=0.0
             DO I=1,N-1
                INTEGRAL=INTEGRAL+FUNCION(A+H*I)
             END DO

             INTEGRAL=INTEGRAL*2
             INTEGRAL=INTEGRAL+FUNCION(A)+FUNCION(B)
             INTEGRAL=INTEGRAL*H/2
             
             TRAPECIO=INTEGRAL
      END FUNCTION
      
      REAL*8 FUNCTION FUNCION(X)
             REAL*8 X
             REAL*8 Y
             Y=EXP(-X*X)*SIN(X)
             FUNCION=Y
      END FUNCTION

             
              


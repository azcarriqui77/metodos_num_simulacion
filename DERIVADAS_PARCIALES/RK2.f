      PROGRAM RK2
              IMPLICIT NONE
              REAL*8, ALLOCATABLE:: X(:),Y(:)
              REAL*8 A, B, H, F, K1, K2, T, ERROR
              REAL*8 FUNCION, FUNCION_AN
              INTEGER I, N
              
              OPEN (UNIT=501, FILE="datos_RK2.dat")
              
              WRITE(*,*) "N: "
              READ(*,*) N
              
              A=0
              B=2
              
              ALLOCATE( X(0:N) )
              ALLOCATE( Y(0:N) )
              X(0)=0
              
              H=(B-A)/N
              
              T=A
              
              DO I=0,N-1
                 F=FUNCION(T,X(I))
                 K1=H*F
                 F=FUNCION(T+H,X(I)+K1)
                 K2=H*F
                 X(I+1)=X(I)+0.5*(K1+K2)
                 Y(I)=FUNCION_AN(T)
                 T=T+H
              END DO
              
              Y(N)=FUNCION_AN(T)

              DO I=0,N
                 ERROR=ABS(X(I)-Y(I))/Y(I)
                 WRITE(501,*) A+I*H, X(I), Y(I), ERROR
              END DO
              
              CLOSE(501)
              

              PAUSE
              STOP
      END PROGRAM
      
      REAL*8 FUNCTION FUNCION (X,Y)
             REAL*8 X,Y
             FUNCION=X*X+Y
             RETURN
      END FUNCTION FUNCION
      
      REAL*8 FUNCTION FUNCION_AN(X)
             REAL*8 X
             FUNCION_AN=2*EXP(X)-X*X-2*X-2
             RETURN
      END FUNCTION FUNCION_AN

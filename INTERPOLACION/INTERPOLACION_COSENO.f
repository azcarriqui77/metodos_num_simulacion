      PROGRAM INTERPOLACION_COSENO
              IMPLICIT NONE
              
              INTEGER N,I
              REAL X(8)
              REAL Y(8)
              REAL C(8)
              REAL EVALUAR_POLINOMIO
              REAL X0,PASO,Y0
              REAL*8 ERROR
              
              OPEN(UNIT=201, FILE="valores.dat")
              
              N=8
              PASO=0.01
              
              X(1)=-4
              X(2)=-3
              X(3)=0
              X(4)=0.25
              X(5)=1
              X(6)=1.5
              X(7)=2
              X(8)=2.5
              
              Y(1)=COS(-4.0)
              Y(2)=COS(-3.0)
              Y(3)=COS(0.0)
              Y(4)=COS(0.25)
              Y(5)=COS(1.0)
              Y(6)=COS(1.5)
              Y(7)=COS(2.0)
              Y(8)=COS(2.5)

              CALL CALCULA_POLINOMIO(N,X,Y,C)
              DO I=1,N,1
                 WRITE(*,*) "C(",I,"): "
                 WRITE(*,*) C(I)
              END DO
              
              X0=-4.0
              DO WHILE (X0.LE.4)
                 Y0=EVALUAR_POLINOMIO(X0,X,C,N)
                 ERROR=(ABS(COS(X0)-Y0))
                 WRITE(*,*) X0,Y0,COS(X0),ERROR
                 WRITE(201,*) X0,Y0,COS(X0),ERROR
                 X0=X0+PASO
              END DO
              
              CLOSE(201)
              PAUSE
              STOP
      END PROGRAM INTERPOLACION_COSENO

              
      SUBROUTINE CALCULA_POLINOMIO(N,X,Y,C)
              REAL X(N),Y(N),C(N)
              REAL P,AUX
              INTEGER I,J,K

              C(1)=Y(1)
              C(2)=1.0*(Y(2)-C(1))/(X(2)-X(1))
              DO I=3,N,1
                 P=C(1)
                 DO K=2,I-1,1
                 AUX=1.0
                    DO J=1,K-1,1
                       AUX=AUX*(X(I)-X(J))
                    END DO
                    P=P+C(K)*AUX
                 END DO
                 C(I)=1.0*(Y(I)-P)/(AUX*(X(I)-X(I-1)))
              END DO
              RETURN
      END SUBROUTINE CALCULA_POLINOMIO
      
      REAL FUNCTION EVALUAR_POLINOMIO (X0,X,C,N)
           REAL X0,X(N),C(N)
           INTEGER I,N
           REAL VALOR

           VALOR=0.0
           DO I=N,1,-1
              VALOR=VALOR*(X0-X(I))+C(I)
           END DO

           EVALUAR_POLINOMIO=VALOR
           RETURN
      END FUNCTION EVALUAR_POLINOMIO

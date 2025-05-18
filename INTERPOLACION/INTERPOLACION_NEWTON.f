      PROGRAM INTERPOLACION_NEWTON
              IMPLICIT NONE
              INTEGER N,I
              REAL, ALLOCATABLE::X(:)
              REAL, ALLOCATABLE::Y(:)
              REAL, ALLOCATABLE::C(:)
              REAL X0,VALOR
              REAL EVALUAR_POLINOMIO
              
              WRITE(*,*) "NUMERO DE PUNTOS: "
              READ(*,*)N

              ALLOCATE(X(1:N))
              ALLOCATE(Y(1:N))
              ALLOCATE(C(1:N))
              
              DO I=1,N,1
                 WRITE(*,*) "ABSCISA X",I," : "
                 READ(*,*) X(I)
                 WRITE(*,*) "ORDENADA Y",I," : "
                 READ(*,*) Y(I)
              END DO
              
              CALL CALCULA_POLINOMIO(N,X,Y,C)
              DO I=1,N,1
                 WRITE(*,*) "C(",I,"): "
                 WRITE(*,*) C(I)
              END DO
              
              WRITE(*,*) " "
              WRITE(*,*) "INTRODUZCA PUNTO DONDE EVALUAR POLINOMIO: "
              READ(*,*) X0
              
              VALOR=EVALUAR_POLINOMIO(X0,X,C,N)
              
              WRITE(*,*) "VALOR CALCULADO: ",VALOR
              
              PAUSE
              STOP
      END PROGRAM INTERPOLACION_NEWTON
      
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
                 






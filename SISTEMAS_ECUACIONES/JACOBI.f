      PROGRAM METODO_JACOBI
              IMPLICIT NONE
              INTEGER I,J,N,IT
              REAL*8 TOL,SUMA,PROD,NORMAR,NORMAB
              REAL*8, ALLOCATABLE:: A(:,:)
              REAL*8, ALLOCATABLE:: B(:)
              REAL*8, ALLOCATABLE:: X0(:)
              REAL*8, ALLOCATABLE:: X1(:)
              REAL*8, ALLOCATABLE:: R(:)
              OPEN(UNIT=301,FILE="solucion-jacobi.dat")
              
              !RESOLUCI‡N DEL SISTEMA DE ECUACIONES POR EL MêTODO DE JACOBI

              WRITE(*,*)"DIMENSION MATRIZ COEFICIENTES: "
              READ(*,*) N
              
              ALLOCATE( A(1:N,1:N) )
              ALLOCATE( B(1:N) )
              ALLOCATE( X0(1:N) )
              ALLOCATE( X1(1:N) )
              ALLOCATE( R(1:N) )
              
              IF(N.LE.3) THEN
                DO I=1,N
                    DO J=1,N
                       WRITE(*,*)"A(",I,", ",J,"): "
                       READ(*,*) A(I,J)
                    END DO
                    WRITE(*,*)"B(",I,"): "
                    READ(*,*) B(I)
                END DO
              ELSE IF (N.GT.3) THEN
                OPEN(UNIT=201,FILE="sistema.txt")
                DO I=1,N
                   READ(201,*) A(I,:),B(I)
                END DO
              END IF
              
              DO I=1,N
                 X0(I)=0
              END DO
              
              WRITE(*,*) "TOLERANCIA: "
              READ(*,*) TOL
              IT=0
              DO I=1,N
                 X1(I)=B(I)/A(I,I)
              END DO
              IT=1
              DO WHILE(ABS(X1(1)-X0(1)).GE.TOL)
                 WRITE(301,*) IT,"      ",X1(:)
                 DO I=1,N
                    X0(I)=X1(I)
                 END DO

                 DO I=1,N
                    SUMA=0
                    DO J=1,N
                       IF(J.NE.I) THEN
                        SUMA=SUMA+A(I,J)*X0(J)
                       ELSE
                        SUMA=SUMA
                       END IF
                    END DO
                    X1(I)=(B(I)-SUMA)/A(I,I)
                 END DO
                 IT=IT+1
              END DO
              
              WRITE(*,*) "SOLUCION: "
              DO I=1,N
                 WRITE(*,*) X1(I)
              END DO
              WRITE(*,*) IT
              WRITE(301,*) IT,"       ",X1(:)

              
              !CALCULO DEL RESIDUO Y DE SU NORMA
              DO I=1,N
                  PROD=0
                  DO J=1,N
                     PROD=PROD+A(I,J)*X1(J)
                  END DO
                  R(I)=B(I)-PROD
              END DO
              
              NORMAR=0
              NORMAB=0
              DO I=1,N
                 NORMAR=NORMAR+R(I)*R(I)
                 NORMAB=NORMAB+B(I)*B(I)
              END DO
              NORMAR=NORMAR**0.5
              NORMAB=NORMAB**0.5
              
              WRITE(*,*) "NORMAR/NORMAB"
              WRITE(*,*) NORMAR/NORMAB
              
              PAUSE
              STOP
      END PROGRAM
              
      

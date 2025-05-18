      PROGRAM METODO_JACOBI
              IMPLICIT NONE
              INTEGER I,J,N,IT
              REAL*8 TOL,SUMA1,SUMA2,PROD,NORMAR,NORMAB,RESIDUO
              REAL*8, ALLOCATABLE:: A(:,:)
              REAL*8, ALLOCATABLE:: B(:)
              REAL*8, ALLOCATABLE:: X(:)
              REAL*8, ALLOCATABLE:: R(:)
              OPEN(UNIT=301,FILE="solucion-gauss-seidel.dat")

              !RESOLUCI‡N DEL SISTEMA DE ECUACIONES POR EL MêTODO DE GAUSS-SEIDEL

              WRITE(*,*)"DIMENSION MATRIZ COEFICIENTES: "
              READ(*,*) N

              ALLOCATE( A(1:N,1:N) )
              ALLOCATE( B(1:N) )
              ALLOCATE( X(1:N) )
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

              WRITE(*,*) "TOLERANCIA: "
              READ(*,*) TOL
              
              IT=0


              DO I=1,N
              SUMA1=0
              SUMA2=0
                 DO J=1,I-1
                    SUMA2=SUMA2+A(I,J)*X(J)
                 END DO
                 X(I)=(B(I)-SUMA2)/A(I,I)
              END DO
              IT=1
              RESIDUO=0
              
              DO WHILE(ABS(X(1)-RESIDUO).GE.TOL)
                 WRITE(301,*) IT,"      ",X(:)
                 RESIDUO=X(1)
                 DO I=1,N
                 SUMA1=0
                 SUMA2=0
                    DO J=I+1,N
                       SUMA1=SUMA1+A(I,J)*X(J)
                    END DO
                    DO J=1,I-1
                       SUMA2=SUMA2+A(I,J)*X(J)
                    END DO
                    X(I)=(B(I)-SUMA2-SUMA1)/A(I,I)
                 END DO
                 IT=IT+1
              END DO

              WRITE(*,*) "SOLUCION: "
              WRITE(*,*) X(:)
              WRITE(*,*) IT
              WRITE(301,*) IT,"       ",X(:)
              
              CLOSE(301)


              !CALCULO DEL RESIDUO Y DE SU NORMA
              DO I=1,N
                  PROD=0
                  DO J=1,N
                     PROD=PROD+A(I,J)*X(J)
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

      PROGRAM EJERCICIO_6
              IMPLICIT NONE
              REAL, ALLOCATABLE:: X(:)
              REAL, ALLOCATABLE:: Y(:)
              REAL SUMA,PROD_ESC
              INTEGER N,I
              WRITE(*,*) "N?"
              READ(*,*) N
              ALLOCATE( X(0:N-1) )
              ALLOCATE( Y(0:N-1) )
              I=0
              DO WHILE(I.LT.N)
                 WRITE(*,*)"X(",I,")"
                 READ(*,*)X(I)
                 WRITE(*,*)"Y(",I,")"
                 READ(*,*)Y(I)
                 I=I+1
              END DO
              SUMA=PROD_ESC(X,Y,N)
              WRITE(*,*) SUMA
              PAUSE
              STOP
      END PROGRAM
      
      REAL FUNCTION PROD_ESC(X,Y,N)
           REAL X(0:N-1),Y(0:N-1),SUMA
           INTEGER N,I
           SUMA=0.0
           DO I=0,N-1,1
              SUMA=SUMA+X(I)*Y(I)
           END DO
           PROD_ESC=SUMA
           RETURN
      END FUNCTION PROD_ESC
           

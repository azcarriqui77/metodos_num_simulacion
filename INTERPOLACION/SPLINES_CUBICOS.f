      PROGRAM SPLINES_CUBICOS
              IMPLICIT NONE
              REAL*8, ALLOCATABLE:: H(:)
              REAL*8, ALLOCATABLE:: U(:)
              REAL*8, ALLOCATABLE:: V(:)
              REAL*8, ALLOCATABLE:: Z(:)
              REAL*8, ALLOCATABLE:: X(:)
              REAL*8, ALLOCATABLE:: Y(:)
              REAL*8, ALLOCATABLE:: B(:)
              INTEGER N,I
              
              !WRITE(*,*) "N: "
              !READ(*,*) N
              
              N=3
              ALLOCATE( X(0:N) )
              ALLOCATE( Y(0:N) )
              ALLOCATE( H(0:N-1) )
              ALLOCATE( U(1:N-1) )
              ALLOCATE( V(1:N-1) )
              ALLOCATE( Z(0:N) )
              ALLOCATE( B(0:N) )
              
              X(0)=-2
              X(1)=0
              X(2)=0.1
              X(3)=2
              
              Y(0)=0
              Y(1)=0
              Y(2)=-0.798
              Y(3)=0
              
              DO I=0,N-1
                 H(I)=X(I+1)-X(I)

              END DO
              
              Z(0)=0.0
              Z(N)=0.0
              
              DO I=0,N-1
                 B(I)=6*(Y(I+1)-Y(I))/H(I)

              END DO
              
              DO I=1,N-1
                 V(I)=B(I)-B(I-1)
                 U(I)=2*(H(I)+H(I-1))

              END DO
              
              CALL GAUSS_TRID_SIM (U, H, V, Z, N)
              WRITE(*,*)'Z(I) ',(Z(I),I=0,N)
              
              PAUSE
              STOP
      END PROGRAM SPLINES_CUBICOS
      
        ! Esta subrutina resuelve un sistema de ecuaciones tridiagonal
        ! y simetrico. Los elementos de la diagonal se almacenan en el
        ! vector U, los de las diagonales superior e inferior en el
        ! vector H, y los terminos independientes en el vector V. El
        ! vector Z almacenara los resultados. El numero de ecuaciones es
        ! N-1. Se sigue la notacion de la parte de splines del tema 2.
        ! OJO! Los vectores U, V y Z quedan modificados tras la ejecucion
        ! de la subrutina. Cuidado tambien con los indices de los vectores
        ! (no todos estan definidos con los mismos limites).
      
        SUBROUTINE GAUSS_TRID_SIM (U, H, V, Z, N)
            IMPLICIT NONE
            INTEGER N, I
            REAL H(0:N-1), U(1:N-1), V(1:N-1), Z(0:N)

            ! Calculamos los nuevos elementos de matriz.
            ! Se hacen cero todos los términos bajo la
            ! diagonal principal.
            DO I=2,N-1
                U(I)=U(I)-H(I-1)*H(I-1)/U(I-1)
                V(I)=V(I)-H(I-1)*V(I-1)/U(I-1)
            END DO

            ! Con los nuevos elementos de matriz empezamos a
            ! resolver el sistema de ecuaciones en orden inverso.
            Z(N-1)=V(N-1)/U(N-1)

            DO I=N-2,1,-1
                Z(I)=( V(I)-H(I)*Z(I+1) )/U(I)
            END DO

            RETURN
        END SUBROUTINE GAUSS_TRID_SIM

              
              
              

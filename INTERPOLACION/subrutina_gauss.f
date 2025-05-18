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

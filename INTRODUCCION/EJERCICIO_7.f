      PROGRAM EJERCICIO_7
              IMPLICIT NONE
              REAL, ALLOCATABLE:: X(:,:)
              REAL, ALLOCATABLE:: Y(:,:)
              REAL, ALLOCATABLE:: Z(:,:)
              INTEGER FILSX,COLSX,FILSY,COLSY,FILSZ,COLSZ
              INTEGER I,J,K, AllocateStatusX, AllocateStatusY
              INTEGER AllocateStatusZ
              
              WRITE(*,*)"FILSX"
              READ(*,*)FILSX
              WRITE(*,*)"COLSX"
              READ(*,*)COLSX
              WRITE(*,*)"FILSY"
              READ(*,*)FILSY
              WRITE(*,*)"COLSY"
              READ(*,*)COLSY
              FILSZ=FILSX
              COLSZ=COLSY
              
              ALLOCATE( X(FILSX,COLSX) , STAT=AllocateStatusX )
              ALLOCATE( Y(FILSY,COLSY) , STAT=AllocateStatusY )
              ALLOCATE( Z(FILSZ,COLSZ) , STAT=AllocateStatusZ )

              IF(COLSX.EQ.FILSY) THEN
                  DO I=1,FILSX,1
                     DO J=1,COLSX,1
                        WRITE(*,*)"ELEMENTO X(",I,", ",J,"):"
                        READ(*,*) X(I,J)
                     END DO
                  END DO
                  DO I=1,FILSY,1
                     DO J=1,COLSY,1
                        WRITE(*,*)"ELEMENTO Y(",I,", ",J,"):"
                        READ(*,*) Y(I,J)
                     END DO
                  END DO
                  CALL PROD_MATRICES (X,Y,Z,FILSX,FILSY,COLSX,COLSY)
                  WRITE(*,*) "FILAS: ", FILSZ, "COLUMNAS: ", COLSZ
                  DO I=1,FILSZ,1
                        WRITE(*,*) Z(I,:)
                  END DO

              ELSE
                  WRITE(*,*)"NO SE PUEDEN MULTIPLICAR LAS MATRICES"
              END IF
              PAUSE
              STOP
      END PROGRAM EJERCICIO_7
      
      SUBROUTINE PROD_MATRICES(S,P,Q,A,B,C,D)
              INTEGER A,B,C,D,M,N,O
              REAL AUX
              REAL :: S(A,B)
              REAL :: P(C,D)
              REAL :: Q(A,D)
              DO M=1,A,1
                 DO N=1,D,1
                    AUX=0
                    DO O=1,A,1
                       AUX=AUX+(S(M,O)*P(O,N))
                    END DO
                    Q(M,N)=AUX
                 END DO
              END DO
              RETURN
      END SUBROUTINE PROD_MATRICES
                 


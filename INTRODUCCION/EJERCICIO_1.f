      PROGRAM EJERCICIO_1
              IMPLICIT NONE
              INTEGER I
              
              DO I=0,100,1
                 IF(MOD(I,2).EQ.0) THEN
                     WRITE(*,*)I," ES PAR."  !ESTO ES UN COMENTARIO
                 ELSE
                     WRITE(*,*)I,"ES IMPAR."
                 END IF
              END DO
              PAUSE
              STOP
      END PROGRAM
C     ESTO TAMBIêN ES UN COMENTARIO


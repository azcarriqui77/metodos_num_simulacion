      PROGRAM EJERCICIO_2
              IMPLICIT NONE
              INTEGER NUM, I
              WRITE(*,*)"NUMERO: "
              READ(*,*)NUM
              DO I=0,10,1
                 WRITE(*,*)NUM," * ",I," = ",NUM*I
              END DO
              PAUSE
              STOP
      END PROGRAM

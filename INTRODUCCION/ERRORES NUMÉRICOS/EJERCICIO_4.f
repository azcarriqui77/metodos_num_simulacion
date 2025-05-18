      PROGRAM EJERCICIO_4
              IMPLICIT NONE
              INTEGER I,N
              REAL*8 VALOR_SUC,VALOR_CALC,ACTUAL,ANTERIOR,ANTERIOR2
              REAL*8 ERROR
              OPEN(UNIT=201,FILE="calculos.dat")
              
              WRITE(*,*)"N: "
              READ(*,*) N
              
              
              ANTERIOR=1.0/3
              ANTERIOR2=1
              DO I=2,N,1
                 ACTUAL=((13.0D0*ANTERIOR)-(4.0D0*ANTERIOR2))
                 VALOR_SUC=ACTUAL
                 ANTERIOR2=ANTERIOR
                 ANTERIOR=ACTUAL
                 VALOR_CALC=(1.0/3)**I
                 ERROR=(ABS(1.0*(VALOR_CALC-VALOR_SUC)))/VALOR_CALC
                 WRITE(201,*) I,VALOR_CALC,VALOR_SUC,ERROR
                 WRITE(*,*) I,VALOR_CALC,VALOR_SUC,ERROR
              END DO
              
              CLOSE(201)
              PAUSE
              STOP
      END PROGRAM EJERCICIO_4
                 
                 
              
              
              

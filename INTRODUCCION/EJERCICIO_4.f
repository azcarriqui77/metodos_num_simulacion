      PROGRAM EJERCICIO_4
              IMPLICIT NONE
              INTEGER N,I
              REAL NUM,MAYOR,MENOR
              WRITE(*,*)"INTRODUZCA NUMERO DE TERMINOS: "
              READ(*,*)N
              WRITE(*,*)"NUMERO: "
              READ(*,*)MAYOR
              MENOR=MAYOR
              DO I=2,N,1
                 WRITE(*,*)"NUMERO: "
                 READ(*,*)NUM
                 IF(NUM.GT.MAYOR) THEN
                      MAYOR=NUM
                 ELSE IF(NUM.LT.MENOR) THEN
                      MENOR=NUM
                 END IF
              END DO
              WRITE(*,*)"MAYOR: ",MAYOR
              WRITE(*,*)"MENOR: ",MENOR
              PAUSE
              STOP
      END PROGRAM

      PROGRAM EJERCICIO_5
              IMPLICIT NONE
              INTEGER XINI,XFIN,N
              REAL X,FUNCION,Y,PASO
              WRITE(*,*)"INTRODUZCA EL VALOR INICIAL"
              READ(*,*)XINI
              WRITE(*,*)"INTRODUZCA EL VALOR FINAL"
              READ(*,*)XFIN
              WRITE(*,*)"INTRODUZCA EL VALOR DE N: "
              READ(*,*)N
              WRITE(*,*)"INTRODUZCA EL PASO ENTRE LOS VALORES: "
              READ(*,*)PASO
              OPEN(UNIT=100,FILE="tablavalores.dat")
              X=XINI
              DO WHILE(X.LE.XFIN)
                 Y=FUNCION(X,N)
                 WRITE(100,*)X,Y
                 X=X+PASO
              END DO
              CLOSE(100)
              PAUSE
              STOP
      END PROGRAM
      
      REAL FUNCTION FUNCION(X, N)
           REAL X,Y
           INTEGER N,I,FACTORIAL,FACT
           Y=0
           IF(X.LE.(-3.1415))THEN
                Y=COS(N*X)
           ELSE IF(X.LE.0) THEN
                Y=((-1)**N)+X*(X+3.1415)
           ELSE IF(X.LE.2) THEN
                FACT=1
                DO I=1,N,1
                   FACT=FACTORIAL(I)
                   Y=Y+(1.0/FACT*(X**I))
                END DO
           ELSE
                Y=1.0
                DO I=0,N,1
                   Y=Y*((I*I+1)*(X**(-I)))
                END DO
           END IF
           FUNCION=Y
           RETURN
      END FUNCTION FUNCION
      
      INTEGER FUNCTION FACTORIAL(X)
              INTEGER X,I,PROD
              PROD=1
              DO I=1,X,1
                 PROD=PROD*I
              END DO
              FACTORIAL=PROD
              RETURN
      END FUNCTION FACTORIAL

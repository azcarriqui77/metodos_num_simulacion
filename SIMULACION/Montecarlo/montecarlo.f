      PROGRAM MONTECARLO
              IMPLICIT NONE
              REAL*8 X, XANT, Y, YANT, PI, Q, ERROR
              INTEGER*8 N, dentro, I
              
              OPEN(UNIT=30, FILE="datos.dat")
              
              N=1E5
              PI=3.14159265359
              XANT=1
              YANT=3
              dentro=0
              Q=714025
              
              DO I=1,N
                 X=MOD(1366*XANT+150889, Q)
                 XANT=X
                 X=DBLE(X)/DBLE(Q)
                 
                 Y=MOD(1366*YANT+150889, Q)
                 YANT=Y
                 Y=DBLE(Y)/DBLE(Q)

                 IF((X*X+Y*Y).LT.1) THEN
                    dentro=dentro+1
                 END IF
                 
                 IF(MOD(I,10).EQ.0) THEN
                    ERROR=PI-(4.0*dentro/I)
                    WRITE(30,*) I, X, Y, 4.0*dentro/I, ERROR
                 END IF
              END DO
              
              CLOSE (30)
              
              PAUSE
              STOP
      END PROGRAM
              

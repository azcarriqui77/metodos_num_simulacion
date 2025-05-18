      PROGRAM PENDULO_FISICO_EULER_CROMER
              IMPLICIT NONE
              REAL*8 periodo, L, g, t, paso, m
              REAL*8, ALLOCATABLE:: Y(:,:)
              REAL*8 X0, Y0, D0, PI
              REAL*8 EC, EP, ET
              INTEGER N, I
              OPEN(UNIT=27, FILE="datos_EULER-CROMER.dat")


              PI=3.14159265359
              L=1
              g=9.8
              periodo=2*PI*((L/g)**0.5)
              paso=2E-4*periodo
              t=0
              m=0.1
              X0=0
              Y0=PI/4
              D0=0
              N=INT(10*periodo/paso)
              I=0

              ALLOCATE ( Y(1:2,0:N) )

              Y(1,0)=Y0
              Y(2,0)=D0
              EC=0.5*m*L*L*Y(2,0)*Y(2,0)
              EP=m*g*L*(1-COS(Y(1,0)))
              ET=EC+EP


              WRITE(27,*) t, Y(1,0), Y(2,0), EC, EP, ET


              DO I=1,N
                 t=t+paso
                 
                 Y(2,I)=Y(2,I-1)+paso*(-g/L)*SIN(Y(1,I-1))
                 Y(1,I)=Y(1,I-1)+paso*Y(2,I)

                 EC=0.5*m*L*L*Y(2,I)*Y(2,I)
                 EP=m*g*L*(1-COS(Y(1,I)))
                 ET=EC+EP

                 WRITE(27,*) t, Y(1,I), Y(2,I), EC, EP, ET

              END DO

              CLOSE(27)

              PAUSE
              STOP

      END PROGRAM


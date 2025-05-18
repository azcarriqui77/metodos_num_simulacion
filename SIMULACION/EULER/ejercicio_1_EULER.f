      PROGRAM PENDULO_MATEMATICO_EULER
              IMPLICIT NONE
              REAL*8 periodo, L, g, t, paso, m
              REAL*8 FUNCION_ANALITICA, v_ana
              REAL*8, ALLOCATABLE:: Y(:,:)
              REAL*8 X0, Y0, D0, PI
              REAL*8 EC, EP, ET
              INTEGER N, I
              OPEN(UNIT=27, FILE="datos_EULER.dat")
              OPEN(UNIT=28, FILE="diagrama_fase_EULER.dat")
              OPEN(UNIT=29, FILE="error_EULER.dat")

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
              EP=0.5*m*g*L*Y(1,0)*Y(1,0)
              ET=EC+EP
              v_ana=FUNCION_ANALITICA(t,g,L)


              WRITE(27,*) t, Y(1,0), Y(2,0), EC, EP, ET
              WRITE(28,*) Y(1,0), Y(2,0)
              WRITE(29,*) t, Y(1,0), v_ana, ABS(Y(1,0)-v_ana)

              DO I=1,N
              
                 t=t+paso
              
                 Y(1,I)=Y(1,I-1)+paso*Y(2,I-1)
                 Y(2,I)=Y(2,I-1)+paso*(-g/L)*Y(1,I-1)

                 EC=0.5*m*L*L*Y(2,I)*Y(2,I)
                 EP=0.5*m*g*L*Y(1,I)*Y(1,I)
                 ET=EC+EP
                 v_ana=FUNCION_ANALITICA(t,g,L)

                 WRITE(27,*) t, Y(1,I), Y(2,I), EC, EP, ET
                 WRITE(28,*) Y(1,I), Y(2,I)
                 WRITE(29,*) t, Y(1,I), v_ana, ABS(Y(1,I)-v_ana)
              END DO

              CLOSE(27)
              CLOSE(28)
              CLOSE(29)

              PAUSE
              STOP

      END PROGRAM

      REAL*8 FUNCTION FUNCION_ANALITICA (t,g,L)
             REAL*8 t,g,L
             REAL*8 f
             f=(3.14159265359/4)*COS(SQRT(g/L)*t)
             FUNCION_ANALITICA=f
             RETURN
      END FUNCTION FUNCION_ANALITICA


      PROGRAM EJERCICIO_2
              IMPLICIT NONE
              INTEGER I,N
              REAL*4 SUMA1GL, SUMA1LG
              REAL*8 SUMA2GL, SUMA2LG

              WRITE(*,*) "INTRODUZCA NUMERO DE TERMINOS: "
              READ(*,*) N

              SUMA1GL=0.0
              SUMA2GL=0.0
              SUMA1LG=0.0
              SUMA2LG=0.0

              DO I=1,N,1
                 SUMA1GL=SUMA1GL+1.0/I
                 SUMA2GL=SUMA2GL+1.0D0/I
              END DO

              DO I=N,1,-1
                 SUMA1LG=SUMA1LG+1.0/I
                 SUMA2LG=SUMA2LG+1.0D0/I
              END DO
              
              SUMA1GL=SUMA1GL-LOG(1.0*N)
              SUMA2GL=SUMA2GL-LOG(1.0D0*N)
              SUMA1LG=SUMA1LG-LOG(1.0*N)
              SUMA2LG=SUMA2LG-LOG(1.0D0*N)

              WRITE(*,*) "SUMA CON SIMPLE PRECISION", SUMA1GL, SUMA1LG
              WRITE(*,*) "SUMA CON DOBLE PRECISION", SUMA2GL, SUMA2LG

              PAUSE
              STOP
      END PROGRAM EJERCICIO_2

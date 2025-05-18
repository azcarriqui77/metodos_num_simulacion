      PROGRAM EJERCICIO_3
              IMPLICIT NONE
              INTEGER I,NUM_DATOS
              REAL MEDIA_ARIT,MEDIA_GEO,DESVIACION,DATO
              MEDIA_ARIT=0
              MEDIA_GEO=1
              DESVIACION=0
              WRITE(*,*)"INTRODUZCA NUMERO DE DATOS: "
              READ(*,*)NUM_DATOS
              DO I=1,NUM_DATOS,1
                 WRITE(*,*)"INTRODUZCA DATO NUMERO", I
                 READ(*,*)DATO
                 MEDIA_ARIT=MEDIA_ARIT+DATO
                 MEDIA_GEO=MEDIA_GEO*DATO
                 DESVIACION=DESVIACION+DATO*DATO
              END DO
              DESVIACION=(DESVIACION-(MEDIA_ARIT*MEDIA_ARIT)/NUM_DATOS)
              DESVIACION=DESVIACION/NUM_DATOS
              DESVIACION=DESVIACION**0.5
              MEDIA_ARIT=MEDIA_ARIT/NUM_DATOS
              MEDIA_GEO=MEDIA_GEO**(1.0/NUM_DATOS)
              WRITE(*,*)"MEDIA ARITMETICA",MEDIA_ARIT
              WRITE(*,*)"MEDIA GEOMETRICA",MEDIA_GEO
              WRITE(*,*)"DESVIACION ESTANDAR",DESVIACION
              PAUSE
              STOP
      END PROGRAM


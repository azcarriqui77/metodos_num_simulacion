# No queremos leyenda (cada grafica tiene solo una curva)
unset key
# Salida a un fichero png
set term pngcairo size 960,480
set output 'ejercicio2-3.png'
# Funcion que hemos usado
f(x)=exp(-x)

# Dos graficas, una al lado de la otra
set multiplot layout 1,2

# Primera grafica
set xlabel 'x'
set ylabel 'N'
plot 'ejercicio2-3.dat' using 1:3 with lines

# Segunda grafica
set xlabel 'x'
set ylabel 'Error'
# Queremos el eje y en escala logaritmica
set logscale y
# Y los numeros como potencias de diez
set format y '10^{%T}'
# Ojo! En el eje y vamos a hacer operaciones en vez
# de representar el fichero tal cual. Concretamente,
# vamos a coger nuestro desarrollo de Taylor (columna
# dos) y le vamos a restar la funcion f(x) evaluada en
# cada uno de los puntos (columna uno). Cuando se van
# a hacer operaciones las columnas se referencian como 
# $<numero> en vez de solo con el numero.
plot 'ejercicio2-3.dat' using 1:(abs($2-f($1))) with lines

unset multiplot

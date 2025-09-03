# Ejercicios Tecnicas de Conteo

perm <- function(n,k){
  y = factorial(n)/factorial(n-k)
  return(y)
}

comb <- function(n,k){
  y = factorial(n)/(factorial(n-k)*factorial(k))
  return(y)
}

# 2.21
recorridos <- 6
dias <- 3
recorridos_totales <- recorridos * dias; recorridos_totales

# 2.23
numeros_dado <- 6
tamaño_alfabeto <- 26
tamaño_em <- numeros_dado * tamaño_alfabeto; tamaño_em

# 2.25
estilo_zapatos <-  5
colores_zapatos <- 4
paresDeZapatos <- estilo_zapatos * colores_zapatos; paresDeZapatos

# 2.27
diseños <- 4
sis_calefaccion <- 3
opcion1 <- 2
opcion2 <- 2
opcionesComprador <- diseños * sis_calefaccion * opcion1 * opcion2; opcionesComprador

# 2.29
autos <- 3
marcas_autos <- 5
lugares_prueba <- 7
pilotos <- 2
cantidad_pruebas <- autos * marcas_autos * lugares_prueba * pilotos; cantidad_pruebas

# 2.31
# La placa es RLH 5--
posiblesNumeros <- 9
espacios <- 2
posibilidadesTotales <- perm(posiblesNumeros, espacios);posibilidadesTotales

# 2.33

# a)
cantidad_preguntas <- 5
opciones_respuestas <- 4
posibles_respuestas <- opciones_respuestas ** cantidad_preguntas; posibles_respuestas

# b)
opciones_incorrectas <- 3
posibles_respuestas_incorrectas <- opciones_incorrectas ** cantidad_preguntas; posibles_respuestas_incorrectas

# 2.35
casas <- 9
lotes <- 9
posibles_ubicaciones <- perm(casas,lotes); posibles_ubicaciones # Es lo mismo que hacer 9!

# 2.37
ninas <- 5
ninos <- 4
formas_ubicarse <- perm(ninas, 5) * perm(ninos, 4); formas_ubicarse

# 2.39

# a) 
ninosConcurso <- 5+3
eight_finalistas <- perm(ninosConcurso, 8); eight_finalistas

# b)
finalistas <- 3
primerosLugares <- perm(ninosConcurso,finalistas); primerosLugares

# 2.41
comb(6,4) * perm(4,4)

# 2.43
arboles <- 5
circulo_posiciones <- factorial(arboles-1);circulo_posiciones

# 2.45
letras <- 8
permutaciones <- perm(letras,8)/(factorial(3)*2); permutaciones

# 2.47
vacantes <- 3
candidatos <- 8
formas_seleccion <- comb(candidatos, vacantes); formas_seleccion


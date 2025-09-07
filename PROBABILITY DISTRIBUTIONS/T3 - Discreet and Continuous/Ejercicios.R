
"3.9 La proporción de personas que responden a cierta
encuesta enviada por correo es una variable aleatoria
continua X que tiene la siguiente función de densidad:"

"           {  (2( x + 2))/5 , 0< x < 1,
      f(x)= {
            {      0         , en otro caso."

"
  a) Demuestre que P(0 < X < 1) = 1. 
  b) Calcule la probabilidad de que más de 1/4 pero 
  menos de 1/2 de las personas contactadas respondan 
  a este tipo de encuesta."


f.x <- function(x) {
  ifelse(0 < x, ifelse(x<1,(x+2)*2/5,0),0)
}

sol9a <- integrate(f.x, lower = 0, upper = 1)$value; sol9a
cat("El area bajo la curva de la función entre 0 y 1 es:", sol9a)
x <- seq(0,1,by = 0.001)
y <- sapply(x,f.x)

df <- data.frame(x,y)
sol9b <- integrate(f.x, lower = 1/4, upper = 1/2)$value;sol9b

df_fill <- subset(df, x >= 1/4 & x <= 1/2)

library(ggplot2)

ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "aquamarine3", size = 1) +
  geom_area(data = df_fill, aes(x = x, y = y), fill = "magenta2", alpha = 0.4) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.1)) +  # evitar espacio extra
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$y)*1.1)) +  # más pegado al eje
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # centered title, sz and bold
    axis.title = element_text(face = "bold"), # title ejes bold
    axis.text = element_text(color = "black"), # col text
    axis.line = element_line(color = "black") # mostrar líneas de ejes
  ) +
  labs(
    title = "Función de Densidad", x = "x - Proporción personas contactadas", 
    y = "f(x) - Probabilidad Respuesta a Encuesta"
  ) +
  # Flechas simulando los ejes cartesianos
  annotate("segment", x = 0, xend = 1.1, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = max(df$y) * 1.1,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8)

cat("La probabilidad de que más de 1/4 pero menos de 1/2 de las personas 
contactadas respondan a este tipo de encuesta es:", sol9b)

"3.15 Calcule la función de distribución acumulativa
de la variable aleatoria X que represente el número de
unidades defectuosas en el ejercicio 3.11. Luego, utilice
F(x) para calcular:

  a) P(X = 1);
  b) P(0 < X ≤ 2)."


"3.21 Considere la función de densidad

        {  k√x, 0< x < 1
f (x) = {
        {  0, en otro caso
        
a) Evalúe k
b) Calcule F(x) y utilice el resultado para evaluar P(0.3 < X < 0.6)."

#Integral de 0 a 1 de k√x = 1 
# k * integral((x)^1/2)
# k * 2/3 * x^(3/2) | evaluado de 0 a 1
# k * 2/3 * 1 = 1
# k * 2/3 = 1
# k = 3/2
k_21 <- 3/2
cat("Para que f(x) sea una función de densidad. El valor de k es: ", k_21)
f.x.21 <- function(x){
  ifelse(0<x, ifelse(x<1, 1.5*sqrt(x),0),0)
}
sol21b <- integrate(f.x.21, lower = 0.3, upper = 0.6)$value

x.21 <- seq(0,1, by=0.001)
y.21 <- sapply(x.21,f.x.21)
df3 <- data.frame(x.21, y.21)
df3_fill <- subset(df3, x.21 >= 0.3 & x.21 <= 0.6)

ggplot(df3, aes(x = x.21, y = y.21)) +
  geom_line(color = "maroon", size = 1) +
  geom_area(data = df3_fill, aes(x = x.21, y = y.21), fill = "mediumpurple", alpha = 0.4) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.1)) +  # evitar espacio extra
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df3$y.21)*1.1)) +  # más pegado al eje
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # centered title, sz and bold
    axis.title = element_text(face = "bold"), # title ejes bold
    axis.text = element_text(color = "black"), # col text
    axis.line = element_line(color = "black") # mostrar líneas de ejes
  ) +
  labs(
    title = "Función de Densidad", x = "x", 
    y = "f(x)"
  ) +
  # Flechas simulando los ejes cartesianos
  annotate("segment", x = 0, xend = 1.1, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = max(df3$y.21) * 1.1,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8)

cat("La probabilidad P(0.3 < X < 0.6) para esta función, la cuál representa 
el área sombreada de la gráfica, es:", sol21b)

"3.27 El tiempo que pasa, en horas, antes de que una
parte importante de un equipo electrónico que se utiliza
para fabricar un reproductor de DVD empiece a fallar
tiene la siguiente función de densidad:

         { (1/2000) exp(−x/ 2000),  x>= 0
f (x) =  {
         {         0             ,  x < 0

  a) Calcule F(x).
  b) Determine la probabilidad de que el componente (y, por lo tanto, 
  el reproductor de DVD) funcione durante más de 1000 horas antes de 
  que sea necesario reemplazar el componente.
  c) Determine la probabilidad de que el componente falle antes de 2000 horas."

f.x.27 <- function(x){
  ifelse(x > 0, (1/2000)* exp(-x/2000), 0)
}
F.x.27 <- integrate(f.x.27, lower = 0, upper = Inf)$value; F.x.27
#F(x)= 1 - e^(-x/2000)
cat("F(x):", F.x.27)
x.27 <- seq(0,3200, by = 0.1)
y.27 <- sapply(x.27, f.x.27)
plot(x.27,y.27,type="h")

sol27b <- integrate(f.x.27, lower = 1000, upper = Inf)$value
cat("Probabilidad de que el componente funcione más de 1000 horas", sol27b)
df2 <- data.frame(x.27,y.27)

df2_fill <- subset(df2, x.27 >= 1000 & x.27 <= 3200)

ggplot(df2, aes(x = x.27, y = y.27)) +
  geom_line(color = "maroon", size = 1) +
  geom_area(data = df2_fill, aes(x = x.27, y = y.27), fill = "mediumpurple", alpha = 0.4) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3200), breaks = seq(0, 3200, by = 500)) +  # evitar espacio extra
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df2$y.27)*1.1)) +  # más pegado al eje
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # centered title, sz and bold
    axis.title = element_text(face = "bold"), # title ejes bold
    axis.text = element_text(color = "black"), # col text
    axis.line = element_line(color = "black") # mostrar líneas de ejes
  ) +
  labs(
    title = "Función de Densidad", x = "x - Horas", 
    y = "f(x) - Probabilidad de Funcionamiento"
  ) +
  # Flechas simulando los ejes cartesianos
  annotate("segment", x = 0, xend = 3200, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = max(df2$y) * 1.1,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8)

sol27c <- integrate(f.x.27, lower = 0, upper = 2000)$value
cat("Probabilidad de que el componente falle antes de cumplir 2000 horas", sol27c)

"3.33 Suponga que cierto tipo de pequeñas empresas de procesamiento de 
datos están tan especializadas que algunas tienen dificultades para 
obtener utilidades durante su primer año de operación. La función de 
densidad de probabilidad que caracteriza la proporción Y que obtiene 
utilidades está dada por:

         { (ky^4)(1 −y)^3, 0≤ y ≤ 1
f (y) =  {
         {       0       , en otro caso

  a) ¿Cuál es el valor de k que hace de la anterior una
  función de densidad válida?
  b) Calcule la probabilidad de que al menos 50% de
  las empresas tenga utilidades durante el primer año.
  c) Calcule la probabilidad de que al menos 80% de las
  empresas tenga utilidades durante el primer año."

# integral de 0 a 1 de (ky^4)(1-y)^3 = 1 . Para que pueda ser una función de densidad
# k * integral((y^4)(1-y)(1-y)^2)
# k * integral de 0 a 1(-y^7 + 3y^6 - 3y^5 + y^4)
# k * (-1/8 + 3/7 - 1/2 + 1/5) = 1
# k * 1/280 = 1 
# k = 280
k.33 <- 280
cat("El valor de k que hace que f(x) sea una función de densidad válida es:", k.33)
f.x.33 <- function(x){
  ifelse(0<x,ifelse(x<1,280*(x**4)*((1-x)**3),0),0)
}
sol33b <- integrate(f.x, lower = 0.5, upper = 1)$value
cat("La probabilidad de que al menos el 50% de las empresas tenga utilidades 
durante el primer año es:", sol33b)

x.33 <- seq(0,1,by=0.001)
y.33 <- sapply(x.33, f.x.33)
df5 <- data.frame(x.33,y.33)

df5_fill <- subset(df5, x.33 >= 0.5 & x.33 <= 1)

ggplot(df5, aes(x = x.33, y = y.33)) +
  geom_line(color = "#548B54", size = 1) +
  geom_area(data = df5_fill, aes(x = x.33, y = y.33), fill = "steelblue3", alpha = 0.6) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.1)) +  # evitar espacio extra
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df5$y.33)*1.1)) +  # más pegado al eje
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # centered title, sz and bold
    axis.title = element_text(face = "bold"), # title ejes bold
    axis.text = element_text(color = "black"), # col text
    axis.line = element_line(color = "black") # mostrar líneas de ejes
  ) +
  labs(
    title = "Función de Densidad", x = "Proporción de empresas", 
    y = "Probabilidad"
  ) +
  # Flechas simulando los ejes cartesianos
  annotate("segment", x = 0, xend = 1.1, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = max(df5$y) * 1.1,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8)

sol33c <- integrate(f.x, lower = 0.8, upper = 1)$value
cat("La probabilidad de que al menos el 50% de las empresas tenga utilidades 
durante el primer año es:", sol33c)

df5_fillc <- subset(df5, x.33 >= 0.8 & x.33 <= 1)

ggplot(df5, aes(x = x.33, y = y.33)) +
  geom_line(color = "#8B3626", size = 1) +
  geom_area(data = df5_fillc, aes(x = x.33, y = y.33), fill = "turquoise4", alpha = 0.6) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1.1), breaks = seq(0, 1.1, by = 0.1)) +  # evitar espacio extra
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(df5$y.33)*1.1)) +  # más pegado al eje
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), # centered title, sz and bold
    axis.title = element_text(face = "bold"), # title ejes bold
    axis.text = element_text(color = "black"), # col text
    axis.line = element_line(color = "black") # mostrar líneas de ejes
  ) +
  labs(
    title = "Función de Densidad", x = "Proporción de empresas", 
    y = "Probabilidad"
  ) +
  # Flechas simulando los ejes cartesianos
  annotate("segment", x = 0, xend = 1.1, y = 0, yend = 0,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8) +
  annotate("segment", x = 0, xend = 0, y = 0, yend = max(df5$y) * 1.1,
           arrow = arrow(type = "closed", length = unit(0.3, "cm")),
           color = "black", size = 0.8)

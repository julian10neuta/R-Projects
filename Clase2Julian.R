
library("dplyr")
library("descriptr")
library("ggplot2")
library("datos")

## Datos cualitativos: Tortas y barras

## Datos cuantitativos discretos: Tortas y barras
## Diagrama de barras

barplot()
?datos
data()
datos2 <- encuesta
View(encuesta)
set.seed(2548) ## Estable un estandar para todos
datos.m <- sample_n(tbl = datos2, size=200, replace = FALSE) ## Genera una muestra aleatoria de tamaño 200
attach(datos.m)
tfre1 <- table(datos.m$raza)
tfre1.df <- as.data.frame(tfre1)
colnames(tfre1.df) <- c("Raza", "Frecuencia")
barplot(tfre1, col=c("green3","pink","skyblue"), border = TRUE, density = 50, horiz = FALSE, main = "Frecuencia por raza")

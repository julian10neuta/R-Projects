
## Taller Distribuciones y Prueba de Hipotesis


#1. Recíen nacidos
pnorm(45,50,2, lower.tail = TRUE)  # Probabilidad de que mida menos de 45 cm el recien nacido

pnorm(52,50,2, lower.tail = FALSE) # Probabilidad de que mida a lo menos 52 cm

ninos <- (pnorm(52,50,2) - pnorm(48,50,2)) * 100  # Porcentaje de recien nacidos que tienen talla entre 48 y 52

floor(200 * ninos/100) # Cantidad de niños de talla entre 48 y 52 cm


#2. Temperatura en Lima
temperture <- pnorm(42, 36, 3) - pnorm(39, 36, 3); temperture
round(30 * temperture, 1) # Cantidad de días que tienen una temperatura entre 39 y 42


#3. Intervalo de confianza para la venta media por trabajador
install.packages("BSDA")
library(BSDA) 
tsum.test(mean.x = 5, s.x = sqrt(2), n.x = 15, conf.level = 0.9)

#4. Intervalo de confianza de la demanda diaria de un producto
demandaProducto <- c(35,44,38,55,33,56,67,45,48,40,43)
demandaMedia <- mean(demandaProducto)
demandaDesviacion <- sd(demandaProducto)
tsum.test(mean.x = demandaMedia, s.x = demandaDesviacion, n.x = 11, conf.level = 0.95)

#5. Hipotesis nula de la media de las deudas de una empresa
tsum.test(mean.x = 188000, s.x = 81000, n.x = 50, mu = 160000, alternative = "two.sided", conf.level = 0.95)
# En este caso se rechaza la hipotesis nula H0 ya que el valor de p = 0,018 < 0,05. La media es significativamente diferente a 160.000
# Hipotesis nula H0: la media de las deudas es 160000.

#6. Hipotesis nula vida media de un componente electronico
zsum.test(mean.x = 1450, sigma.x = sqrt(650), n.x = 900, mu = 1500, alternative = "greater", conf.level = 0.95)
# No existe suficiente evidencia para rechazar H0 ya que p > 0.05, lo que nos dice que no podemos establecer que H1 se correcta
# No se puede afirmar que la vida media de un c. electronico es mayor a 1500 horas
# Por lo tanto no se rechaza la hipotesis nula H0 de que la vida media de un comoponente sea menor a 1500.

#7. Analizar la normalidad de los datos de Pesos de perosnas por 
library(tidyverse)
Peso_hombre <- c(87.3, 80.0, 82.3, 73.6, 74.1, 85.9, 73.2, 76.3, 65.9, 90.9, 89.1, 62.3, 82.7, 79.1, 98.2, 84.1, 83.2, 83.2)
Peso_mujer <- c(51.6, 59.0, 49.2, 63.0, 53.6, 59.0, 47.6, 69.8, 66.8, 75.2, 55.2, 54.2, 62.5, 42.0, 50.0, 49.8, 49.2, 73.2)

Pesos_df <- data.frame(PesoHombre = Peso_hombre, PesoMujer = Peso_mujer)
BoxplotPesos <- boxplot(as.matrix(Pesos_df), col = rainbow(2), xlab="Sexo", ylab="Peso", main = "Boxplot de Pesos por sexo")
Histograma_Hombre <- hist(Peso_hombre, col = "skyblue2", density = 30, main = "Histograma de los Pesos de los Hombres", xlab = "Peso", ylab = "Frecuancia")
Histograma_Mujer <- hist(Peso_mujer, col = "#DDA0DD", density = 40, main = "Histograma de los Pesos de las Mujeres", xlab = "Peso", ylab = "Frecuancia")
# Normalidad de Pesos Hombre
qqnorm(Peso_hombre)
qqline(Peso_hombre, col = "#6A5ACD")
library(car)
qqPlot(Peso_hombre, envelope = 0.90, col = "brown1", main = "QQplot de Pesos Hombre")
shapiro.test(Peso_hombre) # Test de Shapiro
library(nortest)
lillie.test(Peso_hombre) # Test de Kolmogorov-Smirnov
# Normalidad de Pesos Mujer
qqnorm(Peso_mujer)
qqline(Peso_mujer, col = "#D02090")
qqPlot(Peso_mujer, envelope = 0.90, col = "brown1", main = "QQplot de Pesos Mujer")
shapiro.test(Peso_mujer)
lillie.test(Peso_mujer) # Test de Kolmogorov-Smirnov
# Comparación entre ambas muestras con qqplot
qqplot(Peso_hombre, Peso_mujer, main = "Comparación entre Pesos Hombre y Pesos Mujer", xlab = "Pesos de Hombres", ylab = "Pesos de Mujeres")
abline(0, 1, col = "blue")
# Graficas de curvas de densidad
plot(density(Peso_hombre), xlim = c(30, 110), col = "blue2", main = "Gráfica Curva de densidad de Pesos")
lines(density(Peso_mujer), col = "orange2")

legend("topright", c("Peso de Hombres", "Peso de Mujeres"),
       lty = 1, col = c("blue2", "orange2"))

#8. Analisis de Normalidad y Homocedasticidad de Medicamento para Parasitos de peces criados en acuicultura
gcontrol <- c(50, 65, 72, 46, 38, 29, 70, 85, 72, 40, 57, 59)
g25mg <- c(49, 47, 30, 602, 62, 60, 19, 28, 56, 62, 55, 40)
g50mg <- c(20, 59, 64, 61, 28, 47, 29, 41, 60, 57, 61, 38)
g100mg <- c(20, 23, 38, 31, 27, 16, 27, 18, 22, 12, 24, 11)
g125mg <- c(18, 30, 22, 26, 31, 11, 15, 12, 31, 36, 16, 13)

Medicamentos_df <- data.frame(gcontrol, g25mg, g50mg, g100mg, g125mg)
boxplot(as.matrix(Medicamentos_df), col = rainbow(5), xlab="Grupos", ylab="Parasitos", main = "Boxplot de Grupos de Peces")
shapiro.test(gcontrol)
shapiro.test(g25mg)
shapiro.test(g50mg)
shapiro.test(g100mg)
shapiro.test(g125mg)
MedicamentosNormalidadP <- data.frame(Grupo = c("Control", "25mg", "50mg", "100mg", "125mg"), 
                                      Pvalue = c(0.9486, 4.136e-06, 0.08465, 0.9607, 0.2147)); MedicamentosNormalidadP
# Elimino el valor atípico
g25mg <- c(49, 47, 30, 49, 62, 60, 19, 28, 56, 62, 55, 40)
Medicamentos_df <- data.frame(gcontrol, g25mg, g50mg, g100mg, g125mg)
shapiro.test(g25mg)
MedicamentosNormalidadP <- data.frame(Grupo = c("Control", "25mg", "50mg", "100mg", "125mg"), 
                                      Pvalue = c(0.9486, 0.1908, 0.08465, 0.9607, 0.2147)); MedicamentosNormalidadP
boxplot(as.matrix(Medicamentos_df), col = rainbow(5), xlab="Grupos", ylab="Parasitos", main = "Boxplot de Grupos de Peces")
#Homocedasticidad
Parasitos <- c(gcontrol, g25mg, g50mg, g100mg, g125mg)
Grupos <- factor(rep(c("Control", "25mg", "50mg", "100mg", "125mg"), 
                     times = c(length(gcontrol), length(g25mg), length(g50mg), length(g100mg), length(g125mg))))
bartlett.test(Parasitos ~ Grupos)
#Tabla Anova y gráfico de intervalos
AnovaTable <- aov(lm(Parasitos ~ Grupos)); summary(AnovaTable)
intervalos <- TukeyHSD(AnovaTable)
par(mar=c(5, 8, 4, 2))
plot(intervalos, las =2)

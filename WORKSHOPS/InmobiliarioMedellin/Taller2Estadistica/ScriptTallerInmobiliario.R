library(dplyr)
library(ggplot2)

BaseInmobiliario <- read.csv("Apartamentos_Medellin.csv")

#Exploración de Datos
View(BaseInmobiliario)
nrow(BaseInmobiliario)
ncol(BaseInmobiliario)
head(BaseInmobiliario, 20)
tail(BaseInmobiliario)
summary(BaseInmobiliario)
str(BaseInmobiliario)
names(BaseInmobiliario)

# ¿Hay campos vacíos?
sum(is.na(BaseInmobiliario))
colSums(is.na(BaseInmobiliario))

#Analisis de Variable precio
mean(BaseInmobiliario$precio)
median(BaseInmobiliario$precio)
max(BaseInmobiliario$precio)
min(BaseInmobiliario$precio)
range(BaseInmobiliario$precio)
sd(BaseInmobiliario$precio)
var(BaseInmobiliario$precio)
quantile(BaseInmobiliario$precio, probs = c(0.25, 0.5, 0.75))
summary(BaseInmobiliario$precio)

par(mar = c(6, 4, 4, 2)) # Ajuste de margen para histograma
histograma <- hist(BaseInmobiliario$precio, breaks = 40, ylab = "Cantidad de Apartamentos", xlab = "Precio en Millones $" , 
     main= "Análisis de Precios de Apartamentos en Medellín", col = "purple2", density = 30, xaxt = "n")
axis(side = 1, at = seq(0, max(BaseInmobiliario$precio), by = 200))


BoxplotPrecio <- boxplot(BaseInmobiliario$precio, col="purple1", border = "#8B2500", ylab="Precio", main = "Gráfico de Caja", horizontal = TRUE, xaxt = "n")
axis(side = 1, at = seq(0, max(BaseInmobiliario$precio), by = 200))

par(mar = c(8, 4, 4, 2)) # Aumenta el espacio de abajo de la margen con el 8
BoxplotUbicacionPrecio <- boxplot(precio ~ ubicacion, data = BaseInmobiliario, main = "Distribución del precio por ubicación", 
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray", "lightsalmon", "hotpink3"), las =2,
        ylab="Precio en Millones $", xlab = "")
mtext("Ubicación", side = 1, line = 6)

#Analisis de Variable precio vs mt2

## Gráfico de disperción
DispercionAreaPrecio <- plot(BaseInmobiliario$mt2,BaseInmobiliario$precio, ylab = "Precio en Millones $", xlab = "Área en Mts^2", 
     main="Análisis de Area en m^2 vs Precio", col = "blue4")

cor(BaseInmobiliario$mt2,BaseInmobiliario$precio) # Correlación
cov(BaseInmobiliario$mt2,BaseInmobiliario$precio) # Covarianza

## Grafico de disperción con color de ubicación

DisspercionPorUbicacion <- ggplot(BaseInmobiliario, aes(x = mt2, y=precio, color = ubicacion)) + geom_point(size = 2, alpha = 0.7) +
  labs(title = "Precio vs Area en m^2 por Ubicación",
      x = "Metros cuadrados",
      y = "Precio en millones",
      color = "Ubicación")
## Tabla de contingencia
cat("Tabla de Contingencia de Ubicación vs Estrato\n")
EstratoUbicacionTable <- table(BaseInmobiliario$ubicacion, BaseInmobiliario$estrato)

AptosPobladoEstrato4 <- BaseInmobiliario %>%
  filter(ubicacion == "poblado", estrato == 4) %>% nrow()
cat("La cantidad de apartamentos del poblado de estrato 4 es:", AptosPobladoEstrato4)

## Probabilidad de que un apartamento tenga parqueadero
cat("Probabilidad de que un apartamento tenga parqueadero: ", sum(BaseInmobiliario$parqueadero == "si"), "/", nrow(BaseInmobiliario))
cat("Probabilidad en porcentaje:", round(sum(BaseInmobiliario$parqueadero == "si") / nrow(BaseInmobiliario) * 100, 3), "%")

## Probabilidad de que el precio de un apartamento estrato 3 sea mayor a 100 millones
BaseEstrato3 <- BaseInmobiliario %>%
  filter(estrato == 3)
cat("Probabilidad de que el precio de un apartamento estrato 3 sea mayor a 100 millones:", sum(BaseEstrato3$precio > 100), "/", nrow(BaseEstrato3))
cat("Probabilidad de que el precio de un apartamento estrato 3 sea mayor a 100 millones:", round(sum(BaseEstrato3$precio > 100) / nrow(BaseEstrato3) * 100, 3), "%")

## Probabilidad de que una apartamento ubicado en norte tenga más de 3 alcobas
BaseNorte <- BaseInmobiliario %>%
  filter(ubicacion == "norte")
cat("Probabilidad de que una apartamento ubicado en norte tenga más de 3 alcobas:", sum(BaseNorte$alcobas > 3), "/", nrow(BaseNorte))
cat("Probabilidad de que una apartamento ubicado en norte tenga más de 3 alcobas:", round(sum(BaseNorte$alcobas > 3) / nrow(BaseNorte) * 100, 3), "%")

## Probabilidad de que un apartamento con balcón tenga más de 2 baños
BaseConBalcon <- BaseInmobiliario %>%
  filter(balcon == "si")
cat("Probabilidad de que un apartamento con balcón tenga más de 2 baños:", sum(BaseConBalcon$banos > 2), "/", nrow(BaseConBalcon))
cat("Probabilidad de que un apartamento con balcón tenga más de 2 baños:", round(sum(BaseConBalcon$banos > 2) / nrow(BaseConBalcon) * 100, 3), "%")

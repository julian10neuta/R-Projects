A <- 19 + 17
A

# Vectores
vectorA <- c(1,2,5) 
vectorB <- c('Futbol','Tenis','Volleyball')


# Operaciones básicas
B <- 2^32
log(10) # In
log(100, base = 2) # Log en otra basew
e <- exp(1) # Exponenciales

# Dataframes
df1 <- data.frame(vectorB, vectorA)
colnames(df1) <- c('Deportes','Inscritos')
View(df1)

# Operaciones con vectores
sqrt(c(1,4,9,16,25,36))
2*vectorA # No se pueden multiplicar vectores != a numeros
round(sqrt(c(70,80)), digits = 2)

# Textos
cat("Este es un ejemplo de como escribir en un doc sin comillas")

# Secuencias
sec1 <- 1:10; sec1
sec2 <- round(seq(from=0.01, top=9.98, length.out=50), 2); sec2
sec3 <- round(seq(from=1, top=5, by=10), 2); sec3

# Base de datos
library('MASS')
data()
View(Titanic)
View(Animals)
help(data)
library('datos')
datos1 <- diamantes
View(datos1)
summary(datos1)

# Histograma
hist(datos1$precio)
histograma <- hist(x = datos1$precio, freq = T, density = 80, angle = 45, col = "purple",
    border = "blue", main = "Histograma del precio", xlab = "Precio")
text(x = histograma$mids[1:7], y = histograma$counts[1:7] / 2 - 1, labels = histograma$counts[1:7], 
    srt = 90, col = "azure", font = 2)

install.packages("tidyverse")
install.packages("shiny")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("tinytex")
install.packages("readxl")

library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
#Importar base en R, teniendo en cuenta la ubicación de nuestro archivo
BaseConcesionario = read_excel("Workshops/CONCESIONARIO/TallerExploratorio.xlsx")

#Exploración de datos
BaseConcesionario = BaseConcesionario[-c(1,2),]

names(BaseConcesionario)
head(BaseConcesionario)
tail(BaseConcesionario)
str(BaseConcesionario)
summary(BaseConcesionario)
sum(is.na(BaseConcesionario))
colSums(is.na(BaseConcesionario))

#Arreglo de datos
BaseConcesionario$EDAD = as.integer(BaseConcesionario$EDAD)
BaseConcesionario$`NUMERO DE HIJOS` = as.integer(BaseConcesionario$`NUMERO DE HIJOS`)
BaseConcesionario$ESTATURA = as.numeric(BaseConcesionario$ESTATURA)
BaseConcesionario = BaseConcesionario %>% 
rename(
  Nhijos = `NUMERO DE HIJOS`,
  Marca = `MARCA DE AUTO`,
  Escolaridad = `NIVEL ESCOLAR`)

BaseConcesionario$`Marca` = replace(BaseConcesionario$`Marca`, BaseConcesionario$`Marca` == "FOR", "FORD")
BaseConcesionario$`Marca` = replace(BaseConcesionario$`Marca`, BaseConcesionario$`Marca` == "BWM", "BMW")
BaseConcesionario$`Marca` = replace(BaseConcesionario$`Marca`, BaseConcesionario$`Marca` == "renault", "RENAULT")

BaseConcesionario$EDAD[28] <- 20
BaseConcesionario$Escolaridad = replace(BaseConcesionario$Escolaridad, BaseConcesionario$Escolaridad == "NA", NA)
BaseConcesionario$Marca = replace(BaseConcesionario$Marca, BaseConcesionario$Marca == "NA", NA)

BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "f", "F")
BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "MUJER", "F")
BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "mujer", "F")
BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "m", "M")
BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "hombre", "M")
BaseConcesionario$SEXO = replace(BaseConcesionario$SEXO, BaseConcesionario$SEXO == "HOMBRE", "M")

BaseConcesionario$Escolaridad = replace(BaseConcesionario$Escolaridad, BaseConcesionario$Escolaridad == "PhD", "DOCTORADO")

## Analisis variable Marca
sum(is.na(BaseConcesionario$Marca))
tfrecuencia1 <- table(na.omit(BaseConcesionario$Marca))
tfrecuencia1
tfrecuencia2 <- prop.table(tfrecuencia1) *100
tfrecuencia2

tfrecuencia1_df <- as.data.frame(tfrecuencia1)
colnames(tfrecuencia1_df) <- c("Marca", "FrecuenciaAbs")
tfrecuencia1_df

Grafico_de_Barras <- ggplot(tfrecuencia1_df, aes(x = Marca, y = FrecuenciaAbs)) + geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Gráfico de Barras de Frecuencia")

BaseConcesionario %>% select(BaseConcesionario$Marca) %>% sum(BaseConcesionario$Marca) %>% group_by(BaseConcesionario$Marca)

tfrecuencia2_df <- as.data.frame(tfrecuencia2)
colnames(tfrecuencia2_df) <- c("Marca", "FrecuenciaRel")
tfrecuencia2_df$Porcentaje <- paste0(round(tfrecuencia2_df$FrecuenciaRel, 1), "%")
tfrecuencia2_df

Grafica_de_torta <- ggplot(tfrecuencia2_df, aes(x = "", y = FrecuenciaRel, fill = Marca)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # Convierte el gráfico en un gráfico circular
  labs(title = "Gráfico de Torta de Frecuencia Relativa") + 
  geom_text(aes(label = Porcentaje), position = position_stack(vjust = 0.5))

Grafico_de_Barras
Grafica_de_torta

##Analisis de variable EDAD
BaseConcesionario_EDAD <- BaseConcesionario %>%
  filter(EDAD > 10)

Histograma1 <- hist(BaseConcesionario$EDAD, right = FALSE)

Histograma2 <- ggplot(BaseConcesionario_EDAD, aes(x = EDAD)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) + 
  labs(title = "Histograma de Edad", x = "Edad", y = "Frecuencia")

Histograma1
Histograma2

##Analisis de variable ESTATURA
BaseConcesionario_ESTATURA <- BaseConcesionario %>%
  filter(ESTATURA < 2.5)

Poligono_de_Frecuencia <- ggplot(BaseConcesionario_ESTATURA, aes(x = ESTATURA)) +
  geom_freqpoly(color = "brown", size = 1) +
  labs(title = "Polígono de Frecuencias de Estatura", x = "Estatura", y = "Frecuencia") +
  geom_histogram(fill = "white", color = "black", alpha = 0.4)

#Ojiva
h <- hist(BaseConcesionario_ESTATURA$ESTATURA, right = FALSE)
plot(cumsum(h$counts), col = "blue", xlab = "Estatura", ylab = "Frecuencia Acumulada", main = "Ojiva de Estatura")
lines(cumsum(h$counts))

Poligono_de_Frecuencia


##Analisis de variable Nhijos
BaseNhijos <- BaseConcesionario %>%
  filter(Nhijos < 20)

tfrecuencia3 <- table(na.omit(BaseNhijos$Nhijos))
tfrecuencia3_df <- as.data.frame(tfrecuencia3)
colnames(tfrecuencia3_df) <- c("Nhijos", "FrecuenciaAbs")

Grafico_de_Barras_Hijos <- ggplot(tfrecuencia3_df, aes(x = Nhijos, y = FrecuenciaAbs, fill = as.factor(Nhijos))) + 
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Gráfico de Barras de Frecuencia de Hijos", x = "Número de Hijos", y = "Frecuencia")

Grafico_de_Barras_Hijos

##Analisis de variaable Sexo
tfrecuencia4 <- table(na.omit(BaseConcesionario$SEXO))
tfrecuencia4_df <- as.data.frame(tfrecuencia4)
colnames(tfrecuencia4_df) <- c("Sexo", "FrecuenciaAbs")
tfrecuencia4_df

##Preguntas
#¿Cuantos clientes tienen mascota?
sum(BaseConcesionario$MASCOTA == "SI")
#¿Cuantos clientes mayores de 25 años tienen maestria?
A <- BaseConcesionario %>% filter(EDAD > 25) %>% filter(Escolaridad == "MAESTRÍA")
sum(!is.na(A$Escolaridad))
#¿Cuantos clientes con doctorado ganan mas de 2 millones de pesos?
B <- BaseConcesionario %>% filter(Escolaridad == "DOCTORADO") %>% filter(SALARIO > 2000000)
sum(!is.na(B$Escolaridad))
#El salario promedio por categoría de marca auto es el siguiente
C <- BaseConcesionario %>% filter(!is.na(Marca)) %>% group_by(Marca) %>% 
  summarize(SalarioPromedio = mean(SALARIO, na.rm = TRUE))
C

##
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
baseEstadistica <- read_excel("D:/Documentos/RStudio Files/RCodesUnal/PARCIAL1/Base_Estadistica_Descriptiva.xlsx", skip = 1)  

## primer punto ##

View(baseEstadistica)
tb_ed <- prop.table(table(baseEstadistica$NivelEducativo)); tb_ed
tb_df <- as.data.frame(tb_ed);
colnames(tb_df) <- c("NivelEducativo", "Proporcion"); tb_df

barplotNivelEducativo <- ggplot(tb_df, aes(x = NivelEducativo, y = Proporcion)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  labs(title = "Proporción de estudiantes por cada nivel educativo",
       x = "Nivel Educativo",
       y = "Proporción") +
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal()

## segundo punto ##
Gn <- prop.table(table(baseEstadistica$Genero)) * 100; Gn
tb2_df <- as.data.frame(Gn); 
colnames(tb2_df) <- c("Genero", "Frecuencia")
pieplot_ <- ggplot(tb2_df, aes(x = "", y = Frecuencia, fill = Genero)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución por género") +
  theme_void() +
  geom_text(aes(label = paste0(Frecuencia, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel2")


## tercer punto ##

med_edad <- mean(baseEstadistica$Edad, na.rm = TRUE) #Se utiliza
med_edad

mediana_edad <- median(baseEstadistica$Edad, na.rm = TRUE)
mediana_edad

desv_edad <- sd(baseEstadistica$Edad, na.rm = TRUE)
desv_edad

## cuarto punto ##
tot_persinherm <- sum(baseEstadistica$Hermanos > 3)
tot_persinherm

NumeroHermanos <- ggplot(baseEstadistica, aes(x = factor(Hermanos))) + 
  geom_bar(fill = "#1f77b4") + 
  labs(title = "Numero de Hermanos", x = "Hermanos", y = "Frecuencia") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, fontface = "italic", size = 4)+
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none")
NumeroHermanos

## quinto punto ##
rangoIngresoMensual <- max(baseEstadistica$IngresoMensual) - min(baseEstadistica$IngresoMensual); rangoIngresoMensual
varianzaIngreso <- var(baseEstadistica$IngresoMensual); varianzaIngresoMensual
cv <- round(sd(baseEstadistica$IngresoMensual) / mean(baseEstadistica$IngresoMensual) * 100, 2); cv

## sexto punto ##
histogramaEstatura <- ggplot(baseEstadistica, aes(x = Estatura)) +
  geom_histogram(bins=30,fill = "#69b3a2", color = "white", alpha = 0.8) +
  scale_x_continuous(labels = comma) +
  labs(title = "Distribución de la Estatura",
       x = "Estatura",
       y = "Frecuencia") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5, colour = "black"),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )
histogramaEstatura

## septimo punto ##

est_gen_prom <- baseEstadistica %>%
  group_by(Genero) %>%
  summarise(Estatura_promedio = mean(Estatura, na.rm = TRUE)); est_gen_prom

diferencia_maxima <- max(est_gen_prom$Estatura_promedio) - min(est_gen_prom$Estatura_promedio)
round(diferencia_maxima, 2)

barplotGeneroEstatura <- ggplot(est_gen_prom, aes(x = Genero, y = Estatura_promedio, fill = Genero)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Estatura Promedio por Genero",
       y = "Estatura Promedio", x = "Genero") +
  geom_text(aes(label = round(Estatura_promedio, 2)), vjust = -0.5, fontface = "italic", size = 4) +
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        legend.position = "none")

## octavo punto ##

BoxplotIngresoEducacion <- ggplot(baseEstadistica, aes(x = NivelEducativo, y = IngresoMensual, fill = NivelEducativo)) + 
  geom_boxplot(outlier.shape = 8, outlier.color = "red") +
  scale_fill_manual(values = c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072")) +
  labs(title = "Distribucion del Ingreso segun Nivel Educativo", 
       x = "Nivel Educativo", y = "Ingreso Mensual") +
  coord_flip() +
  theme_classic(base_size = 14, base_family = "serif") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

BoxplotIngresoEducacion

## noveno punto ##

BoxplotIngresoMensual_Educacion <- ggplot(baseEstadistica, aes(x = IngresoMensual, y = NivelEducativo, fill = NivelEducativo)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8, alpha = 0.7) +
  scale_fill_brewer(palette = "PuBu") +
  labs(title = "Boxplot del Ingreso Mensual por Nivel Educativo",
       x = "Ingreso Mensual", y = "Nivel Educativo") +
  scale_x_continuous(label = comma) +
  theme_minimal(base_size = 12)
BoxplotIngresoMensual_Educacion

## decimo punto ##

DispercionEdadIngreso <- ggplot(baseEstadistica, aes(x = Edad, y = IngresoMensual, fill = IngresoMensual, color = IngresoMensual)) + 
  geom_point(size = 1, alpha = 0.5) +
  labs(title = "Ingreso Mensual según la Edad",
       x = "Edad", y = "Ingreso Mensual")
DispercionEdadIngreso

## onceavo punto ##

tabla <- table(baseEstadistica$Genero, baseEstadistica$NivelEducativo)
tabla

## doceavo punto ##

condicion_hermanos <- baseEstadistica %>%
  filter(Hermanos > 2) 

casos_condicion <- condicion_hermanos %>%
  filter(IngresoMensual > 3000) %>%
  nrow()

casos_totales <- nrow(condicion_hermanos)

proporcion <- casos_condicion/casos_totales
round(proporcion, 2)

plot(baseEstadistica$Estatura, baseEstadistica$Edad)
plot(baseEstadistica$Estatura, baseEstadistica$HorasDeTrabajo)
plot(baseEstadistica$IngresoMensual, baseEstadistica$HorasDeTrabajo)

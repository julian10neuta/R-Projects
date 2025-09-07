#######################################################################
################# IMPORTACIÓN DE LA BASE DE DATOS #####################
#######################################################################
library(readxl)

base <- read.csv("Produccion_de_frutales_en_el_Valle_del_Cauca_20250409.csv")
#View(base)

#######################################################################
################## PREPARACIÓN DE LA BASE DE DATOS ####################
#######################################################################

cat("La base contiene", nrow(base), "filas")
cat("La base contiene", ncol(base), "columnas, las cuales son:")
cat(names(base))
s <- summary(base)
s2 <- str(base)
cat("La base de datos se encuentra con una cantidad de datos nulos de:",sum(is.na(base)))
colSums(is.na(base))
unique(base$Cultivo)

#######################################################################
########### Algunas busquedas en la base de datos usando dplyr ########
#######################################################################

library(dplyr)
##Cultivo con mayor y menor cantidad de filas en la base
b1 <- base %>% count(Cultivo) %>% filter(n == max(n) | n == min(n))
##Cinco cultivos con mayor cantidad de filas en la base
b2 <- base %>% count(Cultivo, sort = TRUE) %>% slice_head(n = 5)
##Cantidad de filas no repetidas entre año, municipio y subregion para el cultivo de Plátano
b3 <- base %>% filter(Cultivo == "Plátano") %>% distinct(año, Municipio, Subregion) %>% count()
##Base de datos del cultivo de Plátano en el municipio de Dagua
b4 <- base %>% filter(Cultivo == "Plátano" & Municipio == "Dagua")

#######################################################################
####### Creación de los dataframes para las graficas con ggplot #######
#######################################################################

library(dplyr)
##Cantidad de toneladas de frutas por munipios (10 con mayor cantidad de toneladas)
toneladasMunicipios <- base %>%
  filter(!Cultivo %in% c("Melón", "Sandía")) %>%
  group_by(Municipio) %>%
  summarise(Total_toneladas = sum(Produccion_toneladas)) %>%
  arrange(desc(Total_toneladas)) %>%
  slice_head(n = 10)

##Cantidad de toneladas por cultivos de ciclo anual
toneladasCultivos <- base %>%
  filter(Ciclo == "Anual") %>%
  group_by(Cultivo) %>%
  summarise(Total_toneladas = sum(Produccion_toneladas)) %>%
  arrange(desc(Total_toneladas)) %>%
  slice_head(n = 10) %>%
  mutate(porcentage = Total_toneladas/sum(Total_toneladas) * 100, label = paste0(Cultivo, "(", round(porcentage, 1), "%)"))

##Diez cultivos con mayor cantidad de toneladas en el municipio con mayor producción
ToneladasMunicipioMayorProd <- base %>% filter(Municipio == "Sevilla") %>%
  group_by(Cultivo) %>%
  summarise(Total_toneladas = sum(Produccion_toneladas)) %>%
  arrange(desc(Total_toneladas)) %>%
  slice_head(n = 5) %>%
  mutate(porcentage = Total_toneladas/sum(Total_toneladas) * 100, label = paste0(Cultivo, "(", round(porcentage, 1), "%)"))

##Producción de Plátano por año
Platano <- base %>% filter(Cultivo == "Plátano") %>%
  group_by(año) %>% 
  summarise(Total_toneladas = sum(Produccion_toneladas))

##Producción de Banano por año
Banano <- base %>% filter(Cultivo == "Banano") %>%
  group_by(año) %>% 
  summarise(Total_toneladas = sum(Produccion_toneladas))

#Historico
Platano <- Platano %>%
  mutate(Cultivo = "Plátano")
Banano <- Banano %>%
  mutate(Cultivo = "Banano")
historico <- bind_rows(Platano, Banano)

#######################################################################
############################ GRAFICAS #################################
#######################################################################

library(ggplot2)
## GRAFICA 1
g1 <- ggplot(toneladasMunicipios, aes(x=reorder(Municipio, -Total_toneladas), y=Total_toneladas)) +
  geom_bar(stat = "identity", fill = c("lightblue", "lightyellow", "lightgreen", "lightpink", "aquamarine", "mistyrose", "lightcyan", "lightsalmon", "lavender", "honeydew"), colour="black") +
  ggtitle("Los 10 municipios con mayor producción de cultivos en toneladas") +
  xlab("Municipio") +
  ylab("Cantidad de toneladas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## GRAFICA 2

colores_frutas <- c(
  "Plátano"    = "#FFF68f",  # amarillo plátano
  "Cítricos"   = "#FFDAB9",  # naranja
  "Piña"       = "#FFD700",  # dorado
  "Banano"     = "#FFFACD",  # amarillo claro
  "Papaya"     = "#FF8C00",  # naranja oscuro
  "Chontaduro" = "#FF4500",  # naranja quemado
  "Vid"        = "#800080",  # morado uva
  "Aguacate"   = "#228B22",  # verde oscuro
  "Maracuyá"   = "#e5c633",  # amarillo verdoso
  "Guayaba"    = "#FFB6C1",  # rosado claro
  "Naranja"    = "#FFA500"   # naranja
)

g2 <- ggplot(toneladasCultivos, aes(x="", y = Total_toneladas, fill = Cultivo)) +
  geom_bar(stat = "identity", width = 1, colour = "grey30", size = 0.5) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de los 10 cultivos de mayor producción") +
  theme_void() +
  scale_fill_manual(values = colores_frutas)

## GRAFICA 3
library(tibble)
labels_cultivo <- ToneladasMunicipioMayorProd %>%
  distinct(Cultivo, label) %>%
  deframe()

g3 <- ggplot(ToneladasMunicipioMayorProd, aes(x="", y = Total_toneladas, fill = Cultivo)) +
  geom_bar(stat = "identity", width = 1, colour = "grey30", size = 0.5) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de los 5 cultivos de mayor producción en el municipo de Sevilla") +
  theme_void() +
  scale_fill_manual(values = colores_frutas, labels = labels_cultivo)

## GRAFICA 4
library(scales)
g4 <- ggplot(historico, aes(x = año, y = Total_toneladas, color = Cultivo)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("Plátano" = "gold", "Banano" = "green3")) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Producción de Plátano y Banano por año desde 2000 hasta 2022",
    x = "Año",
    y = "Toneladas",
    color = "Cultivo"
  ) +
  theme_minimal()

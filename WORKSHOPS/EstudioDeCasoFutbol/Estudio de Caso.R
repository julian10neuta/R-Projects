
library("dplyr")
library(ggplot2)
library(lmtest)
library(car)

baseDatos <- read.csv("D:/Documentos/RStudio Files/Workshops/EstudioDeCasoFutbol/players_15.csv")
View(baseDatos)
colSums(is.na(baseDatos))

baseDatos <- select(baseDatos, -release_clause_eur, -nation_jersey_number, -gk_kicking,
                    -gk_positioning, -gk_diving, -gk_reflexes, -gk_handling, -gk_speed, 
                    -mentality_composure)

baseDatos <- baseDatos[!is.na(baseDatos$shooting), ]

b <- baseDatos %>% filter(shooting <= 25);
View(b)


## Analisis Variable Shooting
ggplot(baseDatos, aes(x = shooting)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#CD8C95", color = "black") +
  scale_x_continuous(
    name = "Shooting score",
    breaks = seq(0, ceiling(max(baseDatos$shooting) / 10) * 10, by = 10)
  ) +
  ylab("Frecuencia") +
  ggtitle("Histograma de shooting") +
  theme_bw()

boxplot(baseDatos$shooting, horizontal = TRUE, 
        xlab = "Shooting score", col = "#8B5F65",
        main = "Diagrama de Boxplot variable Shooting")

## Analisis Variable Dribbling
ggplot(baseDatos, aes(x = dribbling)) +
  geom_histogram(binwidth = 5, boundary = 0, fill = "#87CEFA", color = "black") +
  scale_x_continuous(
    name = "Dribbling score",
    breaks = seq(0, ceiling(max(baseDatos$shooting) / 10) * 10, by = 10)
  ) +
  ylab("Frecuencia") +
  ggtitle("Histograma de dribbling") +
  theme_bw()

boxplot(baseDatos$dribbling, horizontal = TRUE, 
        xlab = "Dribbling score", col = "#87CEFA",
        main = "Diagrama de Boxplot variable Dribbling")


# Muestra principal de 140 elementos
set.seed(10)
indices_muestra <- sample(nrow(baseDatos), 140)
muestra <- baseDatos[indices_muestra, ]

# Ahora sacamos los 10 datos solo de los que no fueron seleccionados antes
set.seed(7)
restantes <- setdiff(1:nrow(baseDatos), indices_muestra)
indices_test <- sample(restantes, 10)
test <- baseDatos[indices_test, ]


# Regresión lineal

# a)
plot(muestra$shooting, muestra$dribbling, col = "blue4",
     xlab = "Shooting score", ylab = "Dribbling score", 
     main = "Gráfico de disperción Shooting vs Dribbling", pch = 19)
cor(muestra$shooting, muestra$dribblin)

# b)
modelo <- lm(dribbling ~ shooting, muestra)
modelo$coefficients
# modelo$fitted.values
# modelo$residuals

# c)
summary(modelo) # La hipotesis nula se rechaza, por lo tanto, 
#shooting está linealmente relacionado con dribbling y el coeficiente B1 es != 0

# d)
anova(modelo) 
# La hipotesis nula de que el modelo SIN shooting es tan bueno como el modelo CON shooting
# se rechaza debido a p -value, por lo tanto, el modelo con shooting es significativamente mejor.

# f)
plot(modelo$residuals, col = "green4")
dwtest(modelo)
# Los residuos son independientes y no se encuentran correlacionados
hist(modelo$residuals, col = "steelblue")
# Los residuos son normales como se representa en el histograma de residuos
bptest(modelo)
# Los residuos tienen varianza constante, es decir, existe homocedasticidad
plot(modelo, 1, main = "Análisis de Linealidad", col = "green4", pch = 10)
# Los residuos presentan linealidad ya que en el gráfico están distribuidos aleatoriamente

# g)
par(mar = c(5, 5, 4, 2))
influenceIndexPlot(modelo, vars="Bonf", las=1, col = "darkblue",
                   main = "Valores atípicos (Bonferroni p-value)",
                   las = 1, ylim = c(0, 1))
par(mar = c(5, 6, 4, 2))
influenceIndexPlot(modelo, vars="Cook", col = "darkred",
                   main = "Observaciones Influyentes (Cook's Distance)",
                   las = 1, ylim = c(0, 0.1))

# h)
nuevos_datos <- data.frame(shooting = seq(min(muestra$shooting), max(muestra$shooting), length.out = 100))
pred_conf <- predict(modelo, newdata = nuevos_datos, interval = "confidence", level = 0.95); pred_conf
pred_pred <- predict(modelo, newdata = nuevos_datos, interval = "prediction", level = c(0.95)); pred_pred

nuevos_datos$fit <- pred_conf[, "fit"]
nuevos_datos$conf_lwr <- pred_conf[, "lwr"]
nuevos_datos$conf_upr <- pred_conf[, "upr"]
nuevos_datos$pred_lwr <- pred_pred[, "lwr"]
nuevos_datos$pred_upr <- pred_pred[, "upr"]

ggplot(muestra, aes(x=shooting, y=dribbling)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x, se=FALSE, col = 'blue') +
  geom_ribbon(data = nuevos_datos, aes(x = shooting, ymin = conf_lwr, ymax = conf_upr),
              fill = 'orchid4', alpha = 0.2, inherit.aes = FALSE) +  
  geom_ribbon(data = nuevos_datos, aes(x = shooting, ymin = pred_lwr, ymax = pred_upr),
              fill = 'red', alpha = 0.1, inherit.aes = FALSE) +
  theme_bw()

#
View(test)


predict(modelo, newdata = data.frame(shooting = 43), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 27), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 68), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 53), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 65), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 43), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 50), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 50), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 59), interval = "prediction", level = 0.95)
predict(modelo, newdata = data.frame(shooting = 48), interval = "prediction", level = 0.95)

        
# Cargar el CSV
df <- read.csv("ds_salaries.csv")
# Verificar las primeras filas del dataframe
head(df)
# Convertir la columna 'experience_level' a un factor y luego a un valor numérico
df$experience_level_num <- as.numeric(factor(df$experience_level,
                                             levels = c("EN", "MI", "SE", "EX"),
                                             labels = c(1, 2, 3, 4)))
# Verificar la transformación
table(df$experience_level, df$experience_level_num)
# Crear el modelo de regresión lineal
modelo <- lm(salary_in_usd ~ experience_level_num, data = df)
# Resumen del modelo
summary(modelo)
# Cargar librerías necesarias
library(ggplot2)
# Crear la gráfica
ggplot(df, aes(x = experience_level_num, y = salary_in_usd)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Modelo de regresión lineal de salario en función del nivel de experiencia",
       x = "Nivel de experiencia (Numérico)",
       y = "Salario en USD") +
  scale_x_continuous(breaks = 1:4, labels = c("EN", "MI", "SE", "EX"))


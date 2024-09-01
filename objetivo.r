library(ggplot2)
datos <- read.csv("ds_salaries.csv")


contingency_table <- table(datos$experience_level, datos$company_size)

freqabTot <- addmargins(contingency_table)


freqrel <- prop.table(contingency_table)

print(chisq_test$expected)

barplot(freqrel,
        col = c(2:5),
        beside = TRUE,
        ylim = c(0, .7), ylab = "Nivel de experiencia", main = "Nivel de experiencia según el tamaño de la empresa ");legend(x = "topright",legend = unique(datos$experience_level),  fill = c(2:5))



chisq_test <- chisq.test(freqab)
print(chisq_test)


df_contingency <- as.data.frame(as.table(contingency_table))

# Crear el heatmap
ggplot(df_contingency, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Nivel de Experiencia", y = "Tamaño de la Empresa", fill = "Frecuencia") +
  theme_minimal()
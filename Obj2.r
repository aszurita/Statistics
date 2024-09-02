library(ggplot2)
datos <- read.csv("ds_salaries.csv")

contingency_table <- table(datos$experience_level, datos$company_size)
print(contingency_table)
freqabTot <- addmargins(contingency_table)
print(freqabTot)

freqrel <- prop.table(contingency_table)
print(freqrel)

print(chisq_test$expected)

barplot(freqrel,
        col = c(2:5),
        beside = TRUE,
        ylim = c(0, .7), ylab = "Nivel de experiencia", main = "Nivel de experiencia según el tamaño de la empresa ");legend(x = "topright",legend = unique(datos$experience_level),  fill = c(2:5))


chisq_test <- chisq.test(freqab)
print(chisq_test)
cat("El valor p es:", chisq_test$p.value, "\n")


# Convertir la tabla de contingencia a un data.frame
contingency_df <- as.data.frame(contingency_table)

# Renombrar las columnas para mayor claridad
colnames(contingency_df) <- c("experience_level", "company_size", "count")

contingency_df <- contingency_df %>%
  group_by(company_size) %>%
  mutate(proportion = count / sum(count))
contingency_df$experience_level <- as.factor(contingency_df$experience_level)
contingency_df$company_size <- as.factor(contingency_df$company_size)
contingency_df$proportion <- as.numeric(contingency_df$proportion)

ggplot(contingency_df, aes(x = company_size, y = proportion, fill = experience_level)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +  
  labs(title = "Proporción de Nivel de Experiencia según el Tamaño de la Empresa",
       x = "Tamaño de la Empresa",
       y = "Proporción de Empleados",
       fill = "Nivel de Experiencia") +
  scale_fill_manual(values = c("EN" = "#1f77b4",  
                               "EX" = "#ff7f0e",  
                               "MI" = "#2ca02c",  
                               "SE" = "#d62728"   
  )) +
  theme_minimal()

#Prueba de hipótesis
# X - trabajadores con nivel de experiencia senior en empresas grandes
p1 <- 158 
n1 <- 263 
# Y trabajadores con nivel de experiencia senior en empresas medianas
p2 <- 2087
n2 <- 2723

prop.test(c(p1, p2), n = c(n1, n2), alternative = "greater")
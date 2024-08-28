df_salaries <- read.csv("ds_salaries_US.csv")
print(head(df_salaries))

print("CATEGORIAS UNICAS") 
categorias <- unique(df_salaries$job_category)
print(categorias)


# Function to filter by job category and select columns
filter_and_select <- function(data, job_category, columns) {
  filtered_data <- data[data$job_category == job_category, columns]
  return(filtered_data)
}

# Define the columns of interest
columns_of_interest <- c("job_category", "salary_in_usd")

# Data Scientist
print("# Data Scientist")
DS <- filter_and_select(df_salaries, "Data Scientist", columns_of_interest)
print(head(DS))

# Data Engineer
print("# Data Engineer")
DE <- filter_and_select(df_salaries, "Data Engineer", columns_of_interest)
print(head(DE))

# Data Analyst
print("# Data Analyst")
DA <- filter_and_select(df_salaries, "Data Analyst", columns_of_interest)
print(head(DA))

salaries_DS <- as.numeric(DS$salary_in_usd)
print(" Media Data Scientist ")
median_DS  <- mean(salaries_DS)
print( median_DS )

salaries_DE <- as.numeric(DE$salary_in_usd)
print(" Media Data Engineer ")
median_DE <- mean(salaries_DE)
print(median_DE)

salaries_DA <- as.numeric(DA$salary_in_usd)
print(" Media Data Engineer ")
median_DA <- mean(salaries_DA)
print(median_DA)
# Perform a t-test to compare the mean salaries of
# Data Scientists and Data Engineers
## significancia 0.05
# Ho: median_DS >= median_DE
# Ha : median_DS < median_DE

test_result_DS_DE <- t.test(salaries_DS, salaries_DE, alternative = "less",
                      conf.level = 0.95)
print(test_result_DS_DE)
# Conclusión
# p_value = 0.9906 y el nivel de significancia 0.05
# por lo tanto p_value > nivel_significacnia no se rechaza Ho
# Y se concluye que los  Data Scientist ganan en promedio más que los Data Engineer



# Perform a t-test to compare the mean salaries of
# Data Scientists and Data Analyst
## significancia 0.05
# Ho: median_DS >= median_DA
# Ha : median_DS < median_DA

test_result_DS_DA <- t.test(salaries_DS, salaries_DA, alternative = "less",
                          conf.level = 0.95)
print(test_result_DS_DA)  
# Conclusión
# p_value = 1 y el nivel de significancia 0.05
# por lo tanto p_value > nivel_significacnia no se rechaza Ho
# Y se concluye que
# Los  Data Scientist ganan en promedio más que los Data Analyst



# Realizando las dos hipotesis que Data Scientist ganan en promedio más que los Data Analyst y que los Data Engineer,
# se puede concluir que los Data Scientist son los que gana más en el area de los datos.
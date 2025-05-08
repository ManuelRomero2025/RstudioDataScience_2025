library(readr)#Importación de archivos CSV y otros formatos delimitados.
library(naniar)#Análisis y visualización de valores faltantes (NA) en los datos.
library(ggplot2)# Visualizaciones
library(janitor)# Limpiar nombres de columnas

Data.1=as.data.frame(unclass(heart_disease),
                      stringsAsFactors = TRUE)

#1. Verificar estructura
str(Data.1)
summary(Data.1)
names(Data.1)


#2. Identificar valores faltantes
colSums(is.na(Data.1))          # Conteo por columna
gg_miss_var(Data.1)             # Gráfico de variables con más NA
vis_miss(Data.1)                # Mapa de calor de NAs
gg_miss_var(Data.1, facet = Gender)
gg_miss_fct(Data.1, fct = Gender)

#3. Eliminar duplicados
sum(duplicated(Data.1)) # Cuántos duplicados hay
Data.1=Data.1[!duplicated(Data.1), ]  # Eliminar duplicados

#4. Estandarizar nombres de columnas
## Evitar mayúsculas, espacios, o caracteres especiales.
Data.1=clean_names(Data.1)

#5. Detectar y tratar outliers (valores atípicos)
boxplot.stats(Data.1$age)

ggplot(Data.1, aes(x = gender, 
                   y = age, 
                   fill = gender)) +
  geom_boxplot() +
  labs(title = "Edad por Género",
       x = "Género", y = "Edad") +
  theme_minimal()

#6. Eliminar o imputar outliers
remove_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  H <- 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H) | x > (qnt[2] + H)] <- NA
  return(x)
}

numeric_vars <- sapply(Data.1, is.numeric)
Data.1[, numeric_vars]=lapply(Data.1[, numeric_vars], remove_outliers)

# 7. Imputación de NA en variables numéricas con mediana
Data.1[, numeric_vars] <- lapply(Data.1[, numeric_vars], function(x) {
  ifelse(is.na(x), median(x, na.rm = TRUE), x)
})

# 8. Imputación de NA en variables categóricas con moda
categorical_vars <- sapply(Data.1, is.factor)

# Función para imputar moda
imputar_moda <- function(x) {
  moda <- names(which.max(table(x)))
  x[is.na(x)] <- moda
  return(x)
}

Data.1[, categorical_vars] <- lapply(Data.1[, categorical_vars], imputar_moda)

# 9. Validación final (no deben quedar NA)
cat("Resumen de valores NA por columna:\n")
colSums(is.na(Data.1))

# 10. Resumen general limpio
summary(Data.1)

###########################################
##análisis exploratorio gráficos ggplot2###
###########################################

#1. Distribución de edades
ggplot(Data.1, aes(x = age)) +
  geom_histogram(bins = 10, 
                 fill = "steelblue", 
                 color = "white") +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

#2. Proporción de pacientes por género
ggplot(Data.1, aes(x = gender, 
                   fill = gender)) +
  geom_bar() +
  labs(title = "Distribución por Género", 
       x = "Género", y = "Conteo") +
  theme_minimal() +
  theme(legend.position = "none")

#3. Edad según presencia de enfermedad cardíaca
ggplot(Data.1, aes(x = heart_disease_status,
                   y = age,
                   fill = heart_disease_status)) +
  geom_boxplot() +
  labs(title = "Edad según presencia de enfermedad cardíaca",
       x = "¿Tiene enfermedad cardíaca?", y = "Edad") +
  theme_minimal()


#4. Frecuencia de enfermedad cardíaca por género
ggplot(Data.1, aes(x = gender,
                   fill = heart_disease_status)) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de enfermedad cardíaca por género",
       x = "Género", y = "Proporción") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

#5. Relación entre presión arterial y colesterol
ggplot(Data.1, aes(x = blood_pressure,
                   y = cholesterol_level,
                   color = heart_disease_status)) +
  geom_point(alpha = 0.7) +
  labs(title = "Relación entre presión arterial y nivel de colesterol",
       x = "Presión arterial", 
       y = "Colesterol") +
  theme_minimal()

#6. Mapa de calor de correlaciones
library(corrplot)
cor_mat <- cor(Data.1[, sapply(Data.1, is.numeric)])
corrplot::corrplot(cor_mat, 
                   method = "color", 
                   type = "upper", 
                   tl.cex = 0.8)

#7. Densidad de edad por género
ggplot(Data.1, aes(x = age, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de la edad por género",
       x = "Edad", y = "Densidad") +
  theme_minimal()
#8. Facetas por hábito de ejercicio
ggplot(Data.1, aes(x = age)) +
  geom_histogram(binwidth = 5, 
                 fill = "darkorange",
                 color = "white") +
  facet_wrap(~ exercise_habits) +
  labs(title = "Distribución de edad por hábito de ejercicio",
       x = "Edad", y = "Frecuencia") +
  theme_minimal()

Data.1$stress_level
#9. Nivel de estrés por estado de enfermedad cardíaca
ggplot(Data.1, aes(x = stress_level, 
                   fill = heart_disease_status)) +
  geom_bar(position = "dodge") +
  labs(title = "Frecuencia de niveles de estrés según enfermedad cardíaca",
       x = "Nivel de estrés", y = "Frecuencia",
       fill = "Estado de enfermedad cardíaca") +
  theme_minimal()


#10. Comparación de colesterol LDL y HDL
ggplot(Data.1, aes(x = high_ldl_cholesterol, 
                   fill = low_hdl_cholesterol)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparación LDL alto vs. HDL bajo",
       x = "LDL alto",
       y = "Frecuencia", 
       fill = "HDL bajo") +
  theme_minimal()

# Instalar y cargar las librerías necesarias
install.packages("ggplot2")    # Para visualización de datos
install.packages("corrplot")   # Para visualizar la matriz de correlación
install.packages("caret")      # Para dividir los datos en entrenamiento y prueba
install.packages("dplyr")      # Para manipulación de datos
install.packages("readr")      # Para leer archivos CSV de manera eficiente
install.packages("tidyr")      # Para manipular y limpiar datos
install.packages("forcats")    # Para manejar factores (variables categóricas)
install.packages("stringr")    # Para manipular cadenas de texto
install.packages("e1071")      # Para algunas funciones estadísticas y modelado

# Cargar las librerías
library(ggplot2)
library(corrplot)
library(caret)
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(stringr)

data <- read_csv("D:/4/5/rstudio/tr1/house_price_bd.csv")

head(data)

colSums(is.na(data))
# Eliminar el símbolo de la moneda (৳) y las comas, luego convertir a numérico
data$Price_in_taka <- as.numeric(gsub("৳", "", gsub(",", "", data$Price_in_taka)))

# Verificar si se han convertido correctamente a numéricos
sum(is.na(data$Price_in_taka))  # Ver si hay valores NA en la columna Price_in_taka
# Imputar los valores NA con la media
data$Price_in_taka[is.na(data$Price_in_taka)] <- mean(data$Price_in_taka, na.rm = TRUE)

# Eliminar filas duplicadas
data <- data[!duplicated(data), ]

# Verificar si se han eliminado duplicados
dim(data)

# Histograma de los precios
ggplot(data, aes(x = Price_in_taka)) +
  geom_histogram(binwidth = 1000000, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribución de los Precios de las Propiedades", x = "Precio en Taka", y = "Frecuencia")

# Histograma con escala logarítmica en el eje X
ggplot(data, aes(x = Price_in_taka)) +
  geom_histogram(binwidth = 1000000, fill = "blue", color = "black") +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Distribución de los Precios de las Propiedades (Escala Logarítmica)", 
       x = "Precio en Taka (Escala Log)", y = "Frecuencia")

# Relación entre el precio y el número de habitaciones
ggplot(data, aes(x = Bedrooms, y = Price_in_taka)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre Número de Habitaciones y Precio", 
       x = "Número de Habitaciones", y = "Precio en Taka")

# Relación entre el precio y el tamaño del piso (Floor_area)
ggplot(data, aes(x = Floor_area, y = Price_in_taka)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Relación entre Tamaño del Piso y Precio", 
       x = "Área del Piso (m²)", y = "Precio en Taka")

# Seleccionar solo las columnas numéricas
numeric_data <- data[, sapply(data, is.numeric)]

# Calcular la matriz de correlación
correlation_matrix <- cor(numeric_data, use = "complete.obs")

# Visualizar la matriz de correlación
library(corrplot)
corrplot(correlation_matrix, method = "circle")

# Codificar la columna "City" en variables dummy (One-Hot Encoding)
data <- cbind(data, model.matrix(~ City - 1, data = data))

# Crear una nueva variable que sea la relación entre el área del piso y el número de habitaciones
data$area_per_bedroom <- data$Floor_area / data$Bedrooms

# Establecer la semilla para la reproducibilidad
set.seed(123)

# Dividir los datos en conjunto de entrenamiento (80%) y prueba (20%)
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Verificar las dimensiones de los conjuntos
dim(train_data)
dim(test_data)



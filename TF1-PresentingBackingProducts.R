# Caso 1: PRESENTING BANKING PRODUCTS

# 1. Preparación 

# Instalación de librerías 
install.packages("leaps")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")
install.packages("ROCR")
install.packages("readxl")  


# Llamar librerias
library(leaps) 
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(ROCR)
library(readxl)

# 2. Cargar datos ---------------------------------------------------------

# PREGUNTA 1: Calcule la rentabilidad promedio de los clientes que vieron y la de los clientes que no vieron la presentación. ¿Es efectiva la presentación? Explique.
# RESPUESTA 1: Más que a la efectividad de la presentación, la gran diferencia se da en las rentas promedios, 
# por lo que es muy probable que la proyección de rentabilidad sea inferior por menor ingreso.
archivo_base <- "/Users/carolinaherreraazolas/Downloads/Presenting_Banking_Products.xlsx"

data <- read_excel(archivo_base) 
print(data)

#Renombramos la columna Monthly Income

data <- data %>%
  rename(Monthly_Income = `Monthly Income`) 

# Separación de clientes

# Creamos subconjuntos de datos

grupoConPresentacion <- data %>% filter(Presentation == 1)  # Clientes que asistieron a la presentación
grupoSinPresentacion <- data %>% filter(Presentation == 0)  # Clientes que no asistieron

# Grupo con presentación
promedios_presentacion <- grupoConPresentacion %>%
  summarise(
    Edad_Promedio = mean(Age, na.rm = TRUE),
    Ingreso_Promedio = mean(Monthly_Income, na.rm = TRUE),
    Rentabilidad_Promedio = mean(Profit, na.rm = TRUE)
  )

# Grupo sin presentación
promedios_no_presentacion <- grupoSinPresentacion %>%
  summarise(
    Edad_Promedio = mean(Age, na.rm = TRUE),
    Ingreso_Promedio = mean(Monthly_Income, na.rm = TRUE),
    Rentabilidad_Promedio = mean(Profit, na.rm = TRUE)
  )

# Mostrar resultados
print("Promedios para clientes que asistieron a la presentación:")
print(promedios_presentacion)

print("Promedios para clientes que no asistieron a la presentación:")
print(promedios_no_presentacion)

# Gráfico de densidad de ingreso mensual por grupo
ggplot(data, aes(x = Monthly_Income, fill = factor(Presentation))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de ingresos por grupo",
       x = "Ingreso mensual",
       y = "Densidad",
       fill = "Presentación") +
  theme_minimal()


#PREGUNTA 2: Incorpore en su análisis las variables de control disponibles. Con este nuevo análisis, ¿es efectiva la presentación? Explique
#RESPUESTA 2

modelo <- lm(Profit ~ Presentation + Age + Monthly_Income + Gender, data = data)

summary(modelo)

data$predicted_profit <- predict(modelo, data)

# Graficar las predicciones por grupo de presentación
ggplot(data, aes(x = Monthly_Income, y = predicted_profit, color = factor(Presentation))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Impacto ajustado de la presentación en la rentabilidad",
       x = "Ingreso mensual",
       y = "Rentabilidad ajustada",
       color = "Presentación") +
  theme_minimal()


# 


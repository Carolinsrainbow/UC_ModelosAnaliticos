#Caso 3: Germain Credit
# 1. Preparación 

# Instalación de librerías 
install.packages("leaps")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")
install.packages("ROCR")
 

# Llamar librerias
library(leaps) 
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(ROCR)

# 2. Cargar datos ---------------------------------------------------------

archivo_base <- "/Users/carolinaherreraazolas/Downloads/german_credit_data.csv"

archivo <- read.csv(archivo_base) # Lo usé sólo para ver como venían los datos


# 3. Exploración de datos ---------------------------------------------------------

# Valores faltantes 
colSums(is.na(archivo)) 
str(archivo)            

# Limpiamos datos

data <-  read.csv(archivo_base, na.strings = "NA") %>% 
  select(1:11) # %>%  
  #na.omit()     


# PREGUNTA 1  ---------------------------------------------------------
#a) Calcule estadísticas descriptivas de los datos y en base a estas determine potenciales observaciones inusuales.

data <- data %>%
  mutate(Job = factor(Job,
                      levels = c(0, 1, 2, 3),
                      labels = c("unskilled and non-resident", 
                                 "unskilled and resident", 
                                 "skilled", 
                                 "highly skilled")),
         Sex = factor(Sex),
         Housing = factor(Housing),
         Saving.accounts = factor(Saving.accounts),
         Checking.account = factor(Checking.account),
         Purpose = factor(Purpose),
         Good = factor(Good, 
                       levels = c(0, 1), 
                       labels = c("Not Good", "Good"))
  )


dim(data)
head(data)
summary(data)

#Analítica descriptiva
skimr::skim(data)

# Detección de outliers

outliers <- data %>%
  select_if(is.numeric) %>%
  summarise(
    across(everything(), 
           list(Q1 = ~quantile(., 0.25, na.rm = TRUE),
                Q3 = ~quantile(., 0.75, na.rm = TRUE),
                IQR = ~IQR(., na.rm = TRUE)),
           .names = "{.col}_{.fn}")
  )


outlier_limits <- outliers %>%
  mutate(across(ends_with("Q1"), ~ . - 1.5 * get(sub("Q1", "IQR", cur_column())), .names = "{.col}_lower"),
         across(ends_with("Q3"), ~ . + 1.5 * get(sub("Q3", "IQR", cur_column())), .names = "{.col}_upper"))

potential_outliers <- data %>%
  select_if(is.numeric) %>%
  rowwise() %>%
  mutate(outlier_flag = any(c_across(everything()) < outlier_limits[1,] | 
                              c_across(everything()) > outlier_limits[2,]))

# Gráfico para determinar outliers

data %>%
  pivot_longer(cols = where(is.numeric), names_to = "Variable", values_to = "Valores") %>%
  ggplot(aes(x = Variable, y = Value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Pregunta 2
# b) Estime el mejor modelo de regresión logística con los datos disponibles en base a los criterios: significancia estadística, AIC y BIC.

#Modelo 1: Todas las variables

modelo1 <- glm(Good ~ Age + Sex + Job + Housing+ Saving.accounts + Checking.account  + Credit.amount + Duration, 
               data = data, 
               family = binomial)
summary(modelo1)
AIC(modelo1)
BIC(modelo1)


#Modelo 2: Sacamos las no significativas

modelo2 <- glm(Good ~ Saving.accounts + Checking.account + Duration, 
               data = data, 
               family = binomial)

summary(modelo2)
AIC(modelo2)
BIC(modelo2)

# Pregunta 3
# c) Calcule las probabilidades del modelo y la matriz de confusión. Calcule indicadores de la calidad de las predicciones. Comente los resultados.




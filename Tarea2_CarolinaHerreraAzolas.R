#Caso 2: Subsidios para la capacitación laboral

# 1. Preparación 

# Instalación de librerías 
install.packages("leaps")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")
install.packages("openxlsx")  

# Llamar librerias
library(leaps) 
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(openxlsx) 

# 2. Cargar datos ---------------------------------------------------------

#Abrir base de datos
data <- openxlsx::read.xlsx("/Users/carolinaherreraazolas/Downloads/Subsidios.xlsx")


# 3. Explorar datos -------------------------------------------------------

#Estadísticos descriptivos
summary(data)

#4. Transformación de datos

# Transformación de datos: Aplicar logaritmo natural a las tres variables
data <- data %>%
  mutate(nuevo_scrap = log(scrap),
         nuevo_sales = log(sales),
         nuevo_employ = log(employ))

# Crear la variable binaria T_i, que toma valor 1 si la empresa recibió el subsidio (firm_id 1 a 35)
data <- data %>%
  mutate(T_i = ifelse(firm_id <= 35, 1, 0))

# Crear la variable binaria P_t, que toma valor 1 para el año 1997 y 0 para el año 1995
data <- data %>%
  mutate(P_t = ifelse(year == 1997, 1, ifelse(year == 1995, 0, NA)))

# 5. En relación con el caso anterior resuelva las siguientes actividades:

# 5.1  Considere solo a las empresas que recibieron el subsidio y estime el efecto del subsidio a partir de una comparación pre-post del logaritmo natural de scrap entre 1995 y 1997. ¿Diría usted que este enfoque permite capturar el efecto causal del subsidio? Justifique su respuesta.

#Resolución 5.1
empresas_subsidiadas <- data %>% filter(T_i == 1)
caso_1995 <- empresas_subsidiadas$nuevo_scrap[empresas_subsidiadas$year == 1995]
caso_1997 <- empresas_subsidiadas$nuevo_scrap[empresas_subsidiadas$year == 1997]
t_test_result <- t.test(caso_1995, caso_1997, paired = TRUE)
print(t_test_result)

# Respuesta 5.1
# Utilizando como indicador el valor t (12.481) nos indica que existe una diferencia grande entre las dos medias  y valor p (3.012e-14) siendo inferior al nivel del 5% nos indica que la diferencia entre 1995 y 1997 es estadísticamente significativa.
# Si bien el resultado muestra una reducción significativa no es posible captar totalmente el efecto causal del subsidio debido a que carece de un grupo control y no se incluyen otros factores que modificarían el rendimiento, por lo que no podemos atribuir a causalidad.

# 5.2 Considere ahora a las observaciones correspondientes al año 1997 y estime el efecto del subsidio a partir de la diferencia simple del logaritmo natural de scrap entre empresas que recibieron y que no recibieron el subsidio. ¿Diría usted que este enfoque permite capturar el efecto causal del subsidio? Justifique su respuesta.

#Resolución 5.2 

anio1997 <- data %>% filter(year == 1997)
prom_scrapSubsidiado <- mean(anio1997$nuevo_scrap[anio1997$T_i == 1], na.rm = TRUE)
prom_scrapNoSubsidiado <- mean(anio1997$nuevo_scrap[anio1997$T_i == 0], na.rm = TRUE)
diferenciaSubsidio97 <- prom_scrapSubsidiado - prom_scrapNoSubsidiado
diferenciaSubsidio97

#Respuesta 5.2 
# El resultado obtenido (una diferencia de 0.1440616 en el log(scrap)) sugiere que, en promedio, las empresas que recibieron el subsidio tuvieron una tasa de desperdicio menor en logaritmo (aproximadamente 0.144 unidades menos) que las empresas que no recibieron el subsidio en el año 1997.

# 5.3  Suponga ahora que se cumplen los supuestos necesarios para que el enfoque de diferencias en diferencias sea válido. ¿Cuál es el efecto causal del subsidio? Interprete en términos económicos y luego evalúe formalmente la hipótesis nula de que efecto es 0 frente a la alternativa de que es menor a 0. Considere un nivel de significancia del 1%.

#Resolución 5.3

data <- data %>%
  mutate(DiD = T_i * P_t)

modelo_did <- lm(nuevo_scrap ~ T_i + P_t + DiD, data = data)
summary(modelo_did)

#Respuesta 5.3: FALTA

# 5.4 Repita la estimación anterior, incluyendo como variables de control al logaritmo natural de sales y employ, respectivamente. ¿Cambian en algo sus conclusiones? Justifique su respuesta.

# Resolución 5.4 

modelo_did_control <- lm(nuevo_scrap ~ T_i + P_t + DiD + nuevo_sales + nuevo_employ, data = data)
summary(modelo_did_control)

# Respuesta 5.4: FALTA

# 5.5   Finalmente, se pide que aproveche que la base de datos incluye información para el año 1993 y que evalúe formalmente la plausibilidad del supuesto necesario para que el enfoque de diferencias en diferencias sea válido. Considere un nivel de significancia del 1% y luego explique en palabras las implicancias del resultado de este contraste.

data_pre_subsidy <- data %>% filter(year %in% c(1993, 1997))

data_pre_subsidy <- data_pre_subsidy %>%
  mutate(P_1997 = ifelse(year == 1997, 1, 0))

modelo_tendencia_paralela <- lm(nuevo_scrap ~ T_i + P_1997 + T_i:P_1997, data = data_pre_subsidy)
summary(modelo_tendencia_paralela)

# Respuesta 5.5

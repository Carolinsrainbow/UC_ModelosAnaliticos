# Tutorial 8 --------------------------------------------------------------

# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
install.packages("haven")
install.packages("dplyr")
# Llamar librerias
library(haven)
library(dplyr)

# 2. Cargar datos ---------------------------------------------------------
data <- read_dta("izlaw.dta")


# 3. Explorar datos -------------------------------------------------------
#Ver datos 
View(data)

#Renombrar variable izlaw
data <- rename(data, T = izlaw)

#Generar variable post con valores
data <- data %>% 
  mutate(post = case_when(year == 2000 ~  "1",
                                year == 1995 ~ "0",
                                TRUE ~ NA_character_))


# 4. Estimar modelo  ------------------------------------------------------
#Regresión lineal con variables lnprice y post, si T==1
modelo1 <- lm(lnprice ~ post, data = data %>% filter(T==1))

#Tabla regresión
summary(modelo1)

#Regresión lineal con variables lnprice y T, si post==1
modelo2 <- lm(lnprice ~ T, data = data %>% filter(post==1))

#Tabla regresión
summary(modelo2)

#Tabla cruzada T y post
aggregate(data$lnprice, by=list(data$T, data$post), FUN=mean)

#Crear variable postxT
data$post <- as.numeric(data$post)
data$postxT <- (data$post * data$T)

#Regresión lineal con variables lnprice, post, T y postxT
modelo3 <- lm(lnprice ~ post + T + postxT, data = data)

#Tabla regresión
summary(modelo3)

#Regresión lineal con variables lnprice, post, T, postxT y lmedhhinc
modelo4 <- lm(lnprice ~ post + T + postxT + lmedhhinc, data = data)

#Tabla regresión
summary(modelo4)

#Regresión lineal con variables lnprice, post, T, postxT, lmedhhinc, educattain, proppoverty y lpop
modelo5 <- lm(lnprice ~ post + T + postxT + lmedhhinc + educattain + proppoverty + lpop, data = data)

#Tabla regresión
summary(modelo5)

#Regresión lineal con variables lnunits, post, T y postxT
modelo6 <- lm(lnunits ~ post + T + postxT, data = data)

#Tabla regresión
summary(modelo6)

#Regresión lineal con variables lnunits, post, T, postxT, lmedhhinc, educattain, proppoverty y lpop
modelo7 <- lm(lnunits ~ post + T + postxT + lmedhhinc + educattain + proppoverty + lpop, data = data)

#Tabla regresión
summary(modelo7)

#Generar variable post_1 con valores
data <- data %>% 
  mutate(post_1 = case_when(year == 1995 ~  "1",
                          year == 1990 ~ "0",
                          TRUE ~ NA_character_))

#Generar variable post_1xT
data$post_1 <- as.numeric(data$post_1)
data$post_1xT <- (data$post_1 * data$T)

#Regresión lineal con variables lnprice, post_1, T y post_1xT
modelo8 <- lm(lnprice ~ post_1 + T + post_1xT, data = data)

#Tabla regresión
summary(modelo8)

#Regresión lineal con variables lnprice, post_1, T, post_1xT y lmedhhinc
modelo9 <- lm(lnprice ~ post_1 + T + post_1xT + lmedhhinc, data = data)

#Tabla regresión
summary(modelo9)

#Regresión lineal con variables lnprice, post_1, T, post_1xT, lmedhhinc, educattain, proppoverty y lpop
modelo10 <- lm(lnprice ~ post_1 + T + post_1xT + lmedhhinc + educattain + proppoverty + lpop, data = data)

#Tabla regresión
summary(modelo10)

#Regresión lineal con variables lnunits, post_1, T, post_1xT, lmedhhinc, educattain, proppoverty y lpop
modelo11 <- lm(lnunits ~ post_1 + T + post_1xT + lmedhhinc + educattain + proppoverty + lpop, data = data)

#Tabla regresión
summary(modelo11)
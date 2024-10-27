# Tutorial 5 --------------------------------------------------------------
# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
#install.packages("haven")
#install.packages("multcomp")
# Llamar librerias
library(haven)
library(multcomp)


# 2. Cargar datos ---------------------------------------------------------
#Abrir base de datos
data <- read_dta("utown.dta")


# 3. Explorar datos -------------------------------------------------------
#Estadísticos descriptivos
summary(data)


# 4. Regresion multiple ---------------------------------------------------
#Regresión múltiple con price, sqft, age, pool, fplace
modelo1 <- lm(price ~ sqft + age + pool + fplace, data = data)

#Tabla regresión
summary(modelo1)

#Estat VCE
vcov(modelo1)

#lincom sqft=pool
summary(glht(modelo1, linfct = c("sqft-pool=0")))

#lincom pool=0
summary(glht(modelo1, linfct = c("pool = 0")))

#gen no_pool
data$no_pool <- 1-(data$pool)

#Regresión múltiple con price, sqft, age, pool, fplace
modelo2 <- lm(price ~ sqft + age + no_pool + fplace, data = data)

#Tabla regresión
summary(modelo2)

#Regresión múltiple con price, sqft, age, pool, no_pool, fplace
modelo3 <- lm(price ~ sqft + age + pool + no_pool + fplace, data = data)

#Tabla regresión
summary(modelo3)

#Regresión múltiple con price, sqft, age, pool, fplace, utown
modelo4 <- lm(price ~ sqft + age + pool + fplace + utown, data = data)

#Tabla regresión
summary(modelo4)

#gen sqftXutown
data$sqftXutown <- (data$utown * data$sqft)

#Regresión múltiple con price, sqft, sqftXutown, age, pool, fplace, utown
modelo5 <- lm(price ~ sqft + sqftXutown + age + pool + fplace + utown, data = data)

#Tabla regresión
summary(modelo5)

#lincom utown=0
summary(glht(modelo5, linfct = c("utown = 0")))

#Linear Test Hypotesis sqftXutown=0
summary(glht(modelo5, linfct = c("sqftXutown=0")))

#gen lprice
data$lprice <- log(data$price)

#Regresión múltiple con lprice, sqft, sqftXutown, age, pool, fplace, utown
modelo6 <- lm(lprice ~ sqft + sqftXutown + age + pool + fplace + utown, data = data)

#Tabla regresión
summary(modelo6)

#lincom utown+25*sqftXutown
summary(glht(modelo6, linfct = c("25*sqftXutown + utown = 0")))

#Calculo
(exp(0.247708)-1)*100
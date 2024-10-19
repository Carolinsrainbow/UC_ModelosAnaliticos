# Tutorial 4 --------------------------------------------------------------
# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
#install.packages("haven")
#install.packages("multcomp")
install.packages("car")
# Llamar librerias
library(haven)
library(multcomp)
library(car)


# 2. Cargar datos ---------------------------------------------------------
#Abrir base de dato
data <- read_dta("burger.dta")


# 3. Modelo de regresion multiple -----------------------------------------
#Regresión múltiple con sales, price, advert
modelo1 <- lm(sales ~ price + advert, data = data)

#Tabla regresión
summary(modelo1)

#Estat VCE
vcov(modelo1)

#Ajustar modelo a price = 5.5, advert = 1.2
predict(modelo1, data.frame(price=5.5, advert=1.2))

#lincom _cons+5.5*price+1.2*advert
summary(glht(modelo1, linfct = c("5.5*price + 1.2*advert + (Intercept)=0")))

#lincom -0.2*price-0.5*advert
summary(glht(modelo1, linfct = c("-0.2*price - 0.5*advert = 0")))

#lincom -0.4*price+0.8*advert
summary(glht(modelo1, linfct = c("-0.4*price + 0.8*advert = 0")))

# Crear variable advert2
data$advert2 <- data$advert * data$advert

#Regresión múltiple con sales, price, advert, advert2
modelo2 <- lm(sales ~ price + advert + advert2, data = data)

#Tabla regresión
summary(modelo2)

#Linear Test Hypotesis advert2=0, advert=0
linearHypothesis(modelo2, c("advert2=0", "advert=0"))

#Regresión simple con sales, price
modelo3 <- lm(sales ~ price, data = data)

#Tabla regresión
summary(modelo3)

#Tabla regresión
summary(modelo2)

#Linear Test Hypotesis advert2=0
linearHypothesis(modelo2, c("price=0", "advert2=0", "advert=0"))

#Regresión múltiple con sales, price, advert
modelo4 <- lm(sales ~ sales, data = data)

#Tabla regresión
summary(modelo4)
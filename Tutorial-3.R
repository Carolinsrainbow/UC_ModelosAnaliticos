# Tutorial 3 --------------------------------------------------------------
# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
#install.packages("haven")
install.packages("multcomp")
# Llamar librerias
library(haven)
library(multcomp)


# 2. Cargar datos ---------------------------------------------------------
#Abrir base de datos
data <- read_dta("burger.dta")


# 3. Explorar datos -------------------------------------------------------
#Estadísticos descriptivos
summary(data)


# 4. Regresion multiple  --------------------------------------------------
#Regresión múltiple con sales, price, advert
modelo1 <- lm(sales ~ price + advert, data = data)

#Tabla regresión
summary(modelo1)

#Ajustar modelo a price = 5.5, advert = 1.2
predict(modelo1, data.frame(price=5.5, advert=1.2))

#Tabla regresión
summary(modelo1)

#Estat VCE
vcov(modelo1)

#lincom price=0
summary(glht(modelo1, linfct = c("price = 0")))

#Generar logaritmos
data$lsales <- log(data$sales)
data$lprice <- log(data$price)
data$ladvert <- log(data$advert)

#Regresión múltiple con lsales, lprice, ladvert
modelo2 <- lm(lsales ~ lprice + ladvert, data = data)

#Tabla modelo
summary(modelo2)

#Linear Test Hypotesis lprice=1
summary(glht(modelo2, linfct = c("lprice = -1")))

#Regresión múltiple con sales, price, advert
summary(modelo1)

#Linear Test Hypotesis advert=1
summary(glht(modelo1, linfct = c("advert = 1")))

#Crear advert 2
data$advert2 <- data$advert * data$advert

#Regresión múltiple con sales, price, advert, advert2
modelo3 <- lm(sales ~ price + advert + advert2, data = data)

#Tabla regresión
summary(modelo3)

#Regresión múltiple con sales, price, advert, advert2
summary(modelo3)

#Nivel óptimo de advert
((1-12.15123)/(2*-2.767962))

#más regresiones multiples
summary(modelo2)

#tegresión múltiple con lsales, price, advert, advert2
modelo4 <- lm(lsales ~ lprice + advert + advert2, data = data)

#Tabla regresión
summary(modelo4)
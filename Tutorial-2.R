# Tutorial 2 --------------------------------------------------------------

# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
install.packages("haven")
install.packages("perfomance")
install.packages("car")
# Llamar librerias
library(haven)
library(performance)
library(car)


# 2. Cargar datos ---------------------------------------------------------
data <- read_dta("CEOSAL.dta")


# 3. Estimar modelo  ------------------------------------------------------
#Generar variable de logaritmo de salary
data$lsalary <- log(data$salary)

#Regresión lineal con variables lsalary y roe
modelo1 <- lm(lsalary ~ roe, data = data)

#Tabla regresión
summary(modelo1)

#Generar variable de logaritmo de sales
data$lsales <- log(data$sales)

#Realizar regresión múltiple con logaritmo de salario, roe, ros y logaritmo de sales
modelo2 <- lm(lsalary ~ roe + ros + lsales, data = data)

#Tabla regresión
summary(modelo2)

#Generar variable ceoten
set.seed(332)
data$ceoten <- round(runif(209, min=0, max=18), 0)

#Datos variable
summary(data$ceoten)

#Realizar regresión múltiple con logaritmo de salario, roe, ros, logaritmo de sales y ceoten
modelo3 <- lm(lsalary ~ roe + ros + lsales + ceoten, data = data)

#Tabla regresión
summary(modelo3)


# 4. Cargar nueva base de datos -------------------------------------------
data2 <- read.table("Auto.data", header = T, na.strings = "?")

#Realizar regresión con mpg y horsepower
modelo4 <- lm(mpg ~ horsepower, data = data2)

#Creamos variable horsepower^2
data2$horsepower2 <- (data2$horsepower^2)

#Realizar regresión con mpg, horsepower y horsepower^2
modelo5 <- lm(mpg ~ horsepower + horsepower2, data = data2)


# 5. Robustez y supuestos -------------------------------------------------
#Non-linearity of the response-predictor relationships para base Auto
check_model(modelo4, check = c("ncv", "linearity"))
check_model(modelo5, check = c("ncv", "linearity"))

#Crear modelo2 con salary
modelo6 <- lm(salary ~ roe + ros + lsales, data = data)

#Gráfico de los residuos (diapo 11)
check_model(modelo2, check = c("ncv", "linearity"))
check_model(modelo6, check = c("ncv", "linearity"))

#Non-constant variance of error terms (diapo 12)
check_heteroscedasticity(modelo5)
plot(check_heteroscedasticity(modelo5))

#Cargamos data credit (diapo 14)
data3 <- read.csv("Credit.csv")

#Crear modelo (diapo 14)
modelo7 <- lm(Balance ~ Age + Rating + Limit, data = data3)

#Tabla regresión
summary(modelo7)

#VIF
vif(modelo7)

#Colinearity
check_collinearity(modelo7)
plot(check_collinearity(modelo7))

#Outliers
check_outliers(modelo7)
plot(check_outliers(modelo7))

#Normalidad
plot(check_normality(modelo7))
plot(check_normality(modelo7), type = "qq")
plot(check_normality(modelo7), type = "pp")
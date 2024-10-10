# Tutorial 1 --------------------------------------------------------------

# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
install.packages("haven")
# Llamar librerias
library(haven)


# 2. Cargar datos ---------------------------------------------------------
data <- read_dta("CEOSAL.dta")


# 3. Explorar datos -------------------------------------------------------
#Ver datos 
View(data)

#Tabla general para 4 variables
summary(data)


# 4. Estimar modelo  ------------------------------------------------------
#Regresión lineal con variables salary y roe
modelo1 <- lm(salary ~ roe, data = data)

#Tabla regresión
summary(modelo1)


# Grafico -----------------------------------------------------------------
#Otra forma de realizar gráfico
attach(data)
plot(roe, salary)
abline(modelo1 ,lwd =3, col =" red ")

#Guardar gráfico
ggsave("figura1.png", plot = last_plot())

#Mostrar valores ajustados
head(predict(modelo1), n = 5)

#Ajustar modelo a roe 30
predict(modelo1, data.frame(roe=30))

# 5. Regresion multiple ---------------------------------------------------
#Regresión múltiple con salary, roe, ros, sales
modelo2 <- lm(salary ~ roe + ros + sales, data = data)

#Tabla Regresión
summary(modelo2)

#Ajustar modelo a roe 30
predict(modelo2, data.frame(roe=30, ros = mean(data$ros), sales = mean(data$sales)))

#Ajustar modelo a roe 31
predict(modelo2, data.frame(roe=31, ros = mean(data$ros), sales = mean(data$sales)))

#Ajustar modelo a roe 30
predict(modelo2, data.frame(roe=30, ros = mean(data$ros), sales = mean(data$sales)))

#Ajustar modelo a roe 35
predict(modelo2, data.frame(roe=35, ros = mean(data$ros), sales = mean(data$sales)))

#Ajustar modelo a roe 30, ros 50, sales 7000
predict(modelo2, data.frame(roe=30, ros=50, sales=7000))

#Ajustar modelo a roe 35, ros 50, sales 7100
predict(modelo2, data.frame(roe=35, ros=50, sales=7100))

#Generar variable de logaritmo de salary
data$lsalary <- log(data$salary)

#Realizar regresión simple con logaritmo de salario y roe
modelo3 <- lm(lsalary ~ roe, data = data)

#Tabla regresión
summary(modelo3)

#Generar variable de logaritmo de sales
data$lsales <- log(data$sales)

#Realizar regresión múltiple con logaritmo de salario, roe, ros y logaritmo de sales
modelo4 <- lm(lsalary ~ roe + ros + lsales, data = data)

#Tabla regresión
summary(modelo4)
# Tutorial 6 --------------------------------------------------------------
# 1. Cargar librerias -----------------------------------------------------
# Primero instalar (solo una vez)
install.packages("leaps")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")

# Llamar librerias
library(leaps) 
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# 2. Cargar datos ---------------------------------------------------------
#Abrir base de datos
data <- read.csv("Credit.csv")


# 3. Explorar datos -------------------------------------------------------
#Estadísticos descriptivos
summary(data)


# 4. Selección del mejor subconjunto --------------------------------------
#Observar mejores modelos según cantidad de variables
modelos <- regsubsets(Balance ~., data, nvmax = 11)

#Tabla modelos
sum.modelos <- summary(modelos)

#Observar alza de R2
sum.modelos$rsq

#Observar alza de R2 ajustado
sum.modelos$adjr2

#Elegir mejor modelo según datos estadísticos
#Gráfico para mayor adjr2
which.max(sum.modelos$adjr2)
plot(sum.modelos$adjr2, xlab= "Número de Variables", ylab = "RSq")
points(7, sum.modelos$adjr2[7], col = "red", cex = 2, pch =20)

#Gráfico para menor CP
which.min(sum.modelos$cp)
plot(sum.modelos$cp, xlab= "Número de Variables", ylab = "CP")
points(6, sum.modelos$cp[6], col = "red", cex = 2, pch =20)

#Creamos modelo con menor CP para extraer el valor de AIC
modelo1 <- lm(Balance ~ Income + Limit + Rating + Cards + Age + Student, data = data)
AIC(modelo1)

#Gráfico para menor BIC
which.min(sum.modelos$bic)
plot(sum.modelos$bic, xlab= "Número de Variables", ylab = "BIC")
points(4, sum.modelos$bic[4], col = "red", cex = 2, pch =20)

#Calcular coeficientes mejor modelo (en este caso se eligió el con 7 variables (mejor R2))
coef(modelos, 4)

# 5. Selección escalonada hacia adelante ----------------------------------
modelo.adelante <- regsubsets(Balance ~., data = data, nvmax = 11, method = "forward")

#Tabla regresión
summary(modelo.adelante)

# 6. Selección escalonada hacia atras -------------------------------------
modelo.atras <- regsubsets(Balance ~., data = data, nvmax = 11, method = "backward")

#Tabla regresión
summary(modelo.atras)


# 7. Métodos de reducción: Ridge ------------------------------------------
#Definir variable responsable
y <- data$Balance

#Definir matrix con las variables predictoras
x <- model.matrix(Balance ~., data)[,-1]


# 7.1 Separación data en test y train --------------------------------------
#Separamos data en data de entrenamiento y test para estimar error de los métodos
#Seteamos semilla
set.seed(333)

#Creamos train
train <- sample(1:nrow(x), nrow(x)/2)

#Creamos test
test <- (-train)

#Creamos y.test
y.test <- y[test]

# 7. Métodos de reducción: Ridge ------------------------------------------
# Para obtener un ajuste con regularización Ridge se indica argumento alpha=0.
# Si no se especifica valor de lambda, se selecciona un rango automático.
ridge1 <- glmnet(
  x           = x,
  y           = y,
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
regularizacion <- ridge1$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = ridge1$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw()

#Cross validation para calcular mejor lambda
cv <- cv.glmnet(x[train,], y[train], alpha=0)

#Calculamos bestlambda
bestlambda <- cv$lambda.min
bestlambda

#Predcimos mejor modelo
ridge2 <- glmnet(x, y, alpha=0, lambda = bestlambda)
predict(ridge2, type="coefficients", s=bestlambda)

# 9. Métodos de reducción: Lasso ------------------------------------------
# Para obtener un ajuste con regularización Ridge se indica argumento alpha=0.
# Si no se especifica valor de lambda, se selecciona un rango automático.
lasso1 <- glmnet(
  x           = x,
  y           = y,
  alpha       = 0,
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
regularizacion2 <- lasso1$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = lasso1$lambda)

regularizacion2 <- regularizacion2 %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion2 %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw()

#Cross validation para calcular mejor lambda
cv2 <- cv.glmnet(x[train,], y[train], alpha=1)

#Calculamos bestlambda
bestlambda2 <- cv2$lambda.min
bestlambda2
  
#Predecimos mejor modelo
lasso2 <- glmnet(x, y, alpha=1, lambda = bestlambda2)
predict(lasso2, type="coefficients", s=bestlambda2)

#Calculamos modelo con valor lambda=159 para observar como los coeficientes resultan iguales a 0
lasso3 <- glmnet(x, y, alpha=1, lambda = 159)
predict(lasso3, type="coefficients")

#Modelo con todas las variables para comparación
modelofull <- lm(Balance ~., data)
summary(modelofull)
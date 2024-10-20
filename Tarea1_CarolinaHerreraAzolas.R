# Caso Beta

#Beta Products es una empresa que vende una amplia gama de productos y sus ventas anuales son de aproximadamente US$ 2 billones. Peter Cardan es el gerente de marca de un nuevo producto que consiste en agua embotellada, su nombre es Cayuga. Cayuga está posicionada en el mercado como un producto doméstico competidor de Perrier, el líder en la industria. Por su parte, Perrier está posicionada como una bebida de consumo general, y no exclusivamente para el consumo doméstico. Perrier ha aumentado el gasto en publicidad de MUS$ 819 a MUS$ 5.000 en los últimos años. El gasto actual de la industria en publicidad es de MUS$ 15.000 y las ventas minoristas de la industria son aproximadamente MMUS$ 300. 

#Cayuga es vendida actualmente en 22 mercados domésticos y los ejecutivos están pensando seriamente entrar en nuevos mercados. Para tomar la decisión de qué mercados son los más atractivos, los ejecutivos de Cayuga piensan en construir un modelo que les permita predecir la demanda en los nuevos mercados potenciales. Las variables disponibles para los mercados en los cuales se vende Cayuga son las siguientes:
  
# Ventas en dólares (US$).
# Ventas en unidades.
# Población. 
# Precio Cayuga por unidad.
# Precio Perrier por unidad.
# Gasto en publicidad de Perrier en miles de dólares (MUS$).
# Gasto en publicidad de Cayuga en miles de dólares (MUS$).
# Ingreso promedio de los hogares.
# Años promedio de educación formal de la población adulta.

# Peter Cardan considera los siguientes modelos de demanda para Cayuga. Considera todas las variables en los modelos con transformación logaritmo natural. 

# Modelo 1: Ventas en unidades con los siguientes regresores: Población, Precio Cayuga, Gasto en publicidad Cayuga, Ingreso promedio y Años de educación.

# Modelo 2: Ventas en unidades con los siguientes regresores: Población, Precio Cayuga, Precio Perrier, Publicidad Cayuga, Publicidad Perrier.

# Modelo 3: Igual al anterior pero con la variable Ingreso promedio.

# 3. Utilizando la información anterior y los datos que se presentan en el siguiente archivo, responde según lo indicado.

# En base al Modelo 3 (no necesariamente el mejor), ¿qué mercado(s) recomendaría usted? Ayuda: revise la sección 6.4 del texto de Wooldridge para ver cómo obtener una predicción para cuando la variable dependiente es . Además, suponga que los errores siguen una distribución normal. 


#DESARROLLO CASO

# 1. Cargar librerias -----------------------------------------------------

# Instalación de librerías 

install.packages("haven")
install.packages("multcomp")
install.packages("car")
install.packages("readxl")  
install.packages("lmtest")  
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("broom") 

# Carga de librerias

library(readxl)
library(lmtest)
library(dplyr)
library(ggplot2)
library(broom)
library(haven)
library(multcomp)
library(car)

# 2. Carga de datos

data <- read_excel("/Users/carolinaherreraazolas/Downloads/Caso-Beta.xlsx")
print(data)

# 3. Transformación a Logaritmo de las variables que usaremos

adap_data <- data %>%
  mutate(
    nuevo_Ventas = log(Ventas),
    nuevo_Población = log(Población),
    nuevo_Precio_Cayuga = log(`Precio Cayuga`),
    nuevo_Publicidad_Cayuga = log(`Publicidad C`),
    nuevo_Ingreso = log(Ingreso),
    nuevo_Educación = log(`Educación`), 
    nuevo_Precio_Perrier = log(`Precio Perrier`),
    nuevo_Publicidad_Perrier = log(`Publicidad P`)
  )
 print(adap_data)
 
 # 4. Modelos de demanda (M1, M2 & M3)
 
 # Modelo 1
 modelo_1 <- lm(nuevo_Ventas ~ nuevo_Población + nuevo_Precio_Cayuga + nuevo_Publicidad_Cayuga + nuevo_Ingreso + nuevo_Educación, data = adap_data)
 summary(modelo_1)
 
 # Modelo 2
 modelo_2 <- lm(nuevo_Ventas ~ nuevo_Población + nuevo_Precio_Cayuga + nuevo_Precio_Perrier + nuevo_Publicidad_Cayuga + nuevo_Publicidad_Perrier, data = adap_data)
 summary(modelo_2)
 
 # Modelo 3
 modelo_3 <- lm(nuevo_Ventas ~ nuevo_Población + nuevo_Precio_Cayuga + nuevo_Precio_Perrier + nuevo_Publicidad_Cayuga + nuevo_Publicidad_Perrier + nuevo_Ingreso, data = adap_data)
 summary(modelo_3)
 
 # 5. Pregunta 3
  precio_cayuga_promedio <- mean(data$`Precio Cayuga`, na.rm = TRUE)
 costo_variable <- 1.887
 predicciones_log <- predict(modelo_3, newdata = adap_data)
 predicciones_ventas <- exp(predicciones_log)
  contribucion <- (precio_cayuga_promedio - costo_variable) * predicciones_ventas
 
 adap_data <- adap_data %>%
   mutate(
     Ventas_Predichas = predicciones_ventas,
     Contribucion = contribucion
   )
 
 contribucion_total <- sum(contribucion)
  print(adap_data %>% select(nuevo_Ventas, Ventas_Predichas, Contribucion))
 cat("Contribución total del producto Cayuga:", round(contribucion_total, 2), "MUS$\n")
 
 #6. Pregunta 4
 

 # Precio promedio de Cayuga y Publicidad de Cayuga en los mercados actuales
 precio_cayuga_promedio <- mean(adap_data$`Precio Cayuga`, na.rm = TRUE)
 publicidad_cayuga_promedio <- mean(adap_data$`Publicidad C`, na.rm = TRUE)
 
 # Costo variable por unidad (MUS$ 1.887)
 costo_variable <- 1.887
 
 # Crear un dataframe para los nuevos mercados
 nuevos_mercados <- data.frame(
   nuevo_Población = c(log(953), log(2621), log(559), log(308)),
   nuevo_Ingreso = c(log(13.9), log(14.4), log(12.7), log(16.8)),
   nuevo_Precio_Cayuga = precio_cayuga_promedio,
   nuevo_Precio_Perrier = c(log(1.62), log(1.75), log(1.55), log(1.80)),
   nuevo_Publicidad_Cayuga = publicidad_cayuga_promedio,
   nuevo_Publicidad_Perrier = c(log(70), log(150), log(50), log(160))
 )
 
 # Hacer la predicción de ventas utilizando el Modelo 3
 predicciones_log <- predict(modelo_3, newdata = nuevos_mercados)
 
 # Convertir las predicciones de log(ventas) a ventas reales
 predicciones_ventas <- exp(predicciones_log)
 
 # Calcular la contribución para cada mercado
 contribucion <- (precio_cayuga_promedio - costo_variable) * predicciones_ventas
 
 # Mostrar las contribuciones por mercado
 data.frame(Mercado = 1:4, Ventas_Predichas = predicciones_ventas, Contribucion = contribucion)
 contribucion 

 
 
 # PREGUNTAS 
 

#---------------------------------------PREGUNTA 1. 
# Estime los tres modelos propuestos, y luego explique a su juicio cuál es el mejor. Justifique su respuesta.
 
# Respuesta 1:
# Dada la implementación de los modelos, desde la base podemos concluir que para el modelo 2 se encuentran los atributos que afectan directamente la demanda
# Esto fue respaldado por:
# R-cuadrado ajustado superior en el valor  0.9482  vs    0.9239(M1) & 0.9474 (M3), de lo cual deprendemos que explica de mejor manera la variabilidad de las ventas.
# Significancia de las variables: 
# Las variables que presentan un impacto estadístico para este modelo son:  Población, Publicidad de Cayuga y Publicidad de Perrier son significativas al valor p. 

 #--------------------------------------- PREGUNTA 2. Interprete los resultados del modelo elegido en (a)
 # Respuesta 2: Interpretación 
 
 #Población: 
 

 #---------------------------------------  PREGUNTA 3. Considerando costos variables de MUS$ 1.887, calcule la rentabilidad del nuevo producto de Beta. ¿Qué opinión le merecen estos resultados? Discuta.
# Respuesta 3:
# Contribución = La sumatoria total de los costos vs ingresos del producto no son lo suficiente para un punto equilibrio, esto puede permitirse en un escenario de lanzamiento, sin embargo,
# es requerido un cambio de estrategia en cuanto a la publicidad, nuevos mercados, y reducción de costos para lograr rentabilizar el producto / proyecto.
 
 #---------------------------------------  PREGUNTA 4. Los ejecutivos de Beta quieren usar un modelo para decidir a qué nuevo mercado ingresar.El Cuadro 1 muestra los cuatro mercados alternativos que se están evaluando. Suponga
# que para estos mercados se consideran los siguientes supuestos:
 
 
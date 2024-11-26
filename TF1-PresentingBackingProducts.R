# Caso 1: PRESENTING BANKING PRODUCTS

# 1. Preparación 

# Instalación de librerías 
install.packages("leaps")
install.packages("glmnet")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("scales")
install.packages("ROCR")
install.packages("readxl")  


# Llamar librerias
library(leaps) 
library(glmnet)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(ROCR)
library(readxl)

# 2. Cargar datos ---------------------------------------------------------

archivo_base <- "/Users/carolinaherreraazolas/Downloads/Presenting_Banking_Products.xlsx"

data <- read_excel(archivo_base)  # Lo usé sólo para ver como venían los datos
print(data)

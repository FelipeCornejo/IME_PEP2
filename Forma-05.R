# Authors:
#           Felipe Cornejo I. | 20.427.782-6
#           Miguel Salinas G. | 20.215.515-4

#PEP 2 IME - 2021-2

#PREGUNTA 1.

#Importación de bibliotecas
library(dplyr)
library (tidyverse)
library (ggpubr)
library (ez)

# Se obtienen los datos del archivo csv, windows separa por ";".
data <- read.csv2(choose.files(), sep=";")

alfa <- 0.01

filtro_w <- data %>% filter(division == "Cavetrooper")

# Luego de obtener los datos filtrados por la division Cavetrooper se procede a desarrollar la hipotesis.

# Lo que pide el gran Lord Sith es revisar si existen diferencias significativas en el promedio de las evaluaciones de los soldados.
# Por ende tomando cada variable a estudiar, se obtiene la siguiente hipotesis.

# H0 : No existen diferencias significativas entre el promedio de las evaluaciones.
# HA : Existe por lo menos un promedio que difiere de los demás.

instructor <- data[["eval_instructor"]]
comandante <- data[["eval_comandante"]]
capitan <- data[["eval_capitan"]]
general <- data[["eval_general"]]

datos2 <- data %>% select(id,eval_instructor, eval_capitan,eval_comandante,eval_general)
data2 <- data %>% pivot_longer(c("eval_instructor","eval_capitan","eval_comandante","eval_general"),
                               names_to = "Evaluador",
                               values_to = "Puntaje")
data2[["ID"]] <-  factor (1: nrow ( data2 ) )



# Comprobación de normalidad utilizando Shapiro Test.


shapiro1 <- shapiro.test(data[["eval_instructor"]])
print(shapiro1)
# p-value = 0.0141
shapiro2 <- shapiro.test(data[["eval_capitan"]])
print(shapiro2)
# p-value = 0.0487
shapiro3 <- shapiro.test(data[["eval_comandante"]])
print(shapiro3)
# p-value = 0.01863
shapiro4 <- shapiro.test(data[["eval_general"]])
print(shapiro4)
# p-value = 0.008721

g <- ggqqplot(data, x ="eval_general")
print(g)

#Las primeras 3 evaluaciones testeadas se obtiene un p-valor > alfa, lo cual se puede afirmar que para ellas, con un 99% de confianza, que siguen una distribución normal
#La evaluación del general, se escapa del valor alfa, pero no demasiado más. Evaluando su gráfico Q-Q se puede ver que los datos atípicos no se alejan tanto de la curva de normalidad.
# Por ende se seguirá, manteniendo en cuenta solo este detalle.

# Se verifican las condiciones para utilizar ANOVA:

# - Se puede suponer que las muestras son obtenidas de manera aleatoria e independiente.
# - La escala con la que se miden las evaluaciones (numérica dentro del rango de [550, 750]), tiene 
#   las propiedades de una escala de intervalos iguales.
# - Se puede suponer razonablemente que la población de origen sigue una distribución
#   normal, la cual se puede observar por medio del Shapiro test, se debe tener en cuenta
#   que existen algunos valores que pueden ser atípicos, además se tiene que las muestras 
#   son relativamente grandes, por lo que se utiliza un nivel de significación 
#   igual a alfa = 0,01.
# - Las muestras tienen varianzas aproximadamente iguales, se comprueba al proceder
#   con la función leveneTest() la cual realiza la prueba de homocedasticidad.
#   Además, al realizar el procedimiento de ANOVA con ezANOVA(), esta incluye
#   dicha prueba.

# Prueba de homocedasticidad de Levene.
leveneTest()

varInst =  var(instructor)
varCom =  var(comandante)
varCap = var(capitan)
varGen = var(general)
# H0: Las varianzas de las muestras son iguales.
# HA: Existe por lo menos una varianza que difiere de las demás.

pruebaANOV <- ezANOVA ( data = data2 , dv = Puntaje , between = Evaluador,
                       wid = ID , return_aov = TRUE )
print(pruebaANOV)

g2 <- ezPlot(data = data2, dv= Puntaje, wid = ID, between = Evaluador, y_lab = "Evaluacion de Evaluador",
             x = Evaluador)
print(g2)

#Se puede apreciar que el test arroja un p valor equivalente a 
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
library (DescTools)

# Se obtienen los datos del archivo csv, windows separa por ";".
data <- read.csv2(choose.files(), sep=";")

alfa <- 0.01

filtro_w <- data %>% filter(division == "Cavetrooper")

set.seed(7782) # Se setea una seed para la obtencion de muestras. ultimos 4 digitos de rut de felipe cornejo.

# Luego de obtener los datos filtrados por la division Cavetrooper se procede a desarrollar la hipotesis.

# Lo que pide el gran Lord Sith es revisar si existen diferencias significativas en el promedio de las evaluaciones de los soldados.
# Por ende tomando cada variable a estudiar, se obtiene la siguiente hipotesis.

# H0 : No existen diferencias significativas entre el promedio de las evaluaciones.
# HA : Existe por lo menos un promedio que difiere de los demás.

#Se realizar� con una muestra de 40 Cavetroopers los siguientes test.
muestra <- filtro_w[sample(nrow(filtro_w), 40),]

muestra2 <- muestra %>% pivot_longer(c("eval_instructor","eval_capitan","eval_comandante","eval_general"),
                               names_to = "Evaluador",
                               values_to = "Puntaje")
muestra2[["ID"]] <-  factor (1: nrow ( muestra2 ) )



# Comprobación de normalidad utilizando Shapiro Test.


shapiro1 <- shapiro.test(muestra[["eval_instructor"]])
print(shapiro1)
# p-value = 0.4172
shapiro2 <- shapiro.test(muestra[["eval_capitan"]])
print(shapiro2)
# p-value = 0.3488
shapiro3 <- shapiro.test(muestra[["eval_comandante"]])
print(shapiro3)
# p-value =0.03497
shapiro4 <- shapiro.test(muestra[["eval_general"]])
print(shapiro4)
# p-value = 0.1232

g <- ggqqplot(muestra2, x ="eval_general")
print(g)

#Las evaluaciones testeadas se obtiene un p-valor > alfa, lo cual se puede afirmar que para ellas, con un 99% de confianza, que siguen una distribución normal

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


pruebaANOV <- ezANOVA ( data = muestra2 , dv = Puntaje , between = Evaluador,
                       wid = ID , return_aov = TRUE )
print(pruebaANOV)

g2 <- ezPlot(data = muestra2, dv= Puntaje, wid = ID, between = Evaluador, y_lab = "Evaluacion de Evaluador",
             x = Evaluador)
print(g2)

# Se puede apreciar que el test arroja un p valor equivalente a 3.072127e-17
# Lo cual significa que se rechaza la hipotesis nula a favor de la alternativa con una seguridad de 99%.

# Por ende se procede a realizar un testeo post-hoc para que vader pueda castigar a el evaluador en cuesti�n.
# Por lo tanto se realizar� una prueba de comparaci�n SCHEFF� ya que tiene la ventaja de obtener el resultado distinto.
anova <- aov( Puntaje ~ Evaluador , data = muestra2 )

posth <- TukeyHSD(anova, "Evaluador", ordered = TRUE, conf.level = 1 - alfa)
print(posth)

# Se puede apreciar que la prueba pos-hoc, junto con el gr�fico otorgado por ezPlot da a mostrar que la variable independiente
# el cual se aleja totalmente de las otras medias es la evaluacion del instructor.


## PREGUNTA 2

# Se realiza la lectura de los datos.
data <- read.csv2(choose.files(), sep=";")

set.seed(1028)
#Se muestrea a 40 chicos
n <- nrow (data)
n_entrenamiento <- floor (0.8*n)
muestra <- sample.int (n = n, size = n_entrenamiento , replace = FALSE )

entrenamiento <- data[muestra, ]
prueba <- data[-muestra, ]

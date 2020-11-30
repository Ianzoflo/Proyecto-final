
library(dagitty)
library(ggdag)


#########################################
# crear DAG


g <- dagitty("dag{
x -> y ; z -> y
x [exposure]
y [outcome]
z [unobserved]
}")

#graficar dag 

ggdag(g, layout = "circle") + theme_dag()


###################################################

####### Cargar archivo de datos

library(readr)
setwd("C:/Users/MASTER/Desktop/Proyecto_finalP/Proyecto-final/R") 
Datos_caribe <- read_delim("Datos_caribe.csv", 
                           ";", escape_double = FALSE, col_types = cols(Especie = col_factor(levels = c("Ficus_colubrinae", 
                                                                                                        "Ectophylla_alba")), LatitudDecimal = col_number(), 
                                                                        LongitudDecimal = col_number()), 
                           trim_ws = TRUE)
View(Datos_caribe)

#######

####### Supuestos param?tricos

# Prueba de normalidad

coo <- c(Datos_caribe$LatitudDecimal, Datos_caribe$LongitudDecimal)

area.test <- shapiro.test(coo) # Prueba Shapiro para normalidad
print(area.test) # Distribucion no normal p>

plotn <- function(x,main="Histograma de frecuencias y distribucion normal",
                  xlab="Especie",ylab="Variabilidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
} ## Grafico distribucion

plotn(coo,main="Distribucion normal") # Graficamente distribucion es normal, pero la  Prueba Shapirolo niega

# Prueba de homogeneidad de varianzas

bartlett.test(Datos_caribe$LatitudDecimal ~ Datos_caribe$Especie)  # Las varianzas son homogeneas
bartlett.test(Datos_caribe$LongitudDecimal ~ Datos_caribe$Especie)  # Las varianzas son homogeneas 

# Homocedasticidad

library(car) # Para homocedasticidad
leveneTest(Datos_caribe$LatitudDecimal ~ Datos_caribe$Especie) # Las variaciones no son equivalentes
leveneTest(Datos_caribe$LongitudDecimal ~ Datos_caribe$Especie) # Las variaciones no son equivalentes

# No se pueden realizar pruebas paramatricas al incumplir supuestos de normalidad y varianzas.



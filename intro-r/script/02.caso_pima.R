##os Pima son un grupo de nativos de Arizona. En los ultimos anios se observo un aumento en la prevalencia de diabetes
#tipo 2, asociado a cambios en la dieta y a una disminucion de la actividad fisica, por lo que han sido objeto de 
#muchos estudios. 
#La base de datos pertenece al Instituto Nacional de Diabetes y Enfermedades Digestivas y Renales. 768 mujeres adultas 
#El objetivo del conjunto de datos es predecir si una paciente tiene diabetes o no, basandose en ciertas mediciones de 
#diagnostico incluidas en el conjunto de datos. 

#Describa la base de datos en forma univariada mediante graficos y estadisticos para las variables cuantitativas y 
#tablas de frecuencias para las cualitativas

#Para las cuanti:

#1. Cual es la tendencia central de las variables y su rango?

library(faraway)

db <- faraway::pima
pima <- db

#Cambio los ceros por NA

pima$glucose[pima$glucose == 0] <- NA
pima$diastolic[pima$diastolic == 0] <- NA 
pima$triceps[pima$triceps == 0] <- NA 
pima$insulin[pima$insulin == 0] <- NA 
pima$bmi[pima$bmi == 0] <- NA 

#Clasifique a las pacientes segC:n su peso en normales, con sobrepeso o con 
#obesidad
pima$cat_peso <- ifelse(pima$bm < 25, "Normal",
                        ifelse(pima$bm >= 25 & pima$bm < 30, "Con sobrepeso",
                               "Obesidad"))
#Genere la variable diabetes a partir de test (si = 1, no=0)

pima$result <- ifelse(pima$test == 1, "si","no")

colnames(pima)[7]<-"diabetes_gen" 

sapply(pima, class)

pima$pregnant = as.numeric(pima$pregnant)
pima$cat_peso = as.factor(pima$cat_peso)
pima$result = as.factor(pima$result)

summary(pima)

#2. Que variables presentan mayor variabilidad?

library(pastecs)

stat.desc(pima)

#3. Que tipo de asimetria presentan las variables?

getmode<-function(x){
  return(as.numeric(rownames(data.frame(which.max(table(x))))))
}
layout(matrix(nrow = 2,ncol = 4,data = c(1:8)))
for (var in colnames(pima)[c(1:8)]){
  hist(pima[,var],main = var)
  #mean in red, median in blue, mode in green
  abline(v=mean(na.omit(pima[,var])),col="red")
  abline(v=median(na.omit(pima[,var])),col="blue")
  abline(v=getmode(na.omit(pima[,var])),col="green")  
  
}  

getmode<-function(x){
  return(as.numeric(rownames(data.frame(which.max(table(x))))))
}
layout(matrix(nrow = 2,ncol = 4,data = c(1:8)))
for (var in colnames(pima)[c(1:8)]){
  plot(density(na.omit(pima[,var])),main = var)
  #mean in red, median in blue, mode in green
  abline(v=mean(na.omit(pima[,var])),col="red")
  abline(v=median(na.omit(pima[,var])),col="blue")
  abline(v=getmode(na.omit(pima[,var])),col="green")
}


#4. Hay valores atipicos?

layout(matrix(nrow = 2,ncol = 4,data = c(1:8)))
for (var in colnames(pima)[c(1:8)]){
  boxplot(pima[,var],main=var)
}


#Para las cuali:
#  1. Cuales son las categoricas mas frecuentes? En que porcentaje?
#  

prop.table(table(pima$cat_peso))

prop.table(table(pima$result))

#Lo mas comun es obesidad. El 62% de las mujeres de la muestra son obesas
#El 86% presenta sobrepeso u obesidad#

#El 35% presenta diabetes


#**BIVARIADA**
#1. Describa la base de datos en forma bivariada mediante graficos y 
#estadisticos

library(GGally)
ggpairs(na.omit(pima),c(1:8)) 

#Se decide descartar la observacion de triceps mas alta porq consideramos que es un error de carga

pima$triceps[pima$triceps == 99] <- NA

#Las asociaciones mas fuertes son: bmi y triceps, nro embarazos y edad, glucosa e insulina. Todas asociaciones moderadas (r>0.33) y positivas


ggpairs(pima,columns = c(4,6))


#Cualitativas sin NA 

library(GGally)
ggpairs(na.omit(pima),columns = c(10,11))

#Cualitativa~Cuantitativa 

ggpairs(na.omit(pima),columns=c(2,5,6,10,11))

#2. Cual es el porcentaje de mujeres con sobrepeso y obesidad entre las 
#que tienen o no diabetes?

table(pima$cat_peso, pima$result)

prop.table(table(pima$cat_peso, pima$result)) ##100% esta en toda la tabla

#El 13% de todas las mujeres es normal y no diab
#El 33% de todas las mujeres es obesa y no diab
#El 18% de todas las mujeres es normal y diab
#...

prop.table(table(pima$cat_peso, pima$result),1) ##100% esta por fila

#El 93% de las normales no son diabeticas


prop.table(table(pima$cat_peso, pima$result),2) ##100% esta por columnas

#El 20% de las no diab tienen peso normal
#El 3% de las diab tienen peso normal

prop.table(table( pima[which(pima$cat_peso!="normales"), 10:11 ] ))

library(dplyr)
library(ggpubr)
library(ggplot2)
library(gridExtra)

datos1 <- data.frame(table(pima$cat_peso, pima$result))
colnames(datos1) <- c("Cat_Peso", "Result", "Freq")
datos1$Cat_Peso <- factor(datos1$Cat_Peso, levels = rev(c("Normal", "Con sobrepeso", "Obesidad")))
datos1$Result <- factor(datos1$Result, levels = rev(c("no", "si")))

# Use dplyr filter to calculate Porc
datos1 <- datos1 %>%
  group_by(Result) %>%
  mutate(Porc = round(Freq / sum(Freq) * 100, 0))

plot1 <- ggbarplot(datos1,x="Result",y="Freq",fill="Cat_Peso", 
                   palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                   xlab = "diagnostico",ylab="Cantidad de pacientes",label = T,  width=0.5,
                   title = "Frequency",lab.hjust = T)

plot2 <- ggbarplot(datos1,x="Result",y="Porc",fill="Cat_Peso",
                   palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                   xlab = "diagnostico",ylab="Cantidad de pacientes",label = T,  width=0.5,
                   title = "Percentage")

grid.arrange(plot1, plot2, ncol = 2 )

#3. Compare las variables cuantitativas entre pacientes con o sin diabetes 
#segun tendencia central y variabilidad


library(arsenal)

tab1 <- tableby(result~glucose + insulin +pregnant+triceps+age+bmi+diabetes_gen, data =pima)
summary(tab1)

#En las pacientes diabeticas la glucosa, la insulina, edad, bmi, ???, son en promedio significativamt mayores que en las no diabeticas
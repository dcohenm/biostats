## DescripciC3n General
##Este documento proporciona una introducciC3n al manejo de datos con R. A lo largo del tutorial, exploraremos diferentes aspectos del manejo de datos utilizando ejemplos prC!cticos.


##Los siguientes datos indican el contenido de nitrC3geno obtenido en 4 lagunas pampeanas el mes pasado (en B5g/L):
  
##  5051.2, 6193.6, 3684.8 4928

#1. Construya el vector con los datos, asignC!ndole el nombre nitro

nitro <- c(5051.2, 6193.6, 3684.8, 4928)


##2. Verifique que el tipo de datos ingresados sea numC)rico.

class(nitro) 

#3. Verifique que la cantidad de datos ingresados sea la correcta

length(nitro)

#4. Visualice el vector

nitro

#5. Se desea expresar el contenido de nitrC3geno en mg/L. Genere el vector 
#correspondiente y denomC-nelo Nmg, para esto dividimos el vector por 1000 con 
#el siguiente script


Nmg <- nitro/1000
Nmg

#6. Se quiere identificar cada laguna con un nC:mero secuencial (de 1 a 4). 
#Genere el vector correspondiente, nombrC!ndolo Id

id <- c(1:4)
id

#7. Los datos correspondieron a las lagunas ChascomC:s, Chis-Chis, El Burro y 
#Adela respectivamente. Genere el vector laguna

laguna <- c("Chascomus", "Chris-Chris","El Burro","Adela")
laguna

#8. Construya el dataframe bd con el Id, el nombre de la laguna y el contenido 
#de N en mg/L uniendo los vectores.

bd <- data.frame(id,laguna,Nmg)
bd

#9. Calcule el promedio del contenido de nitrC3geno

promedio <- mean(bd$Nmg)
promedio

##Utilizando el dataframe bd
  
#10. Seleccione el contenido de N correspondiente a la laguna Chis-Chis y Adela

chris = subset(bd, laguna == "Chris-Chris" )
chris

#11. Seleccione las lagunas con contenido de N superior a 5 mg/L

n_5 = subset(bd, Nmg > 5) 
n_5

#12. Seleccione los datos correspondientes a la laguna Adela

Adela = subset(bd, laguna == "Adela" )
Adela

#Instale el paquete faraway
#install.packages(faraway)
library(faraway)

#13. Explore la base de datos pima contenida en dicho paquete.

# Cargamos la base de datos 'pima' contenida en el paquete 'faraway'.
pima <- faraway::pima
head(pima) 

#Cuales y cuantas variables posee?

ncol(pima) # Numero de variables
names(pima) # Nombres de las columnas

# Cuales y de que tipo son?

sapply(pima, class) 

# Cuantos casos?
nrow(pima) # Numero de casos

# Hay datos faltantes?
colSums(is.na(pima)) #suma los datos faltantes de cada columna 

#14. Para que se utiliza la funciCo3n summary(bd)? Apliquela

#La funciC3n summary() en R se utiliza para obtener un resumen estad??scico
#de un objeto, como un dataframe, una matriz o un vector. El resumen proporciona
#informaci??n util sobre las estad??sticas descriptivas de los datos en el objeto.
#El propC3sito principal de summary() es proporcionar un vistazo rC!pido a los
#datos, especialmente cuando trabajas con conjuntos de datos grandes o complejos

summary(pima)

#15. Analice el rango de las variables. B?Detecta alguna inconsistencia?
  
#Las variables glucose, diastolic, triceps, insulin y bmi tienen casos con el valor 0, sabemo
#que eso no es posible, vamos a reemplazar esos valores por NA.

#Cambio los ceros por NA

pima$glucose[pima$glucose == 0] <- NA
pima$diastolic[pima$diastolic == 0] <- NA 
pima$triceps[pima$triceps == 0] <- NA 
pima$insulin[pima$insulin == 0] <- NA 
pima$bmi[pima$bmi == 0] <- NA 




#16. El C-ndice de masa corporal (bmi) es el peso de una persona (en kg) dividido por el 
#cuadrado de la altura (en m). Si bmi estC! entre 25 a <30 se considera con 
#sobrepeso. Si su IMC es 30.0 o superior, obesidad. Genere la variable cat_peso. 
#Clasifique a las pacientes segC:n su peso en normales, con sobrepeso o con 
#obesidad. Ayuda: explore la funciC3n ifelse

pima$cat_peso <- ifelse(pima$bm < 25, "Normal",
                        ifelse(pima$bm >= 25 & pima$bm < 30, "Con sobrepeso",
                               "Obesidad"))

#17. Genere la variable diabetes a partir de test (si = 1, no=0)

pima$result <- ifelse(pima$test == 1, "si","no")

colnames(pima)[7]<-"diabetes_gen" 

head(pima)



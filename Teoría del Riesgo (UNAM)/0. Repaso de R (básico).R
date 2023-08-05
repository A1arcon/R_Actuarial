
# Repaso de R -------------------------------------------------------------


#Operadores lógicos
y <- c(TRUE, FALSE)
names(y) <- as.character(y)

#Y
outer(y,y,"|")

#O
outer(y,y,"&")

#O exluyente
outer(y,y,"xor")

#En
c(-1,0,1,2,30) %in% 1:10

#Dato curioso
T == TRUE
F == FALSE
T == c(0, 1, 2)
F == c(0, 1, 2)

##Detalle importante##

#Guarda nuevas entradas
vector<-1:5
vector[6]<-6
vector

#No muestres entradas
vector[-6]

#Modifica entradas
vector[c(1,3,5)]<-"impar"
vector

vector[c(F,T)]<-"par"
vector

##Mini-Ejercicio:
#Suponga que usted es el administrador de una cadena de restaurantes,
#a su empresa le surten cierto número de aves y ganado (supongamos pollos y vacas).
#En la última entrega a usted le informaron que fueron entregadas 1442 patas y 
#540 cabezas, sin especificar la especie del animal. Obtenga el número de pollos y vacas.

A <- matrix(c(1,2,1,4),2)
colnames(A) <- c("Pollos", "Vacas")
rownames(A) <- c("Cabezas","Patas")
A

b <- c(540,1442)
names(b) <- c("Cabezas","Patas")
b

X<-solve(A,b)
X

barplot(X,main="Resultado")

#Comprobación
X[1] + X[2] #Cabezas
2*X[1] + 4*X[2] #Patas


#Sexo
gen<-c("F","F","M","F","M")
gen

genf<-factor(gen)
genf

#Ojo con el detalle:
levels(genf) <- rev(c("Femenino", "Masculino"))
genf

levels(genf) <- c("Mujer", "Hombre")
genf

summary(1:10)

summary(genf)

barplot(summary(genf))

#Podemos ver elementos de los factores
genf[1] ; genf[2] ; genf[3] #; etc.
class(genf[1])

#Mujer
Mujer <- genf[1]

#Hombre
Hombre <- genf[3]

#¡Batalla de sexos! ¿Quién vale más?
Mujer > Hombre  

#¿Sexo ordenado?
genf<-ordered(genf,levels=c("Hombre","Mujer"))
genf

#Mujer
Mujer <- genf[1]

#Hombre
Hombre <- genf[3]

#Ahora sí, ¡Batalla de sexos! ¿Quién vale más?
Mujer > Hombre  
Mujer > c("Hombre","Mujer") #Funciona
Mujer > 1#No funciona
"Mujer" > "Hombre" #¿Funciona?

###Bases de datos dentro de R###

#install.packages("learningr")
library(learningr)
#data("obama_vs_mccain",package = "learningr")
help(obama_vs_mccain)
?obama_vs_mccain
View(obama_vs_mccain)
summary(obama_vs_mccain)
dim(obama_vs_mccain)

x<-na.omit(obama_vs_mccain)
View(x)
summary(x)

Obama

#Fijamos la base de datos como sigue:
attach(obama_vs_mccain)

obama_vs_mccain[,3]
obama_vs_mccain$Obama

##Ordenar una base de datos.

#Podemos ordenar la base de datos de la siguiente manera:
?order
?base::order #¿Cómo funciona éste comando?

#Ordenar la base de datos por región de manera ascendende:
class(Region) 
order(Region) #Arroja un vector con las posiciones de manera ordenada.
length(order(Region)) #Coincide con el número de renglones.
orden1<-obama_vs_mccain[order(Region),] #Ordena los renglones de la base por región
View(orden1)

#Ordenar la base de datos por estado de manera descendente.
orden2<-obama_vs_mccain[order(State, #Ordena las columnas por estados
                              decreasing = T),] #De manera descendente
View(orden2)

#Ordenar los nombres de la base por orden alfabético.
nombres<-names(obama_vs_mccain) #Estos son los nombres de la base de datos.
orden3<-obama_vs_mccain[,order(nombres)] #Ordena las columnas de la 
#base de datos por nombre.
View(orden3)

#Ordena la base de datos por region y luego por índice de
#desempleo de manera ascendente.
orden4<-obama_vs_mccain[order(Region,Unemployment),]
View(orden4)

#Ordena la base de datos por region de forma ascendente y 
#luego por índice de desempleo de manera descendente.
class(Unemployment)
orden5<-obama_vs_mccain[order(Region,-Unemployment),]
View(orden5)

#Ordena la base de datos por region de manera ascendete y 
#luego por estado descendente.
class(State)
#Aplicaremos un truco semejante al anterior tomando ventaja 
#de hacer número un factor.
orden6<-obama_vs_mccain[order(Region, #Ordena columnas por región (asc)
                              -as.numeric(State)),] #y luego por estado (desc)
View(orden6)


##Agrupando datos 

#Resúmen de la base de datos
summary(obama_vs_mccain)

#Número de estados por región.
table(Region)

#Crear intervalos
corte<- cut(x = McCain, #Al vector de porcentajes McCain,
            breaks = seq(0,100,10)) #Córtalo en intervalos del 0 al 100
#de 10 en 10.
barplot(table(corte),main = "%McCain")

corte<- cut(x = Obama, #Al vector de porcentajes Obama,
            breaks = seq(0,100,10)) #Córtalo en intervalos del 0 al 100 de 10 en 10.
barplot(table(corte),main = "%Obama")

#Promedio por estado
Promedios<-tapply(X = Obama, #Al vector Obama,
                  INDEX = Region, #Dados los factores por región,
                  FUN = mean) #Aplica la función mean. 
round(Promedios,digits = 2)

#Obtener una sub-base de datos
sub_base<-data.frame(Region,Corte_Obama=corte,Porcentaje=Obama)
View(sub_base)

#Promedio por intervalo
table(corte) #Tenemos intervalos sin datos.
Promedios<-tapply(X = Obama, #Al vector Obama,
                  INDEX = corte, #Dados los factores por región,
                  FUN = mean) #Aplica la función mean. 

#Mínimos por estado.
min_estado<-data.frame(State,Obama,McCain,Mínimo=pmin(Obama,McCain)) 
View(min_estado)

#Máximos por estado.
max_estado<-data.frame(State,Obama,McCain,Máximo=pmax(Obama,McCain)) 
View(max_estado)


# Clase 2 -----------------------------------------------------------------

library(learningr)
attach(obama_vs_mccain)
View(obama_vs_mccain)

##Querys

#¿Cuál es el estado que tiene más preferencia por Obama?
State[Obama==max(Obama)]

#¿Cuál es toda su información?
query<-obama_vs_mccain[Obama==max(Obama),]
View(query)

detach(obama_vs_mccain)

###Mini-Ejercicio: Factores de desarrollo de Chain Ladder###
#Suponga que usted trabaja en el área de reservas de una aseguradora de daños, 
#para uno de sus productos, afortunadamente, usted junto con otros compañeros 
#de trabajo, hicieron lo que cualquier buen equipo de secundaria y se dividieron
#el trabajo, después de que sus compañeros hicieran un par de cuentas,
#a usted le tocó el cálculo de los factores de desarrollo dada la tabla del
#archivo "Factores de desarrollo", termine su parte para que su equipo no lo odie.

#Tabla acumulada
Tabla <- read.delim("clipboard",header = T,row.names = 1)
#setwd("C:/Users/alarc/OneDrive/Escritorio")
#write.table(x = Tabla,file = "Tabla.txt")
#Tabla<-read.table(file = "Tabla.txt")
View(Tabla)
class(Tabla)
class(Tabla[1,1])
#Tabla <- as.matrix(Tabla)
#class(Tabla[1,1])
dim(Tabla)
#Factores de desarrollo
Factores <- c(sum(Tabla[1:6,2])/sum(Tabla[1:6,1]),
              sum(Tabla[1:5,3])/sum(Tabla[1:5,2]),
              sum(Tabla[1:4,4])/sum(Tabla[1:4,3]),
              sum(Tabla[1:3,5])/sum(Tabla[1:3,4]),
              sum(Tabla[1:2,6])/sum(Tabla[1:2,5]),
              sum(Tabla[1:1,7])/sum(Tabla[1:1,6]))

Factores

#En R existe un paquete que los obtiene
library(ChainLadder)
cadena<-chainladder(Tabla)
class(cadena)
cadena
cadena$Models[[1]][[1]]
cadena$Models[[2]]$coefficients
cadena$Models[[3]]$coefficients
cadena$Models[[4]]$coefficients
cadena$Models[[5]]$coefficients
cadena$Models[[6]]$coefficients

#IBNR (Incurred but not reported)
#Siniestros ocurridos pero no reportados.
MackChainLadder(Tabla)
Reserva<-MackChainLadder(Tabla,est.sigma = "Mack")
class(Reserva)
#Modelos
Reserva$FullTriangle
Reserva$Models
Reserva$Models[[1]][[1]]
cadena$Models[[2]]$coefficients
#Triángulo completo
Reserva$FullTriangle
#Factores como vector
Factores
Reserva$f
#Reserva IBNR 
summary(Reserva)
summary(Reserva)$Totals[4,]

#Estructuras de control
x<-"No funciona"
y<-5
if(y>10){ #Comentario
  x<-"Sí funciona"
}
x

#
y<- T + T

if(y !=2 ){
  print("No suma dos")
}else{
  print("¡Suma dos!")
}

#

x<-pi

if(x>3){
  print("x es mayor que 3")
}else if(x == 3){
  print("x es igual a 3")
}else{
  print("x es menor a 3")
}

v<-c("a","b","c")
#any(v=="A")

if(any(v=="a")){
  print("en v hay una 'a'")
}else{
  print("en v no hay una 'a'")
}

if(any(v=="z")){
  print("en v hay una 'z'")
}else{
  print("en v no hay una 'z'")
}

#

for(i in -3:3){
  print(paste("La variable 'i' es igual a: ",i))
}

v1<-seq(1,10,2)

for(i in v1){
  print(paste("La variable 'i' es igual a: ",i))
}


v2<-(1:5)*2

for(i in v2){
  print(paste("La variable 'i' es igual a: ",i))
}

v3<- c("a","b","c")

for(i in v3){
  print(paste("La variable 'i' es igual a: ",i))
}

#

n<-100
suma<-0
for(i in 1:n){
  suma <- suma + i
  #print(suma)
}
#La suma vale:
suma

#Comprobación
n*(n+1)/2


# Ejercicios -----------------------------------------------------------------
# ------------------------------------------------------------
# 1. Crea un código que muestre la sucesión de fibonacci hasta cierta posición "n".
# Respuesta: usando un bucle `for`.
n = 10 # La posición hasta donde se desea calcular la secuencia
fib = numeric(n) # Aquí se genera el vector de longitud n
# Aquí se asigna los primeros dos números de Fibonacci
fib[1] = 0
fib[2] = 1
# Luego calculamos lo siguientes números de Fibonacci
for (i in 3:n){
  fib[i] = fib[i-1] + fib[i-2]
}
# Metemos la variable a la consola de R
fib
# ------------------------------------------------------------

# --------------------------------------------------------------
#2. Crea un código que verifique si un número es primo.
# Respuesta: esta función puede verificar si un numero es primo o no:
is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  } else if (n == 2) {
    return(TRUE)
  } else if (n %% 2 == 0) {
    return(FALSE)
  } else {
    limit <- floor(sqrt(n))
    for (i in 3:limit) {
      if (n %% i == 0) {
        return(FALSE)
      }
    }
    return(TRUE)
  }
}
# --------------------------------------------------------------

#De la base de datos obama_vs_mccain (instala la paquetería learningr):

#3. ¿Cuántos mínimos tiene cada uno (Porcentaje de votos Obama-McCain)? 

#4. ¿En cuáles estados Obama pierde?

#5. ¿En cuáles estados Obama Gana?

#6. Ordena la base de datos obama_vs_mccain por región de manera ascendente
#con números romanos, en caso de empate, descendente por porcentaje
#de votos con Obama.



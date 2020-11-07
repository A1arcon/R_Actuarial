#Supongamos que tenemos una muestra de n personas,
#tomemos los días del año como 1 = 1/Enero y 365=31/Diciembre
#(suponiendo un año "normal", claro) entonces, 
#considerando la hipótesis de que es equiprobable nacer cualquier
#día del año, ¿Cuál es la probabilidad de que AL MENOS dos de las
#n personas cumplan exactamente el mismo día?

#Obtengamos una aproximación a dicha probabilidad vía simulación.

#¿Cómo obtener un éxito?
n<-23
M<-sample(x = 1:365,   #Del vector 1:365
          size = n,    #Toma una muestra de tamaño n
          replace = T) #Con reemplazo.

#La función anyDuplicated, de ser el caso,
#arroja el id del primer valor duplicado,
#es decir si uno ya salió "x" antes, arroja el id
#del inmediato siguiente idéntico a "x", si no hay
#duplicados, entonces arrojará un "0".
id<-anyDuplicated(x = M) ; id
which(M==M[id])

#Por lo tanto, tendremos un éxito si
#anyDuplicated != 0.
prueba<-anyDuplicated(x = M)

#Si hubo éxito, marca 1, si no, marca 0.
Éxito<-ifelse(test = prueba!=0,yes = 1,no = 0)
Éxito

#Queremos generar realizar lo anterior muchas veces
#para estimar una probabilidad.
set.seed(6)
n<-7 ; Ensayos<-100000
#Replicate hace un experimento varias veces.
M<-replicate(n = Ensayos, #Realiza "Ensayos" veces, la siguiente expresión:
             expr = sample(x = 1:365, size = n, replace = T))
#View(M) ; dim(M)

#Realizamos la prueba por columnas:
prueba<-apply(X = M,                #A la matriz M,
              MARGIN = 2,           #aplica por columnas,
              FUN = anyDuplicated)  #la función anyDuplicated.

#Entrada a entrada, si hubo éxito, marca 1, si no, marca 0.
Éxitos<-ifelse(test = prueba!=0,yes = 1,no = 0)
#length(Éxitos)

#Probabilidad estimada:
no.Éxitos<-sum(Éxitos)
no.Éxitos/Ensayos

#En la teoría, para "n" personas, la probabilidad estimada es:
1-exp(-n*(n-1)/(2*365))

#Podemos crear una función entonces que, dada cierta "n"
#nos arroje la probabilidad estimada como sigue:

prob.estimada<-function(n=100,Ensayos=10000){
  
  M<-replicate(n = Ensayos, #Realiza "Ensayos" veces, la siguiente expresión:
               expr = sample(x = 1:365, size = n, replace = T))
  #Realizamos la prueba por columnas:
  prueba<-apply(X = M,                #A la matriz M,
                MARGIN = 2,           #aplica por columnas,
                FUN = anyDuplicated)  #la función anyDuplicated.
  
  #Entrada a entrada, si hubo éxito, marca 1, si no, marca 0.
  Éxitos<-ifelse(test = prueba!=0,yes = 1,no = 0)
  
  #Probabilidad estimada:
  no.Éxitos<-sum(Éxitos)
  return(no.Éxitos/Ensayos)
  
}

#Podemos crear una función entonces que, dada cierta "n"
#nos arroje la probabilidad real aproximada como sigue:

prob.aprox<-function(n){
  
  return(1-exp(-n*(n-1)/(730)))
  
}


#A diferentes valores de n, vamos el resultado:
n<-seq(from = 5, #Del 5
       to = 50,  #al 50
       by = 5)   #De 5 en 5.

#Con este vector, calculamos valores estimados y aprox. reales.

#Aprox. Reales
Aprox.Reales<-sapply(X = n, #Al vector "n" , entrada a entrada
                  FUN = prob.aprox) #aplicale la función dada.


#Estimados
set.seed(21)
Estimados<-sapply(X = n, #Al vector "n" , entrada a entrada
                  FUN = prob.estimada) #aplicale la función dada.


#¿Cuánto tardó en generar todo?
set.seed(21)
system.time(expr =  sapply(X = n, FUN = prob.estimada))

#En estimar para todoslos valores dada cada n, 
#tardó menos de 5 segundos.

#Mostramos los resultados:
Resultados<-data.frame(n,Aprox.Reales,Estimados)
Resultados

#Grafica:
plot(Aprox.Reales~Estimados, #Aprox. Vs. Estimados
     main="Comparación", #Pon como título,
     col="red", #Color rojo,
     lwd=2)     #con ancho 2.

#Agrega la siguiente línea:
abline(a=0,        #ordenada de origen 0,
       b = 1,      #pendiente 1,
       col="blue", #color azul,
       lwd=2)      #ancho 2


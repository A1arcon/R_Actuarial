
# El problema es el siguiente: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "In an election where candidate A receives p votes and candidate B receives q votes
# with p > q, what is the probability that A will be strictly ahead of B throughout 
# the count?"
# Bertrand's ballot theorem - https://en.wikipedia.org/wiki/Bertrand%27s_ballot_theorem

# Proof: https://math.stackexchange.com/questions/2052000/bertrands-ballots-puzzle-proof-by-reflection

# Votantes con diferentes Parámetros --------------------------------------

Sim_Trayectorias<-function(argumentos){
  
  
  # Votantes para A
  N <- argumentos[1]
  # Votantes para B
  M <- argumentos[2]
  # Número de tiempos (contando el cero)
  n = N+M+1 # EN ESTE CASO, el número de tiempos va ligado con los parámetros N y M.
  
  # Los tiempos van entre 0:(N+M) pues son todos los votantes y el inicio en cero.
  times <- 0:(N+M)
  
  # Algoritmo de simulación
  algoritmo<-function(pasado,t){
    # Veamos el voto.
    voto<-sample(x = c(1,-1),  # Si vota A aumenta, B disminuye.
                 size = 1,      # Emite un voto
                 prob = c(N,M)/(N+M)) # Probabilidad de votar A y B respectivamente.
    # Cuando los votantes disminuyen conforme van pasando. 
    # (Obligamos a que la N y M sean guardadas aún fuera de la función)
    if(voto==1){
      N<<-N-1 # Disminuyen los votantes para A
    }else{
      M<<-M-1 # Disminuyen los votantes para A
    }
    # Movamos la trayectoria
    presente <- pasado + voto
    return(presente)
  }
  
  
  # Simulamos las trayectorias
  xt1<- 0 # Xt1 = X0 = 0
  Xt <- Reduce(f = algoritmo,                 #Aplica este algoritmo.
               x = times[2:n]-times[1:(n-1)], #Usa estos tiempos.
               init = xt1,                    #Aquí partimos.
               accumulate = TRUE)             #Toda la trayectoria.
  
  return(Xt)
  
}

# Realizamos las simulaciones:
argumentos<-list(c(100,10),c(10,1),c(1000,100),c(100,90),c(10,9),c(1000,900))
set.seed(21)
Xts <- sapply(X = argumentos,FUN = Sim_Trayectorias)
sapply(X = Xts,FUN = tail) # Aquí vemos dónde acaban las caminatas.
#dim(Xts)

#Hacemos los gráficos
library(ggplot2) ; library(ggpubr) ; library(RTextTools)
library(latex2exp)
plots<-list()
for(i in 1:length(Xts)) {
  df = data.frame(x = 0:(length(Xts[[i]]) - 1), y = Xts[[i]])
  # Gráfico del proceso
  plots[[i]] <- ggplot(df, aes(x = x, y = y)) +
    # Graficamos la línea con estas características
    geom_step(color = "darkblue", lty = 1, lwd = 0.7) +
    # Leyendas de los ejes.
    theme(
      plot.title = element_text(
        color = "violetred",
        size = 12.5,
        face = "bold.italic"
      ),
      axis.title.x = element_text(
        color = "blue",
        size = 11,
        face = "bold"
      ),
      axis.title.y = element_text(
        color = "red",
        size = 11,
        face = "bold"
      )
    ) +
    # Título
    labs(title = TeX(paste0(
      "Proceso con N = ",
      argumentos[[i]][1], " y M = ",
      argumentos[[i]][2]
    )),
    x = expr(t[i]),
    y = expr(X[t[i]])) +
    # Donde termina el proceso
    geom_hline(
      yintercept = argumentos[[i]][1] - argumentos[[i]][2],
      linetype = "dashed",
      color = "red",
      size = 0.25
    ) +
    # Umbral del empate entre los candidatos.
    geom_hline(
      yintercept = 0,
      color = "purple",
      size = 0.25
    )
}

figure <- egg::ggarrange(plots = plots,
                         ncol = 3, nrow = 2)



# Votantes con el mismo parámetro -----------------------------------------

# Votantes para A
N = 100
# Votantes para B
M = 50
# Número de tiempos (contando el cero)
n = N+M+1 # EN ESTE CASO, el número de tiempos va ligado con los parámetros N y M.

# Los tiempos van entre 0:(N+M) pues son todos los votantes y el inicio en cero.
times <- 0:(N+M)

Sim_Trayectorias<-function(){
  
  # Esta función presupone la existencia de los objetos "n" que indica el número de
  # simulaciones, el vector "times" que indica los tiempos t_i y la cantidad de 
  # votantes de A (N) y votantes por B (M).
  
  # Para regresar a las N a su valor original:
  Naux <- N
  Maux <- M
  
  # Algoritmo de simulación
  algoritmo<-function(pasado,t){
    # Veamos el voto.
    voto<-sample(x = c(1,-1),  # Si vota A aumenta, B disminuye.
                 size = 1,      # Emite un voto
                 prob = c(N,M)/(N+M)) # Probabilidad de votar A y B respectivamente.
    # Cuando los votantes disminuyen conforme van pasando.
    if(voto==1){
      N<<-N-1
    }else{
      M<<-M-1
    }
    # Movamos la trayectoria
    presente <- pasado + voto
    return(presente)
  }
  
  
  # Simulamos las trayectorias
  xt1<- 0 # Xt1 = X0 = 0
  Xt <- Reduce(f = algoritmo,                 #Aplica este algoritmo.
               x = times[2:n]-times[1:(n-1)], #Usa estos tiempos.
               init = xt1,                    #Aquí partimos.
               accumulate = TRUE)             #Toda la trayectoria.
  
  # Terminando esto, regresamos a la normalidad a N y a M
  N <<- Naux
  M <<- Maux
  
  return(Xt)
  
}

# Realizamos las simulaciones:
num_trayec = 7 # Voy a pedir que sea par para hacer gráficos después.
num_trayec <- ifelse(num_trayec%%2==0,num_trayec,num_trayec-1)
set.seed(21)
Xts <- replicate(n = num_trayec,expr = Sim_Trayectorias())
#dim(Xts)

#Hacemos los gráficos
library(ggplot2) ; library(ggpubr) ; library(RTextTools)
library(latex2exp)
plots<-list()
for(i in 1:ncol(Xts)){
  df = data.frame(x=times,y=Xts[,i])
  plots[[i]]<-ggplot(df, aes(x = x, y = y)) +
    geom_step(color="darkblue",lty=1,lwd=0.7)+
    theme(
      plot.title = element_text(color="violetred", size=12.5, face="bold.italic"),
      axis.title.x = element_text(color="blue", size=11, face="bold"),
      axis.title.y = element_text(color="red", size=11, face="bold")
    )+
    labs(title = TeX(paste0("Proceso ",i)),
         x = expr(t[i]),
         y = expr(X[t[i]]))+
    geom_hline(yintercept=N-M, linetype="dashed", 
               color = "red", size=0.25)
}

figure <- egg::ggarrange(plots = plots,
                         ncol = num_trayec/2, nrow = 2)


# Y ahora veamos todos juntos ---------------------------------------------

library(reshape2)
#View(Xts)

# Ajustamos algunos nombres
dat<-data.frame(times,Xts)
names(dat)<-c("tiempos",1:num_trayec)
dat.m<-melt(dat,           # Con estos datos
            id.vars = "tiempos",# Y usando de referencia
            measure.vars = names(dat)[-1])
names(dat.m)[2]="Trayectoria"

# Gráfico
ggplot(dat.m,aes(tiempos,value,colour=Trayectoria)) +
  geom_step(lwd=0.75) +
  scale_colour_manual(values = rainbow(n = 10,start = 0.05))+
  theme(
    plot.title = element_text(color="violetred", size=12.5, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=11, face="bold"),
    axis.title.y = element_text(color="red", size=11, face="bold")
  )+
  labs(title = "Trayectorias de las votaciones para A y B.",
       x = expr(t[i]),
       y = expr(X[t[i]]))+
  geom_hline(yintercept=N-M, linetype="dashed", 
             color = "red", size=0.25)+
  geom_hline(
    yintercept = 0,
    color = "purple",
    size = 0.25
  )

# Estimemos la probabilidad de ser positivo -------------------------------

# Si se desea modificar los parámetros hágalo aquí ~~~~~~~~~~~~~~~~~
# Votantes para A
N = 50
# Votantes para B
M = 35

# Número de tiempos (contando el cero)
n = N+M+1 # EN ESTE CASO, el número de tiempos va ligado con los parámetros N y M.

# Los tiempos van entre 0:(N+M) pues son todos los votantes y el inicio en cero.
times <- 0:(N+M)

# Realizamos las simulaciones:
num_trayec = 1000
set.seed(20)
Xts <- replicate(n = num_trayec,expr = Sim_Trayectorias())
Xts<-Xts[-1,]
# Función que revisa si alguno es negativo por
any_neg<-function(x){
  ifelse(any(x<=0),"No siempre positivos","Siempre positivos")
}
ensayos <- apply(X = Xts,MARGIN = 2,any_neg)
# Probabilidades Empíricas
table(ensayos)/length(ensayos)
# Probabilidad Teórica.
(N-M)/(N+M) # Bertrand's ballot theorem
# https://en.wikipedia.org/wiki/Bertrand%27s_ballot_theorem

apply(Xts,2,head,n=1)
apply(Xts,2,tail,n=1)

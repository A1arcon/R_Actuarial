
# Las siguientes funciones asumen la existencia de los objetos:
# 1. St    := Precio del bien subyacente en tiempo t.
# 2. T     := Tiempo de maduración de la opción.
# 3. t     := Tiempo de valuación de la opción.
# 4. r     := Tasa libre de riesgo.
# 5. sigma := Volatilidad.

# Opción Digital ----------------------------------------------------------
V_digital <- function(E){

  # Calculamos
  d1 = (log(St/E)+(r+1/2*sigma^2)*(T-t))/(sigma*sqrt(T-t))
  d2 = d1 - sigma*sqrt(T-t)
  
  # Precio
  return(exp(-r*(T-t))*pnorm(d2))
  
}


# Opción Call -------------------------------------------------------------
V_call <- function(E){
  
  # Calculamos
  d1 = (log(St/E)+(r+1/2*sigma^2)*(T-t))/(sigma*sqrt(T-t))
  d2 = d1 - sigma*sqrt(T-t)
  
  # Precio
  return(-(St*pnorm(d1)-E*exp(-r*(T-t))*pnorm(d2)))
  
}
## Derivamos
V_call_derivada <- function(E){
  numDeriv::grad(V_call,E)
}


# Gráficos ----------------------------------------------------------------

par(mfrow=c(2,2))

## Gráfico 1~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. St    := Precio del bien subyacente en tiempo t.
St = 50
# 2. T     := Tiempo de maduración de la opción.
T = 1
# 3. t     := Tiempo de valuación de la opción.
t = 0
# 4. r     := Tasa libre de riesgo.
r = 0.05
# 5. sigma := Volatilidad.
sigma = 2

curve(V_digital,from=0,to=100,
      main=latex2exp::TeX(paste0("$S_t$=",St,", $T=",T,"$, $t=",t,"$, $r=",r*100,"%$, ",
                                 "$\\sigma$=",sigma)),
      ylab=latex2exp::TeX("$V_{digital}(S_t,t)$"),
      xlab="Precio de ejercicio",
      col="darkblue",
      lwd=3,ylim=c(0,1))
curve(V_call_derivada,from=0.1,to=100,
      add=TRUE,col="gold",lwd=3,lty=2)
abline(h=0,v=0,col="red")
legend("topright", legend=c(latex2exp::TeX("$V_{digital}$"), latex2exp::TeX("$\\dV_{call}/dE}$") ),
       col=c("darkblue", "gold"), lty=1:2,lwd=2,bg='lightblue')

## Gráfico 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. St    := Precio del bien subyacente en tiempo t.
St = 50
# 2. T     := Tiempo de maduración de la opción.
T = 1
# 3. t     := Tiempo de valuación de la opción.
t = 0.8
# 4. r     := Tasa libre de riesgo.
r = 0.05
# 5. sigma := Volatilidad.
sigma = 2

curve(V_digital,from=0,to=100,
      main=latex2exp::TeX(paste0("$S_t$=",St,", $T=",T,"$, $t=",t,"$, $r=",r*100,"%$, ",
                                 "$\\sigma$=",sigma)),
      ylab=latex2exp::TeX("$V_{digital}(S_t,t)$"),
      xlab="Precio de ejercicio",
      col="darkblue",
      lwd=3,ylim=c(0,1))
curve(V_call_derivada,from=0.1,to=100,
      add=TRUE,col="gold",lwd=3,lty=2)
abline(h=0,v=0,col="red")
legend("topright", legend=c(latex2exp::TeX("$V_{digital}$"), latex2exp::TeX("$\\dV_{call}/dE}$") ),
       col=c("darkblue", "gold"), lty=1:2,lwd=2,bg='lightblue')

## Gráfico 3~~~~~~~~~~~~~~~~~~~~~~~

# 1. St    := Precio del bien subyacente en tiempo t.
St = 50
# 2. T     := Tiempo de maduración de la opción.
T = 1
# 3. t     := Tiempo de valuación de la opción.
t = 0.95
# 4. r     := Tasa libre de riesgo.
r = 0.05
# 5. sigma := Volatilidad.
sigma = 2

curve(V_digital,from=0,to=100,
      main=latex2exp::TeX(paste0("$S_t$=",St,", $T=",T,"$, $t=",t,"$, $r=",r*100,"%$, ",
                                 "$\\sigma$=",sigma)),
      ylab=latex2exp::TeX("$V_{digital}(S_t,t)$"),
      xlab="Precio de ejercicio",
      col="darkblue",
      lwd=3,ylim=c(0,1))
curve(V_call_derivada,from=0.1,to=100,
      add=TRUE,col="gold",lwd=3,lty=2)
abline(h=0,v=0,col="red")
legend("topright", legend=c(latex2exp::TeX("$V_{digital}$"), latex2exp::TeX("$\\dV_{call}/dE}$") ),
       col=c("darkblue", "gold"), lty=1:2,lwd=2,bg='lightblue')

## Gráfico 4~~~~~~~~~~~~~~~~~~~~~

# 1. St    := Precio del bien subyacente en tiempo t.
St = 50
# 2. T     := Tiempo de maduración de la opción.
T = 1
# 3. t     := Tiempo de valuación de la opción.
t = 1
# 4. r     := Tasa libre de riesgo.
r = 0.05
# 5. sigma := Volatilidad.
sigma = 2

curve(V_digital,from=0,to=100,
      main=latex2exp::TeX(paste0("$S_t$=",St,", $T=",T,"$, $t=",t,"$, $r=",r*100,"%$, ",
                                 "$\\sigma$=",sigma)),
      ylab=latex2exp::TeX("$V_{digital}(S_t,t)$"),
      xlab="Precio de ejercicio",
      col="darkblue",
      lwd=3,ylim=c(0,1))
curve(V_call_derivada,from=0.1,to=100,
      add=TRUE,col="gold",lwd=3,lty=2)
abline(h=0,v=0,col="red")
legend("topright", legend=c(latex2exp::TeX("$V_{digital}$"), latex2exp::TeX("$\\dV_{call}/dE}$") ),
       col=c("darkblue", "gold"), lty=1:2,lwd=2,bg='lightblue')


par(mfrow=c(1,1))


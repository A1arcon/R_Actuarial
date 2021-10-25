

# Implementación de la función de distribución empírica
ecdf_fun=function(x,CI=TRUE,CI.interval=0.95){
  x <- sort(x)
  n <- length(x)
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
                    method = "constant", yleft = 0, 
                    yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
  x.val=environment(rval)$x
  y.val=environment(rval)$y
  
  if(CI==TRUE){
    alpha=1-CI.interval
    eps=sqrt(log(2/alpha)/(2*n))
    ll=pmax(y.val-eps,0) 
    uu=pmin(y.val+eps,1)
    return(data.frame(value=x.val,proportion=y.val,
                      lwr.CI=ll,upr.CI=uu))
  }else{
    return(data.frame(value=x.val,proportion=y.val))
  }
}

# Ejemplo -----------------------------------------------------------------

# Datos iniciales
set.seed(2012)
sim <- rnorm(100)
Fn <- ecdf_fun(sim)
# Formato del gráfico
plot(Fn$proportion~Fn$value,ylim=c(0,1),
     ylab=latex2exp::TeX("$F_n$"),xlab="Valores",
     main = "CDF")
# Fondo
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb")
grid(col="white",lwd=2)
# Gráfico de los intervalos de confianza
with(Fn,polygon(c(value,rev(value)), c(lwr.CI,rev(upr.CI)),
                col = adjustcolor("red",alpha.f=0.25) ,
                border=NA,type="s"))
with(Fn,lines(lwr.CI~value,type="s",lty=2,col="red"),lwd=2)
with(Fn,lines(upr.CI~value,type="s",lty=2,col="red"),lwd=2)
#Gráfico de la CDF
curve(expr = pnorm(x),col="darkgreen",add = TRUE,lwd=2)
# Gráfico de la ECDF
with(Fn,lines(proportion~value,type="s",col="blue"))
# Observaciones
with(Fn,points(rep(-0.01,length(value))~value,pch="|",lty=2,col="black",cex=0.75))
# Leyenda
legend("topleft", legend=c("CDF", "ECDF","CI(95%)"),
       col=c("darkgreen", "blue","red"), lty=c(1,1,2), cex=0.8,
       title="Curvas", text.font=4, bg='lightblue')



# Ejemplo bootstrap para la mediana ---------------------------------------

{
# Implementación del algoritmo con datos similados
set.seed(20)
X <- rnorm(1000)   # Datos simulados
Tn <- median(X)     # Mediana real de los datos
B = 5000          # Repeticiones bootstrap
# Función que calcula Tn*
Tstar <- function(datos,i){median(datos[i])}
# Procedimiento bootstrap
Tboot <- boot::boot(data = X,statistic = Tstar,R = B)$t[,1]
# Error estándar
se_boot <- sqrt(var(Tboot))
# Nivel de confianza para los intervalos
alpha <- 0.95
# Intervalo Normal
CI_Nor <- Tn + qnorm(1-alpha/2)*se_boot*c(-1,1)
# Intervalo Pivotal
CI_Piv <- 2*Tn-quantile(Tboot,probs = c(1-alpha/2,alpha/2))
# Intervalo por Percentiles
CI_Per <- quantile(Tboot,probs = c(alpha/2,1-alpha/2))
# Dividimos en dos gráficos
par(mfrow=c(2,1))
# Gráfico 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gráfico de la simulaciones
hist(Tboot,probability = TRUE,
     main="Histograma Bootstrap",
     ylab="Densidad",xlab = latex2exp::TeX("$T^*_{n}$"))
# Fondo
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb")
grid(col="white",lwd=2)
# Histograma
hist(Tboot,
     col=adjustcolor("yellow",alpha.f=0.25),
     probability = TRUE,
     add=TRUE)
abline(v = Tn, col = "blue",lwd=2)
polygon(y=c(7, 10,  10, 7),x= rep(CI_Nor,each=2), 
        col=adjustcolor("purple",alpha.f = 0.5),
        border=NA)
polygon(y=c(4, 7,  7, 4),x= rep(CI_Piv,each=2), 
        col=adjustcolor("darkgreen",alpha.f = 0.5),
        border=NA)
polygon(y=c(1, 4,  4, 1),x= rep(CI_Per,each=2), 
        col=adjustcolor("red",alpha.f = 0.5),
        border=NA)
# Leyenda
legend("topleft", legend=c(latex2exp::TeX("$T_n$"),
                           "CI Normal",
                           "CI Pivotal",
                           "CI Percentil"),
       col=c("blue", "purple","darkgreen","red"), 
       cex=0.8, lwd=3,
       text.font=4, bg='lightblue')
# Gráfico 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Gráfico de la simulaciones
hist(Tboot,probability = TRUE,
     main="Histograma Bootstrap (Zoom)",
     xlim = c(0,0.03),
     ylab="Densidad",xlab = latex2exp::TeX("$T^*_{n}$"))
# Fondo
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "#ebebeb")
grid(col="white",lwd=2)
# Histograma
hist(Tboot,
     col=adjustcolor("yellow",alpha.f=0.25),
     probability = TRUE,
     add=TRUE)
abline(v = Tn, col = "blue",lwd=2)
polygon(y=c(7, 10,  10, 7),x= rep(CI_Nor,each=2), 
        col=adjustcolor("purple",alpha.f = 0.5),
        border=NA)
polygon(y=c(4, 7,  7, 4),x= rep(CI_Piv,each=2), 
        col=adjustcolor("darkgreen",alpha.f = 0.5),
        border=NA)
polygon(y=c(1, 4,  4, 1),x= rep(CI_Per,each=2), 
        col=adjustcolor("red",alpha.f = 0.5),
        border=NA)
# Leyenda
legend("topleft", legend=c(latex2exp::TeX("$T_n$"),
                           "CI Normal",
                           "CI Pivotal",
                           "CI Percentil"),
       col=c("blue", "purple","darkgreen","red"), 
       cex=0.8, lwd=3,
       text.font=4, bg='lightblue')
}
par(mfrow=c(1,1))

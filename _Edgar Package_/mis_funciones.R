

# Funciones Diversas ------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


substrLeft <- function(x, n){
  substr(x, 1, n)
}

write_clipboard <- function(x){
  clipr::write_clip(x)
}

read_clipboard <- function(){
  clipr::read_clip()
}



# Función de distribución Empírica ----------------------------------------

# Implementación de la función de distribución empírica con gráfica
ECDF <- function(x,CI=TRUE,CI.interval=0.95,plot=TRUE){
  
  # x  = Vector de datos para obtener la ECDF
  # CI = Calcula los intervalos de confianza
  # CI.Interval = Nivel de confianza de los intervalos
  # plot = Indica si se desea realizar un gráfico
  
  # https://rdrr.io/github/SwampThingPaul/AnalystHelper/src/R/ecdf_fun.R
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
  # Se guardan los resultados
  Fn <- ecdf_fun(x)
  # Procedemos a calcular el gráfico si es necesario
  if(plot){
    # Formato del gráfico
    plot(Fn$proportion~Fn$value,ylim=c(0,1),
         ylab=latex2exp::TeX("$F_n$"),xlab="Valores",
         main = "CDF")
    # Fondo
    rect(par("usr")[1], par("usr")[3],
         par("usr")[2], par("usr")[4],
         col = "#ebebeb")
    grid(col="white",lwd=2)
    if(CI){
      # Gráfico de los intervalos de confianza
      with(Fn,polygon(c(value,rev(value)), c(lwr.CI,rev(upr.CI)),
                      col = adjustcolor("red",alpha.f=0.25) ,
                      border=NA))
      with(Fn,lines(lwr.CI~value,type="s",lty=2,col="red"),lwd=2)
      with(Fn,lines(upr.CI~value,type="s",lty=2,col="red"),lwd=2)
    }
    # Gráfico de la ECDF
    with(Fn,lines(proportion~value,type="s",col="blue"))
    # Observaciones
    with(Fn,points(rep(-0.01,length(value))~value,pch="|",lty=2,col="black",cex=0.75))
  }
  
  return(Fn)
  
}

# # Ejemplo
# 
# # Datos iniciales
# set.seed(2012)
# sim <- rnorm(100)
# ECDF(sim)
# #Gráfico de la CDF
# curve(expr = pnorm(x),col="darkgreen",add = TRUE,lwd=2)
# # Leyenda
# legend("topleft", legend=c("CDF", "ECDF","CI(95%)"),
#        col=c("darkgreen", "blue","red"), lty=c(1,1,2), cex=0.8,
#        title="Curvas", text.font=4, bg='lightblue')

# Series de tiempo ggplot -------------------------------------------------

# Este puede ser un auxiliar
convierte_hora <- function(hora){
  # Si 'hora' viene en formato "%H:%M"
  hora %>% hms::parse_hm() %>% as.POSIXct() 
}


# Para series de tiempo con ggplot
ggplot_time_series <- function(data,x,y=NULL,
                               label_column=NULL,title=NULL,subtitle=NULL,xlab=NULL,ylab=NULL,
                               ymin=NULL,ymax=NULL,
                               alpha=0.5,
                               x_breaks=10,y_breaks=10,
                               x_hora=FALSE){
  
  # Si 'x' viene en formato "%H:%M", por ejemlo, un caractter que viene "17:30", entonces:
  if(x_hora){
    # (puedes usar la función "convierte_hora")
    data[,x] <- data[,x] %>% hms::parse_hm() %>% as.POSIXct() 
  }
  
  # En caso de que no haya líneas ni grupos
  if(is.null(y)&is.null(label_column)){
    label_column = "Intervalo"
    data[,label_column] = as.factor(rep("Datos",nrow(data)))
  }
  
  # En caso de que no haya grupos
  if(is.null(label_column)){
    label_column = "Línea"
    data[,label_column] = as.factor(rep("Datos",nrow(data)))
  }
  
  plt <-
    # Carga de datos
    ggplot(data = data) +
    # Escalas de los ejes
    scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks))
    # En el caso del eje 'x' si tenemos horas...
  if(x_hora){
    plt <- plt + scale_x_datetime(date_labels = "%H:%M",breaks = scales::pretty_breaks(n = x_breaks))
  } 
  else{
    plt <- plt + scale_x_continuous(breaks = scales::pretty_breaks(n = x_breaks))
  }
  
    # Líneas
  if(!is.null(y)){  
    plt <- plt +
      geom_line(aes(x = data[,x],y = data[,y],
                    color = data[,label_column], 
                    linetype = data[,label_column]))
  }
    # Intervalos
  if(!is.null(ymin)&!is.null(ymax)){ # Solo si introducen valores
    plt <- plt +
      geom_ribbon(aes(x = data[,x],ymin = data[,ymin],ymax = data[,ymax],fill = data[,label_column]),
                  alpha = alpha) 
  }
  
  # Títulos
  plt <- plt +
    labs(
      title = title,
      subtitle = subtitle,
      y = ylab,
      x = xlab,
      color = label_column,
      fill = label_column,
      linetype = label_column
    )
  
  return(plt)
  
}

# # Ejemplo
# 
# # Datos
# X <- runif(100,-5,5)
# Y <- ifelse(test = X>=0,yes = X^2+2,no = X^2-2)
# Ymin <- Y + abs(rnorm(length(X),sd=2))
# Ymax <- Y - abs(rnorm(length(X),sd=2))
# Label_column <- ifelse(test = X>=0,yes = "Negativo",no = "Positivo")
# Data <- data.frame(X,Y,Ymin,Ymax,Label_column)
# colnames(Data)
# 
# # 1.
# ggplot_time_series(data = Data,x = "X",y = "Y",
#                    label_column = "Label_column",
#                    ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)
# 
# # 2.
# ggplot_time_series(data = Data,x = "X",y = "Y",
#                    label_column = "Label_column",
#                    #ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)
# 
# # 3.
# ggplot_time_series(data = Data,x = "X",y = "Y",
#                    #label_column = "Label_column",
#                    #ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)
# 
# # 4.
# ggplot_time_series(data = Data,x = "X",#y = "Y",
#                    #label_column = "Label_column",
#                    ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)
# 
# # 5.
# ggplot_time_series(data = Data,x = "X",#y = "Y",
#                    label_column = "Label_column",
#                    ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)
# 
# # 6.
# ggplot_time_series(data = Data,x = "X",y = "Y",
#                    #label_column = "Label_column",
#                    ymin = "Ymin",ymax = "Ymax",
#                    title = "Parábola partida",
#                    subtitle = "En positivos y negativos",
#                    xlab = "X",
#                    ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
#                    alpha = 0.25,x_breaks = 10,y_breaks = 20)


# Histogramas ggplot ------------------------------------------------------


# Para histogramas y densidades empíricas con ggplot

# df es un data.frame que tiene al menos una feature. Si existe, 
# una variable que categorice, llamémosla 'label_column' la agregamos.
# Por ejemplo, el data.frame iris tiene estas características
# feature = iris[,1] y label_column = iris[,5]

# https://stackoverflow.com/questions/6957549/overlaying-histograms-with-ggplot2-in-r
plot_multi_histogram_density <- function(df, feature, label_column=NULL,
                                         histogram=TRUE,density=TRUE,mean=TRUE,
                                         title=NULL,subtitle=NULL,
                                         ylab="Density",xlab=feature,
                                         alpha_hist=0.7,alpha_dens=0.7) {
  
  # En caso de que no haya grupos
  if(is.null(label_column)){
    label_column = "Histograma"
    df[,label_column] = as.factor(rep("Datos",nrow(df)))
  }
  
  # Definimos los colores
  df[,label_column] <- df[,label_column] %>% as.factor()
  colores = df[,label_column] %>% levels() %>% length() %>% viridis::viridis()
  
  library(ggplot2)
  plt <- ggplot(df, aes(x=df[,feature], 
                        fill=df[,label_column]))
  
  # Histograma
  if(histogram){
    plt <- plt + geom_histogram(alpha=alpha_hist, position="identity", 
                                aes(y = ..density..), color="black")
  }
  # Densidades
  if(density){
    plt <- plt + geom_density(alpha=alpha_dens)
  }
  # Caso raro...
  if(!histogram&!density){
    warning("No puedes tener 'histogram'=FALSE y 'density'=FALSE.")
    return()
  }
  
  # Le ponemos lo demás
  plt <- plt +
    # Relleno
    scale_fill_manual(name=label_column,values=colores)
  
  # Media de los histogramas
  if(mean){
    
    # Media global
    plt <- plt + geom_vline(aes(xintercept=mean(df[,feature],na.rm = TRUE),color="Global"),
                            linetype="dashed", size=1)
    valores = c(Global="red")
    # Media por categoría
    if(length(colores)>1){
      for(i in 1:length(colores)){
        nivel = levels(df[,label_column])[i]
        comando <- paste0("plt <- plt + geom_vline(aes(xintercept=mean(df[df[,label_column]=='",nivel,"',feature],na.rm = TRUE),color='",nivel,"'),linetype='dashed', size=1)")
        eval(parse(text=comando))
        valores[i+1] <- colores[i]
        names(valores)[i+1] <- nivel
      }
    }
    
    # Le ponemos los títulos
    plt <- plt + scale_color_manual(name = "Media",
                                    values = valores)
  }
  
  plt <- plt +
    # Eje Horizontal
    geom_hline(yintercept=0,color="black", size=1) +
    # Títulos
    labs(title=title,
         subtitle=subtitle,
         y=ylab, x=xlab)
  
  # Valor de retorno
  return(plt)
  
}

# Ejemplo
# plot_multi_histogram_density(df = iris,feature = "Sepal.Length",label_column = "Species",
#                              histogram = T,density = T,mean=T,
#                              title = "Hola",subtitle = "Adiós",ylab = "densidad",xlab = "error",
#                              alpha_hist = 0.5,alpha_dens = 0.25)




# Superficies 3D plotly ---------------------------------------------------

# Para hacer gráficos fácil en 3D
superficie3d <- function(x.from,x.to,y.from,y.to,fxy,x.lab="x",y.lab="y",z.lab="z",main="",epsilon=0.01,sombra=FALSE,leyenda=TRUE){
  
  library(plotly)
  # from      := A partir de dónde corre esta coordenada.
  # to        := Hasta dónde corre esta coordenada.
  # fxy       := f(x,y) (función de R2 a R).
  # lab       := Nombre del eje/coordenada.
  # main      := Título del gráfico.
  # epsilon   := Precisión en la evaluación de la función.
  # sombra    := Se proyecta la sombra de la superficie.
  # leyenda   := Se muestra la leyenda.
  
  # Definimos dónde vamos a evaluar la superficie.
  x <- seq(x.from,x.to,epsilon)
  y <- seq(y.from,y.to,epsilon)
  z <- outer(X = x,Y = y,FUN = Vectorize(fxy)) %>% t()
  
  fig <- plot_ly(x = x, y = y, z = z,showscale=leyenda) %>% 
    add_surface(
      contours = list(
        z = list(
          show=sombra,
          usecolormap=TRUE,
          highlightcolor="#ff0000",
          project=list(z=TRUE)
        )
    )) %>% 
    layout(
      title = paste("\n",main),
      scene = list(
        xaxis = list(title = x.lab),
        yaxis = list(title = y.lab),
        zaxis = list(title = z.lab)
      ))
  
  return(fig)
  
  # Más información:
  # https://plotly.com/r/3d-surface-plots/#basic-3d-surface-plot
  # https://plotly.com/r/3d-axes/
  
}

# Ejemplo 0
# fxy <- function(x,y){5}
# superficie3d(x.from = 0,x.to = 1,y.from = 0,y.to = 1,
#              fxy = fxy,x.lab = "eje x",y.lab = "eje y",
#              z.lab = "eje z",
#              main = "Gráfico")
# # Ejemplo 1
# fxy <- function(x,y){x^2}
# superficie3d(x.from = 0,x.to = 1,y.from = 0,y.to = 1,
#              fxy = fxy,x.lab = "eje x",y.lab = "eje y", z.lab = "eje z",
#              main = "Gráfico",leyenda = FALSE)
# # Ejemplo 2
# fxy <- function(x,y){x^2 + y^2}
# superficie3d(x.from = -3,x.to = 3,y.from = -5,y.to = 5,
#              fxy = fxy,x.lab = "eje x",y.lab = "eje y", z.lab = "eje z",
#              main = "Gráfico",sombra = TRUE)

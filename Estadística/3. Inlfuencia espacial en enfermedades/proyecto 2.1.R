library(sf)
library(tmap)
library(sp)
library(spdep)
library(maptools)
library(RColorBrewer)
library(tripack)
library(spatialreg)
library(glmmfields)
library(ggplot2)
library(dplyr)


nc <- st_read("sids.shp")

# Analisis exploratorio ---------------------------------------------------
nc$both <- factor(paste(nc$L_id, nc$M_id, sep=":"))

# Graficamos California del Norte con el ID alfabético por condado
plot(st_geometry(nc), axes=TRUE, main = "Mapa de condados de California del Norte")
text(st_coordinates(st_centroid(st_geometry(nc), of_largest_polygon=TRUE)), label=nc$CRESS_ID, cex=1)

#Graficamos la relación de Vecinos (nivel 1)
nc.condados<-readShapeSpatial("sids.shp",ID="CRESS_ID")
sids.vecinos <- poly2nb(nc.condados)
plot(sids.vecinos,coordinates(nc.condados),col="blue",pch=".")
plot(nc.condados,add=T, border="darkgrey")
text(st_coordinates(st_centroid(st_geometry(nc), of_largest_polygon=TRUE)), label=nc$CRESS_ID, cex=1)
title(main = "Relación de vecinos por condado")

#Mapa de probabilidades 74
ch <- choynowski(nc$SID74, nc$BIR74)
nc$ch_pmap_low <- ifelse(ch$type, ch$pmap, NA)
nc$ch_pmap_high <- ifelse(!ch$type, ch$pmap, NA)
prbs <- c(0,0.01,.05,1)
nc$high74 = cut(nc$ch_pmap_high, prbs)
nc$low74 = cut(nc$ch_pmap_low,prbs )

#Mapa de probabilidades 79
ch <- choynowski(nc$SID79, nc$BIR79)
nc$ch_pmap_low <- ifelse(ch$type, ch$pmap, NA)
nc$ch_pmap_high <- ifelse(!ch$type, ch$pmap, NA)
prbs <- c(0,0.01,.05,1)
nc$high79 = cut(nc$ch_pmap_high, prbs)
nc$low79 = cut(nc$ch_pmap_low,prbs )

#is_tmap <- FALSE
#if (require(tmap, quietly=TRUE)) is_tmap <- TRUE
#is_tmap

tm_shape(nc) + tm_fill(c("low74", "high74","low79", "high79"), palette="YlOrRd", title="p-values") +
  tm_layout(main.title = "Choynowski", main.title.position = "center") + 
  tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Grupo Bajo (1974-1978)", "Grupo Alto (1974-1978)","Grupo Bajo (1979-1984)", "Grupo Alto (1979-1984)")) +
  tm_text("CRESS_ID", size = 0.6)


# En la gráfica podemos ver el p-value de H0
#en la gráfica "low" H0: el condado es bajo
#en la gráfica "high" H0: el condado pertenece a alto
#Hay condados cuya tasa de mortalidad por SID es inusialmente alta
#o inusualmente baja

#la Rho es un indice de desviación de igualdad

pmap <- probmap(nc$SID74, nc$BIR74)
nc$pmap <- pmap$pmap
brks <- c(0,0.001,0.01,0.025,0.05,0.95,0.975,0.99,0.999,1)
tm_shape(nc) +
  tm_layout(main.title = "Mapa de probabilidades", main.title.position = "center") + 
  tm_fill("pmap", breaks=brks, midpoint=0.5, palette="RdBu") + tm_layout(legend.outside=TRUE) +
  tm_text("CRESS_ID", size = 0.6)

#Muertes esperadas por condado para 74 y 79
global_rate74 <- sum(nc$SID74)/sum(nc$BIR74)
global_rate79 <- sum(nc$SID79)/sum(nc$BIR79)
nc$Expected74 <- global_rate74 * nc$BIR74
nc$Expected79 <- global_rate79 * nc$BIR79

#(Pregunta 1)
nc$rr74 <- (nc$SID74/nc$BIR74)/(sum(nc$SID74)/sum(nc$BIR74))
nc$rr79 <- (nc$SID79/nc$BIR79)/(sum(nc$SID79)/sum(nc$BIR79))
ks.test((nc$rr74), (nc$rr79))

#(pregunta 2) Cociende de RR 
nc$or <- ifelse((nc$rr74 ==0 & nc$rr79 ==0), 1, (nc$rr74+0.5)/(nc$rr79+0.5)) 
brks <- c(0, 0.25, 0.75, 1.25, 2, Inf)
tm_shape(nc) + 
  tm_layout(main.title = "Cociente de riesgos relativos", main.title.position = "center") + 
  tm_fill("or", breaks = brks, midpoint=1, palette="RdBu") + tm_layout(legend.outside=TRUE) +
  tm_text("CRESS_ID", size = 0.8)


# Analisis sin efecto espacial --------------------------------------------

fit74_1 <- glm(SID74 ~ offset(log(BIR74)), data=nc, family="poisson")
nc$rr74fitns <- fit74_1$fitted.values/nc$Expected74
nc$res74ns <- (nc$rr74 - nc$rr74fitns)

fit74_1$fitted.values
exp(log(nc$BIR74)+fit74_1$coefficients)
nc$BIR74*sum(nc$SID74)/sum(nc$BIR74)

mean(nc$Expected74)
mean(fit74_1$fitted.values)

fit74_1$coefficients
log(sum(nc$SID74)/sum(nc$BIR74))

fit79_1 <- glm(SID79 ~ offset(log(BIR79)), data=nc, family="poisson")
nc$rr79fitns <- fit79_1$fitted.values/nc$Expected79
nc$res79ns <- (nc$rr79 - nc$rr79fitns)

brks <- c(-Inf, -1:3, Inf)
tm_shape(nc) +
  tm_fill(c("res74ns" , "res79ns"), breaks = brks, title = c("Res. 74" , "Res. 79"), midpoint=0, palette="RdBu") +
  tm_layout(legend.outside=TRUE) +
  tm_layout(main.title = "Residuales del RR a partir de un modelo Poisson", main.title.position = "center") + 
  tm_text("CRESS_ID", size = 0.6) +
  tm_layout(panel.labels=c("1974-1978", "1979-1984")) 
  
#aqui vemos que los residuales no son aleatorios porque los rojos están 
#todos juntos y los azules también

#  Esto nos permitirá calcular vecinadaes

ncs <- as(nc, "Spatial")
w <- poly2nb(ncs, row.names=ncs$FIPSNO)
summary(w)

# creamos la matriz cuadrada de pesos 
wm <- nb2mat(w, style='B',zero.policy=TRUE)
dim(wm)
image(wm)

# Convertimos la matriz en lista de pesos 

rwm <- mat2listw(wm, style='W')
any(is.na(rwm$weights))

## Calculamos el indice de Moran para los residuos
library(ape)
# H0: Independencia (no correlacionados)
mora <- Moran.I(nc$res74ns, weight = wm,na.rm = TRUE)
mora$p.value

mora <- Moran.I(nc$res79ns, weight = wm,na.rm = TRUE)
mora$p.value

#se rechaza NO correlación espacial de los residuales


# Modelo SAR --------------------------------------------------------------

fit74_2<-lagsarlm(rr74 ~ BIR74, data=nc,
                nb2listw(sids.vecinos, style="W"), method="eigen", quiet=TRUE)
summary(fit74_2)

fit79_2<-lagsarlm(rr79 ~ BIR79, data=nc,
                  nb2listw(sids.vecinos, style="W"), method="eigen", quiet=TRUE)
summary(fit79_2)

fit74_3<-lagsarlm(rr74 ~ BIR74 + both - 1, data=nc,
                  nb2listw(sids.vecinos, style="W"), method="eigen", quiet=TRUE)
summary(fit74_3)

fit79_3<-lagsarlm(rr79 ~ BIR79 + both - 1, data=nc,
                  nb2listw(sids.vecinos, style="W"), method="eigen", quiet=TRUE)
summary(fit79_3)

# CAR ---------------------------------------------------------------------
fit74_4 <- spautolm(rr74 ~ BIR74 , data=nc, nb2listw(sids.vecinos, style="W"), weights=BIR74, family="CAR")
summary(fit74_4)

fit79_4 <- spautolm(rr79 ~ BIR79 , data=nc, nb2listw(sids.vecinos, style="W"), weights=BIR74, family="CAR")
summary(fit79_4)

fit74_5 <- spautolm(rr74 ~ BIR74 + both - 1 , data=nc, nb2listw(sids.vecinos, style="W"), weights=BIR74, family="CAR")
summary(fit74_5)

fit79_5 <- spautolm(rr79 ~ BIR79 + both - 1 , data=nc, nb2listw(sids.vecinos, style="W"), weights=BIR74, family="CAR")
summary(fit79_5)
a <- summary(fit79_5$fit)

# Selección del mejor modelo ----------------------------------------------

deviance(fit74_1)
deviance(fit74_2)
deviance(fit74_3)
deviance(fit74_4)
deviance(fit74_5)

AIC(fit74_1)
AIC(fit74_2)
AIC(fit74_3)
AIC(fit74_4)
AIC(fit74_5)

BIC(fit74_1)
BIC(fit74_2)
BIC(fit74_3)
BIC(fit74_4)
BIC(fit74_5)

deviance(fit79_1)
deviance(fit79_2)
deviance(fit79_3)
deviance(fit79_4)
deviance(fit79_5)

AIC(fit79_1)
AIC(fit79_2)
AIC(fit79_3)
AIC(fit79_4)
AIC(fit79_5)

BIC(fit79_1)
BIC(fit79_2)
BIC(fit79_3)
BIC(fit79_4)
BIC(fit79_5)

#el mejor modelo para 74 es fit74_5 y para el 79 fit79_5

#Veamos los residuales estadarizados
nc$res74 <- fit74_5$fit$residuals
nc$res79 <- fit79_5$fit$residuals
brks <- c(-Inf, -4:4, Inf)
tm_shape(nc) + 
  tm_fill(c("res74", "res79"), title = "Res.", palette = "RdBu",
          auto.palette.mapping=FALSE, breaks = brks) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residuales del modelo CAR", main.title.size = 1 ,
            legend.position = c("right", "top"), legend.title.size = 0.8,
            panel.labels=c("1974 - 1978", "1979 - 1983"), legend.outside=TRUE)+
  tm_text("CRESS_ID", size = 0.6)

mora <- Moran.I(fit74_5$fit$residuals, weight = wm,na.rm = TRUE)
mora$p.value

mora <- Moran.I(fit79_5$fit$residuals, weight = wm,na.rm = TRUE)
mora$p.value

#Veamos los ajustados
nc$ganador74 <- fitted(fit74_5)
nc$ganador79 <- fitted(fit79_5)
brks <- c(seq(0,7,0.5))
tm_shape(nc) + 
  tm_fill(c("rr74", "ganador74"), title = "rr", palette = "Reds",
          auto.palette.mapping=FALSE, breaks = brks) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Ajuste del modelo CAR periodo 74", main.title.size = 1 ,
            legend.position = c("right", "top"), legend.title.size = 0.8,
            panel.labels=c("Real", "Ajustada"), legend.outside=TRUE)+
  tm_text("CRESS_ID", size = 0.6)

brks <- c(seq(0,5,0.5))
tm_shape(nc) + 
  tm_fill(c("rr79", "ganador79"), title = "rr", palette = "Reds",
          auto.palette.mapping=FALSE, breaks = brks) +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Ajuste del modelo CAR periodo 79", main.title.size = 1 ,
            legend.position = c("right", "top"), legend.title.size = 0.8,
            panel.labels=c("Real", "Ajustada"), legend.outside=TRUE) +
  tm_text("CRESS_ID", size = 0.6)


# Simulaciones ------------------------------------------------------------

# n = 1000
# 
# faux<-function(x){
#   rnorm(n,x[1],x[2])
# }
# 
# sum_gan <- summary(fit74_4)
# betas_sim <- apply(X = sum_gan$Coef[,1:2],MARGIN = 1,FUN = faux)
# #View(betas_sim)
# 
# X <- data.frame(ID=nc$CRESS_ID,unos=rep(1,100), BIR=nc$BIR74)
# X <- mutate(X,
#             B12=ifelse(both=="1:2",1,0),
#             B13=ifelse(both=="1:3",1,0),
#             B14=ifelse(both=="1:4",1,0),
#             B21=ifelse(both=="2:1",1,0),
#             B22=ifelse(both=="2:2",1,0),
#             B23=ifelse(both=="2:3",1,0),
#             B24=ifelse(both=="2:4",1,0),
#             B31=ifelse(both=="3:1",1,0),
#             B32=ifelse(both=="3:2",1,0),
#             B33=ifelse(both=="3:3",1,0),
#             B34=ifelse(both=="3:4",1,0),
#             B43=ifelse(both=="4:3",1,0),
#             )
# X <- select(X,-both)
# #View(X)
# 
# # dim(X)
# # dim(betas_sim)
# # sum(t(X[X$ID==1,])*t(betas_sim[1,]))
# 
# simula <- matrix(0,
#                  nrow = 100,
#                  ncol = n)
# #t(betas_sim[1,]*X[X$ID==1,-1])
# for(i in 1:100) {
#   for (j in 1:n) {
#     simula[i, j] = sum(X[X$ID == i, -1] * betas_sim[j, ])
#   }
# }
# View(simula)
# 
# Simulaciones <- list()
# Simulaciones$s74 <- simula
# 
# rm(Simulaciones)
# 
# 
# save(Simulaciones,file = "Simulaciones.RData")
load(file = "Simulaciones.RData")

pvalues = c()
for(i in 1:100){
  
  pvalues[i]<-ks.test(Simulaciones$s79[i,],Simulaciones$s74[i,])$p.value
  
}
ks.test(Simulaciones$s79[1,],Simulaciones$s74[1,])
View(Simulaciones$s74)

Resultados <- data.frame(ID=1:100,
                         p.value=pvalues,
                         Desición=ifelse(pvalues>0.05,
                                         "No hubo cambio",
                                         "Hubo Cambio"))
#(Pregunta 1)
View(Resultados)

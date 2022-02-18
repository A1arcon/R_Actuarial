

# Carga de funciones y librerías ------------------------------------------
library(mice)
library(dplyr)

# Los siguientes scripts están disponibles en el siguiente enlace:
# https://github.com/A1arcon/R_Actuarial/tree/main/_Edgar%20Package_
source("~/Actuaría/GitHub/R_Actuarial/_Edgar Package_/mis_funciones.R")


# Datos a utilizar --------------------------------------------------------

#
datos <- airquality
#

summary(datos)
head(datos) %>% write_clipboard()


# Imputación Simple -------------------------------------------------------

# ~ Por media -------------------------------------------------------------


#
imp <- mice(datos, method = "mean", m = 1)
datos_imp <- imp %>% mice::complete(action="long") %>% dplyr::select(names(datos))
#

# Verificación
apply(datos,MARGIN = 2,FUN = mean,na.rm=TRUE)

# Copiamos para hacer la tabla
head(datos_imp) %>% round(2) %>% write_clipboard()


# ~~ Gráficos -------------------------------------------------------------

df <- rbind(datos[,c("Ozone","Solar.R")],datos_imp[,c("Ozone","Solar.R")])
df$Datos <- c(rep("Originales",nrow(datos)),rep("Imputados",nrow(datos_imp)))
color_vector = c("red","darkblue")

# Para el Ozono
plot_multi_histogram_density(df = df,feature = "Ozone",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_mean_ozone
hist_mean_ozone

# Para la Radiación Solar
plot_multi_histogram_density(df = df,feature = "Solar.R",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_mean_solar.r
hist_mean_solar.r


# Ambas variables
datos_imp %>% with(plot(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone"))
fondo_plot()
datos_imp %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[1]))
datos %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[2]))


# ~ Imputación por regresión estocástoca ------------------------------------

#
imp <- mice(datos, seed = 1,
            method = "norm.nob", 
            m = 1, print = FALSE)
datos_imp <- imp %>% mice::complete(action="long") %>% dplyr::select(names(datos))
#

# Copiamos para hacer la tabla
head(datos_imp) %>% round(2) %>% write_clipboard()

# ~~ Gráficos -------------------------------------------------------------

df <- rbind(datos[,c("Ozone","Solar.R")],datos_imp[,c("Ozone","Solar.R")])
df$Datos <- c(rep("Originales",nrow(datos)),rep("Imputados",nrow(datos_imp)))
color_vector = c("red","darkblue")


# Para el Ozono
plot_multi_histogram_density(df = df,feature = "Ozone",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_reg_ozone
hist_reg_ozone

# Para la Radiación Solar
plot_multi_histogram_density(df = df,feature = "Solar.R",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_reg_solar.r
hist_reg_solar.r


# Ambas variables
datos_imp %>% with(plot(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone"))
fondo_plot()
datos_imp %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[1]))
datos %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[2]))

# ~ Imputación por pmm ------------------------------------

#
imp <- mice(datos, seed = 1, 
            method = "pmm", 
            m = 1, print = FALSE)
datos_imp <- imp %>% 
             mice::complete(action="long") %>% 
             dplyr::select(names(datos))
#

# Copiamos para hacer la tabla
head(datos_imp) %>% round(2) %>% write_clipboard()

# ~~ Gráficos -------------------------------------------------------------

df <- rbind(datos[,c("Ozone","Solar.R")],datos_imp[,c("Ozone","Solar.R")])
df$Datos <- c(rep("Originales",nrow(datos)),rep("Imputados",nrow(datos_imp)))
color_vector = c("red","darkblue")


# Para el Ozono
plot_multi_histogram_density(df = df,feature = "Ozone",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_pmm_ozone
hist_pmm_ozone

# Para la Radiación Solar
plot_multi_histogram_density(df = df,feature = "Solar.R",
                             label_column = "Datos",
                             mean = FALSE,ylab = "Densidad",
                             alpha_dens = 0.35,
                             alpha_hist = 0.4,
                             color_vector = color_vector) -> hist_pmm_solar.r
hist_pmm_solar.r


# Ambas variables
datos_imp %>% with(plot(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone"))
fondo_plot()
datos_imp %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[1]))
datos %>% with(points(Solar.R,Ozone,xlab="Solar.R", ylab="Ozone",col=color_vector[2]))


# Imputación Múltiple -----------------------------------------------------

# Regresión quitando los datos faltantes (Listwise)
#
fit <- with(datos, lm(Ozone ~ Wind + Temp + Solar.R))
summary(fit)
#

aux <- summary(fit)
aux$coefficients %>% round(3) %>% write_clipboard()

# Regresión con Imputación Múltiple
#
imp <- mice(datos, seed = 1, m = 20, print = FALSE)
fit <- with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
summary(pool(fit))
#

aux <- summary(pool(fit))
aux[,-1]<- aux[,-1] %>% round(3)
aux %>% write_clipboard()

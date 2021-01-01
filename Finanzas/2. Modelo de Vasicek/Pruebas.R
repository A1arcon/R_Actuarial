

# Valuación del Bono ------------------------------------------------------

# Tenemos un ejemplo aquí de la valuación de un Bono con Vasicek

# https://www.r-bloggers.com/2010/04/fun-with-the-vasicek-interest-rate-model/

n <- function(t, tf, b) {
  1 / b * (1 - exp(-b * (tf - t)))
}

n2 <- function(t, tf, b) {
  (1 / b * (1 - exp(-b * (tf - t)))) ^ 2
}


m <- function(t, tf, sigma, a, b) {
  
  int1 = integrate(
    n2,
    lower = t,
    upper = tf,
    tf = tf,
    b = b
  )$value
  
  int2 = integrate(
    n,
    lower = t,
    upper = tf,
    tf = tf,
    b = b
  )$value
  sigma ^ 2 / 2 * int1 - a * int2
  
}

Bono <- function(t,tf,a,b,sigma,s = 0,rs = 0.05) {
  # Función para obtener la esperanza de rt.
  rt <- function(t, s, rs, a, b) {
    rs * exp(-b * (t - s)) + a / b * (1 - exp(-b * (t - s)))
  }
  
  aux <- function(t, tf, a, b, sigma, s, rs) {
    exp(m(t, tf, sigma, a, b) - n(t, tf, b) * rt(t, s, rs, a, b))
  }
  
  sapply(t,aux,tf = tf,a = a,b = b,sigma = sigma,s = s,rs = rs)
  
}

# En el ejemplo de la página de internet:
# a = k*theta
# b = k
# sigma = beta
# Esto lo sabemos comparando 


Bono(t = 0,tf = 1,a = 0.1*0.3,b = 0.3,sigma = 0.03,s = 0,rs = 0.03)

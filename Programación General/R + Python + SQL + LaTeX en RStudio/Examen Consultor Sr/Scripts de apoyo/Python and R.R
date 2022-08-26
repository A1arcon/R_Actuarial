
# Estas son las librerías que son necesarias para el ejercicio. 
# Al usar Python las instalamos así en RStudio:
library(reticulate)
py_install("pandas")
py_install("fsspec")
py_install("string")
py_install("unidecode")
py_install("openpyxl")
py_install("matplotlib")
py_install("sklearn",pip = TRUE)

# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""


import pandas as pd
# a) Importar los datos
datos = pd.read_csv("C://Users/alarc/Downloads/Examen Consultor Sr/Nombres.txt",
                    sep="|",encoding=("UTF-8"))
datos

# b) 

# Asumiremos que también se deben quitar las mayúsculas con el objetivo de tener monotonía.

# Nombre
import string
simbolos = str.maketrans('', '', string.punctuation)
datos["Nombre"] = datos["Nombre"].str.translate(simbolos)
datos["Nombre"] = datos["Nombre"].str.replace(" ","")
datos["Nombre"] = datos["Nombre"].str.lower()
import unidecode
datos["Nombre"] = datos["Nombre"].apply(unidecode.unidecode)

# Apellido
datos["Apellido"] = datos["Apellido"].str.translate(simbolos)
datos["Apellido"] = datos["Apellido"].str.replace(" ","")
datos["Apellido"] = datos["Apellido"].str.lower()
datos["Apellido"] = datos["Apellido"].apply(unidecode.unidecode)

datos

# c)
datos["First"] = datos["Apellido"].str[0]
datos

# d)
datos_consonantes = datos[~(datos.First.isin(["a","e","i","o","u"]))]
datos_consonantes

# e)
datos_agrupado = datos_consonantes.groupby(["Nombre","Apellido","Id"]).sum()[["Saldo"]].reset_index()

# f)
import openpyxl
writer = pd.ExcelWriter('C://Users/alarc/Downloads/Examen Consultor Sr/Nombres_agrupado.xlsx')
datos_agrupado.to_excel(writer)
writer.save()


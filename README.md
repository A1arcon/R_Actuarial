# "R Actuarial" por Edgar Gerardo Alarcón González
Un repositorio con códigos de R para aplicaciones actuariales. Notas, tareas, exámenes, scripts, etc.

## Teoría del Riesgo (actualizado al 5/11/2020)
Subimos el curso completo de Teoría del Riesgo que se ha impartido en diversos semestres en la facultad de Ciencias de la UNAM por su servidor. El curso tiene un enfoque principalmente basado en temas teoricos y prácticos necesarios para presentar los exámenes P, FM, IFM y STAM de la SoA (Society Of Actuaries).

[Carpeta de Teoría del Riesgo](https://github.com/A1arcon/R_Actuarial/tree/main/Teor%C3%ADa%20del%20Riesgo%20(UNAM))

## Cápsuas AMAT (actualizado al 8/11/2020)

Aquí encontrarán los códigos que fueron utilizados para la elaboración de las cápsulas en las que participo en el canal de Youtube de AMAT.

1. [**Número de reclamaciones - Distribuciones de Clase (a, b, 0) en R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/1.%20Clase(a%2Cb%2C0)) [(Video)](https://www.youtube.com/watch?v=ZX2W59Mdaag&t)

El Act. Edgar Alarcón nos plática sobre una clase de familias muy importantes para modelar variables aleatorias discretas que representan la frecuencia del número de reclamaciones en un seguro, analizando un ejemplo de su implementación en R. En el [video](https://www.youtube.com/watch?v=ZX2W59Mdaag&t) se menciona que es posible demostrar que las distribuciones de clase (a,b,0) propuestas son de hecho las únicas que satisfacen la propiedad recursiva, dicha demostración la podrás ver dando clic [aquí](https://github.com/A1arcon/R_Actuarial/blob/main/C%C3%A1psulas%20AMAT/1.%20Clase(a%2Cb%2C0)/Unicidad%20de%20Clase(a%2Cb%2C0).pdf).

2. [**Modelo de pérdidas agregadas - Formula de Panjer en R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/2.%20Panjer) [(Video)](https://www.youtube.com/watch?v=H4ETDaUUvTk&t)

El Act. Edgar Alarcón nos habla sobre un modelo fundamental para describir el riesgo de una aseguradora: El Modelo de Pérdida Agregada, S=X1+...+XN. Para esto, hace uso de las propiedades de la familia de clase (a,b,0) y la fórmula recursiva de Panjer para obtener la distribución de S, su media y su varianza. Finalmente, implementa las fórmulas en R Studio, con la paquetería "actuar".

3. [**Función de Verosimilitud y Estimación de Parámetros con R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/3.%20Estimador%20m%C3%A1ximo%20veros%C3%ADmil) [(Video)](https://www.youtube.com/watch?v=zPSj0ltrBoc&t)

El Act. Edgar Alarcón nos habla sobre uno de los elementos estadísticos más importantes, la función de verosimilitud y, en particular, sobre un tema muy importante como es la estimación de parámetros por el método de máxima verosimilitud.

Finalmente, implementa las fórmulas en R Studio, mostrándonos una sencilla aplicación en R Shiny, donde podremos visualizar un ejemplo teórico-práctico de lo desarrollado a lo largo del video.

4. [**Integración de Regiones vía Monte Carlo, con R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/4.%20y%205.Integraci%C3%B3n%20Monte%20Carlo) [(Video)](https://www.youtube.com/watch?v=Lot8__UCG5U)

En la cápsula de esta semana, el Act. Edgar Alarcón, nos muestra una interesante aplicación del Método de Simulación Montecarlo para estimar la medida (longitud, área, volumen o hipervolumen) de un subconjunto del espacio Euclidiano con ayuda de R-Studio. 

5. [**Integración de Funciones vía Monte Carlo con R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/4.%20y%205.Integraci%C3%B3n%20Monte%20Carlo) [(Video)](https://www.youtube.com/watch?v=YiMNZ8Hv6w0&t)

En la cápsula de esta semana, el Act. Edgar Alarcón, nos muestra una interesante aplicación del Método de Simulación Montecarlo para estimar una integral definida (área bajo la curva en una región) usando teoremas sencillos de probabilidad y mostrándonos el código de esta aplicación en R-Studio y una aplicación en Shiny que realiza todo el proceso. 

- [Carpeta con los scripts de las cápsulas](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT)

- [Canal de Youtube de AMAT](https://www.youtube.com/channel/UCuqKbVjXExMCqpVhLYy2aog)

## Probabilidad (Actualizado al 7/11/2020)

1. [**Votos (Bertrand's ballot theorem)**](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad/1.%20Votos%20(Bertrand's%20ballot%20theorem))

El candidato A obtiene n votos y B obtiene m con n > m. ¿Cuál es la probabilidad que A siempre vaya en la delantera en los conteos?. En este código explicamos la solución de una forma un tanto eurística y verificamos el resultado haciendo uso de simulaciones.

2. [**La paradoja del cumpleaños**](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad/2.%20Cumplea%C3%B1os) [(Video)](https://www.youtube.com/watch?v=7uzx6D_0V7M)

Para ponerlos en contexto, se siguiere ver el siguiente [video](https://www.youtube.com/watch?v=7uzx6D_0V7M) (clic). En este código vamos a verificar vía simulaciones que en efecto esta paradoja es cierta y las probabilidades exactas se comportan de acuerdo al comportamiento estocástico del experimento.

- [Carpeta de todos los ejercicios](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad)

## Esperen más próximamente...

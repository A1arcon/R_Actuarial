
<a href="https://www.unam.mx/">
<img src="https://img.shields.io/badge/Actuar%C3%ADa-UNAM-yellow">

<a href="http://www.fciencias.unam.mx/directorio/80028">
<img src="https://img.shields.io/badge/Docente-Fac.%20Ciencias-yellowgreen">

<a href="https://amat.mx/cursos">
<img src="https://img.shields.io/badge/Docente-AMAT-red">

# [“R Actuarial”](https://github.com/A1arcon/R_Actuarial) por [Edgar Gerardo Alarcón González](https://www.facebook.com/EdgarGerardoAlarconGonzalez)

El objetivo de este repositorio es brindar un apoyo a la comunidad
interesada en mejorar sus técnicas en el lenguaje de programación R o
emprenderlo desde un punto de vista muy aplicado. Un repositorio con
códigos de R para aplicaciones actuariales: probabilidad, estadística,
riesgo y finanzas.

## [Teoría del Riesgo](https://github.com/A1arcon/R_Actuarial/tree/main/Teor%C3%ADa%20del%20Riesgo%20(UNAM)) (actualizado al 5/11/2020)

Subimos el curso completo de Teoría del Riesgo que se ha impartido en
diversos semestres en la facultad de Ciencias de la UNAM por su
servidor. El curso tiene un enfoque principalmente basado en temas
teoricos y prácticos necesarios para presentar los exámenes P, FM, IFM y
STAM de la SoA (Society Of Actuaries).

## [Cápsulas AMAT](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT) (actualizado al 8/11/2020) [Canal de YouTube - AMAT](https://www.youtube.com/channel/UCuqKbVjXExMCqpVhLYy2aog)

<img src="README_files/figure-gfm/AMAT.PNG" width="50%" style="display: block; margin: auto;" />

Aquí encontrarán los códigos que fueron utilizados para la elaboración
de las cápsulas en las que participo en el canal de Youtube de AMAT.

1.  [**Número de reclamaciones - Distribuciones de Clase (a, b, 0) en
    R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/1.%20Clase(a%2Cb%2C0))
    [(Video)](https://www.youtube.com/watch?v=ZX2W59Mdaag&t)

El Act. Edgar Alarcón nos plática sobre una clase de familias muy
importantes para modelar variables aleatorias discretas que representan
la frecuencia del número de reclamaciones en un seguro, analizando un
ejemplo de su implementación en R. En el
[video](https://www.youtube.com/watch?v=ZX2W59Mdaag&t) se menciona que
es posible demostrar que las distribuciones de clase (a,b,0) propuestas
son de hecho las únicas que satisfacen la propiedad recursiva, dicha
demostración la podrás ver dando clic
[aquí](https://github.com/A1arcon/R_Actuarial/blob/main/C%C3%A1psulas%20AMAT/1.%20Clase(a%2Cb%2C0)/Unicidad%20de%20Clase(a%2Cb%2C0).pdf).

2.  [**Modelo de pérdidas agregadas - Formula de Panjer en
    R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/2.%20Panjer)
    [(Video)](https://www.youtube.com/watch?v=H4ETDaUUvTk&t)

El Act. Edgar Alarcón nos habla sobre un modelo fundamental para
describir el riesgo de una aseguradora: El Modelo de Pérdida Agregada,
S=X1+…+XN. Para esto, hace uso de las propiedades de la familia de clase
(a,b,0) y la fórmula recursiva de Panjer para obtener la distribución de
S, su media y su varianza. Finalmente, implementa las fórmulas en R
Studio, con la paquetería “actuar”.

3.  [**Función de Verosimilitud y Estimación de Parámetros con
    R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/3.%20Estimador%20m%C3%A1ximo%20veros%C3%ADmil)
    [(Video)](https://www.youtube.com/watch?v=zPSj0ltrBoc&t)

El Act. Edgar Alarcón nos habla sobre uno de los elementos estadísticos
más importantes, la función de verosimilitud y, en particular, sobre un
tema muy importante como es la estimación de parámetros por el método de
máxima verosimilitud.

Finalmente, implementa las fórmulas en R Studio, mostrándonos una
sencilla aplicación en R Shiny, donde podremos visualizar un ejemplo
teórico-práctico de lo desarrollado a lo largo del video.

4.  [**Integración de Regiones vía Monte Carlo, con
    R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/4.%20y%205.Integraci%C3%B3n%20Monte%20Carlo/4.%20Integraci%C3%B3n%20de%20Regiones%20v%C3%ADa%20Monte%20Carlo)
    [(Video)](https://www.youtube.com/watch?v=Lot8__UCG5U)

En la cápsula de esta semana, el Act. Edgar Alarcón, nos muestra una
interesante aplicación del Método de Simulación Montecarlo para estimar
la medida (longitud, área, volumen o hipervolumen) de un subconjunto del
espacio Euclidiano con ayuda de R-Studio.

5.  [**Integración de Funciones vía Monte Carlo con
    R**](https://github.com/A1arcon/R_Actuarial/tree/main/C%C3%A1psulas%20AMAT/4.%20y%205.Integraci%C3%B3n%20Monte%20Carlo/5.%20Integraci%C3%B3n%20de%20Funciones%20v%C3%ADa%20Monte%20Carlo)
    [(Video)](https://www.youtube.com/watch?v=YiMNZ8Hv6w0&t)

En la cápsula de esta semana, el Act. Edgar Alarcón, nos muestra una
interesante aplicación del Método de Simulación Montecarlo para estimar
una integral definida (área bajo la curva en una región) usando teoremas
sencillos de probabilidad y mostrándonos el código de esta aplicación en
R-Studio y una aplicación en Shiny que realiza todo el proceso.

## [Estadística](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica) (Actualizado al 26/01/2021)

El objetivo de mostrar estos documentos es dar ejemplos de cómo puede
estár organizado un reporte estadístico, ya sea mostrando y ejecutando
código de R o bien únicamente mostrando los resultados obtenidos, aún
cuando el documento detrás sí ejecute los comandos requeridos.

1.  [**Modelos con tablas de
    contingencia**](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica/1.%20Modelos%20con%20tablas%20de%20contingencia)

Este
[archivo](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/1.%20Modelos%20con%20tablas%20de%20contingencia/Reporte.Rmd)
busca principalmente ejemplificar el uso de tablas con apoyo de la
paquetería ‘kable’, gráficos con ‘ggplot’ y la técnica de Macros en R en
un archivo Markdown. El
[documento](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/1.%20Modelos%20con%20tablas%20de%20contingencia/Reporte.pdf)
habla sobre diversos modelos que se pueden aplicar a tablas de
contingencia “Independencia”, “Cuasi-Independencia”, “Homogeneidad
Marginal”, “Simetría” y “Cuasi-Simetría”. Una aplicación de los modelos
lineales generalizados y datos categóricos.

2.  [**Predicción de la precipitación en estados al noroeste de
    México**](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica/2.%20Predicci%C3%B3n%20de%20la%20precipitaci%C3%B3n%20en%20estados%20al%20noroeste%20de%20M%C3%A9xico)

<img src="README_files/figure-gfm/Noroeste_Mex.PNG" width="50%" style="display: block; margin: auto;" />

Este
[archivo](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/2.%20Predicci%C3%B3n%20de%20la%20precipitaci%C3%B3n%20en%20estados%20al%20noroeste%20de%20M%C3%A9xico/Proyecto%20RMD.Rmd)
es un
[proyecto](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/2.%20Predicci%C3%B3n%20de%20la%20precipitaci%C3%B3n%20en%20estados%20al%20noroeste%20de%20M%C3%A9xico/Proyecto-RMD.pdf)
(es un poco largo, así que tendrán que descargar el archivo para verlo)
que realicé en una clase de estadística espacial. El objetivo es
pronosticar la precipitación en ciertas zonas de México, por los estados
de Baja California Norte y Sur, Sonora y Sinaloa. En este R Markdown
encontrarán un ejemplo de cómo poner un índice o tabla de contenido
(table of contents), así como para hacer gráficos de calor con `ggplot`.
Además, viene un ejemplo de cómo conectar un RMarkdown de formato `pdf`
con otros archivos del mismo formato, lo que se hizo aquí fue conectar
con una portada para el trabajo, esto pues no siempre es sencillo
diseñar algo de este estilo en R Markdown. Por lo mismo, para que el
documento trabaje bien, necesitarán descargar toda la
[carpeta](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica/2.%20Predicci%C3%B3n%20de%20la%20precipitaci%C3%B3n%20en%20estados%20al%20noroeste%20de%20M%C3%A9xico)
que contiene el archivo y posteriormente compilarlo. Recuerden hacer una
instalación correcta de
[MikTek](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown/0.%20MikTex).
¡No olviden cambiar su directorio de trabajo!

3.  [**Inlfuencia espacial en
    enfermedades**](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica/3.%20Inlfuencia%20espacial%20en%20enfermedades)

El
[proyecto](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/3.%20Inlfuencia%20espacial%20en%20enfermedades/Proyecto-2-RMD.pdf)
tiene como objetivo **estimar el riesgo relativo** de muerte de cuna
para los años 1974-1978 y 1979-1984 para el estado de Carolina del Norte
por condado. Para esto, comenzaremos dando una introducción de qué es la
muerte de cuna y algunos otros datos derivados de esta enfermedad.
Posteriormente vamos a mostrar un análisis descriptivo de los datos con
los que contamos para realizar un modelo estadístico que pueda estimar
el riesgo relativo por condado. Luego, realizamos un modelo lineal
generalizado que no contempla el efecto espacial con la finalidad de
mostrar que es necesario acudir a esta rama de la estadística. Una vez
expuesto el punto anterior, realizamos un par de modelos espaciales del
tipo SAR y CAR, para finalmente buscar el mejor modelo por periodo y
culminar respondiendo a tres preguntas importantes derivadas de este
análisis.

4.  [**Modelos Lineales Generalizados sobre Alumnos de
    Maestría**](https://github.com/A1arcon/R_Actuarial/tree/main/Estad%C3%ADstica/4.%20Modelos%20Lineales%20Generalizados%20sobre%20Alumnos%20de%20Maestr%C3%ADa)

Los datos que se trabajan en este
[documento](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/4.%20Modelos%20Lineales%20Generalizados%20sobre%20Alumnos%20de%20Maestr%C3%ADa/Proyecto.pdf)
son
[datos](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/4.%20Modelos%20Lineales%20Generalizados%20sobre%20Alumnos%20de%20Maestr%C3%ADa/edgar.csv)
de alumnos/aspirantes que entraron a la “Maestría en Ciencias
Matemáticas” por parte del programa “Maestría y Doctorado en Ciencias
Matemáticas y de la Especialización en Estadística Aplicada” de la
Unidad de Posgrado de la Universidad Nacional Autónoma de México (UNAM).
Como parte de la labor administrativa, es de interés que los
alumnos/aspirentes logren el objetivo de titularse, esto debido al alto
nivel académico que se maneja y reporta a instituciones como el Consejo
Nacional de Ciencia y Tecnología (CONACyT).

Por tal motivo, la elección correcta de los aspirantes resulta ser un
aspecto fundamental para prgoresar con dicho rendimiento y desempeño
académico. De tal manera que se ha recopilado información estadística de
algunos de los aspirantes a entrar a la maestría de los semestres 2015-1
al 2019-2 con el objetivo de buscar alguna relación que favorezca la
posibilidad de graduarse del grado académico al que se aspira. Todo esto
se realizará utilizando los conceptos más fundamentales de Modelos
Lineales Generalizados (MLG). El software que estaremos trabajando será
[R](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/4.%20Modelos%20Lineales%20Generalizados%20sobre%20Alumnos%20de%20Maestr%C3%ADa/Auxiliar.R)
y el reporte estará hecho en [R
Markdown](https://github.com/A1arcon/R_Actuarial/blob/main/Estad%C3%ADstica/4.%20Modelos%20Lineales%20Generalizados%20sobre%20Alumnos%20de%20Maestr%C3%ADa/Proyecto.Rmd).

## [Probabilidad](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad) (Actualizado al 7/11/2020)

Aquí encontrarán ejercicios e implementaciones, con sus respectivas
aplicaciones en el lenguaje de programación estadística R relacionados
con la probabilidad; una de las ramas en matemáticas más apasionantes e
importantes.

1.  [**Votos (Bertrand’s ballot
    theorem)**](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad/1.%20Votos%20(Bertrand's%20ballot%20theorem))

El candidato A obtiene n votos y B obtiene m con n &gt; m. ¿Cuál es la
probabilidad que A siempre vaya en la delantera en los conteos?. En este
código explicamos la solución de una forma un tanto eurística y
verificamos el resultado haciendo uso de simulaciones.

2.  [**La paradoja del
    cumpleaños**](https://github.com/A1arcon/R_Actuarial/tree/main/Probabilidad/2.%20Cumplea%C3%B1os)
    [(Video)](https://www.youtube.com/watch?v=7uzx6D_0V7M)

Para ponerlos en contexto, se siguiere ver el siguiente
[video](https://www.youtube.com/watch?v=7uzx6D_0V7M) (clic). En este
código vamos a verificar vía simulaciones que en efecto esta paradoja es
cierta y las probabilidades exactas se comportan de acuerdo al
comportamiento estocástico del experimento.

## [Finanzas](https://github.com/A1arcon/R_Actuarial/tree/main/Finanzas) (Actualizado al 5/1/2021)

1.  [**Tiempo continuo** -
    1](https://github.com/A1arcon/R_Actuarial/tree/main/Finanzas/1.%20Tiempo%20continuo%20-%201)
    (Documento en
    [Overleaf](https://www.overleaf.com/read/zntgzskstfyh))

Del [examen general de finanzas matematicas del posgrado en ciencias
matematicas, enero
2017](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/1.%20Tiempo%20continuo%20-%201/FinanzasMate2017-1.pdf),
resolveremos los ejercicios 3 y 4, relativos a finanzas en tiempo
continuo. En [este
documento](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/1.%20Tiempo%20continuo%20-%201/Derivados_a_Tiempo_Continuo.pdf)
veremos las soluciones escritas junto con imágenes, `LaTeX` y `R`. [Este
script](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/1.%20Tiempo%20continuo%20-%201/main.Rtex)
fue realizado completamente online en la página
[Overleaf](https://www.overleaf.com/). Este es un buen ejemplo para
poder usar esta plataforma y juntar estos dos lenguajes de programación
en un estilo muy similar a un `R Sweave`.

2.  [**Modelo de
    Vasicek**](https://github.com/A1arcon/R_Actuarial/tree/main/Finanzas/2.%20Modelo%20de%20Vasicek)
    (Presentación en
    [**Beamer**](https://drive.google.com/file/d/1VRBjHOOX-RQ7fW1sa8DtAOCTWhXkDV63/view?usp=sharing)
    y aplicación en
    [**Shiny**](https://edgar-alarcon.shinyapps.io/Modelo_Vacisek/?_ga=2.81008535.1011404247.1609457667-842461214.1606672993))

Esta es una breve e introductoria explicación de un modelo de tasas de
interés bajo una medida martingala de no arbitraje para el cálculo de
tasas spot. Aquí mostramos un ejemplo de cómo hacer una
[presentación](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/2.%20Modelo%20de%20Vasicek/Presentation/Modelo-de-Vasicek.pdf)
en Beamer usando `R Markdown`. Lo interesante de este documento es que
muestra cómo ligar un `R Markdown` con un
[preámbulo](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/2.%20Modelo%20de%20Vasicek/Presentation/preamble.tex)
`.tex`. Más aún, se ve cómo usar diferentes colores para los bloques en
Beamer, poner imágenes, establecer un tema, poner pies de página, urls,
etc. Con el objetivo de ver de forma un tanto más interactiva la teoría
de la que el trabajo menciona, se desarrolló también una aplicación en
[Shiny](https://edgar-alarcon.shinyapps.io/Modelo_Vacisek/?_ga=2.81008535.1011404247.1609457667-842461214.1606672993).
Podrán acceder a todos los enlaces importantes que tiene este documento
descargando la
[presentación](https://github.com/A1arcon/R_Actuarial/blob/main/Finanzas/2.%20Modelo%20de%20Vasicek/Presentation/Modelo-de-Vasicek.pdf)
en `.pdf`.

## [R Markdown](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown) (Actualizado al 15/01/2021)

Lo mejor de dos mundos LaTeX + R. Los archivos del estilo R Markdown
permiten al usuario escribir ecuaciones, instertar gráficos, crear
tablas de datos y escribir código de R con el apoyo de LaTeX todo en un
mismo archivo. Estos archivos pueden llevar una presentación técnica al
nivel más alto.

**IMPORTANTE**

Se recomienda la instalación de
[MikTek](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown/0.%20MikTex)
antes de comenzar para poder compilar archivos del tipo `pdf` y así
poder usar herramientas más fuertes de LaTeX.

1.  [**Introducción a R
    Markdown**](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown/1.%20Introducci%C3%B3n%20a%20R%20Markdown)

[Aquí](https://github.com/A1arcon/R_Actuarial/blob/main/R%20Markdown/1.%20Introducci%C3%B3n%20a%20R%20Markdown/Introducci%C3%B3n%20a%20R%20Markdown.pdf)
damos una introducción a la realización de reportes con R Markdown, las
reglas básicas y comandos más importantes se comentan en estos archivos
que son a su vez un ejemplo de cómo empezar a usar este tipo de
archivos.

> Adicionalmente, si ya eres experimentado con RMarkdown y si desean
> subir un archivo con formato `.md` en **GitHub** con ayuda de RStudio,
> pueden dar clic
> [aquí](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown/1.%20Introducci%C3%B3n%20a%20R%20Markdown/GitHub)
> para explorar una carpeta donde vemos un
> [ejemplo](https://github.com/A1arcon/R_Actuarial/blob/main/R%20Markdown/1.%20Introducci%C3%B3n%20a%20R%20Markdown/GitHub/R-Markdown---GitHub.md).

2.  [**Blocks de Colores en
    Beamer**](https://github.com/A1arcon/R_Actuarial/tree/main/R%20Markdown/2.%20Blocks%20de%20Colores)

En este
[documento](https://github.com/A1arcon/R_Actuarial/blob/main/R%20Markdown/2.%20Blocks%20de%20Colores/Blocks_de_Colores.pdf)
en formato de presentación Beamer escrito en un
[archivo](https://github.com/A1arcon/R_Actuarial/blob/main/R%20Markdown/2.%20Blocks%20de%20Colores/Blocks_de_Colores.RMD)
R Markdown mostramos el uso de una serie de funciones creadas en formato
`LaTeX` en un
[preámbulo](https://github.com/A1arcon/R_Actuarial/blob/main/R%20Markdown/2.%20Blocks%20de%20Colores/preamble.tex)
que nos permitirán crear los conocidos “Blocks” de diferentes colores en
un ambiente de `LaTeX`.
[Beamer](https://es.overleaf.com/learn/latex/beamer) es una herramienta
de `LaTeX` sumamente utilizada en el ámbito matemático para hacer
presentaciones. Resulta que R permite hacer este tipo de presentaciones
en su modalidad R Markdown. Más aún, es posible ligar un preámbulo
escrito en la extensión `.tex` a un archivo R Markdown. Este documento
es un ejemplo de ello. Para ver más colores, da clic
[aquí](https://en.wikibooks.org/wiki/LaTeX/Colors).

## [Paquetería personal](https://github.com/A1arcon/R_Actuarial/tree/main/_Edgar%20Package_) (Actualizado al 14/5/2021)

Este apartado está dedicado a funciones que he creado con la finalidad
de realizar tareas repetitivas que pueden consumir bastante código y
tiempo. Los scripts de este apartado vienen tienen como objetivo lo
siguiente:

1.  [`Funciones_RMD.R`](https://github.com/A1arcon/R_Actuarial/blob/main/_Edgar%20Package_/Funciones_RMD.R):
    son funciones que facilitan la creación de documentos con
    `R Markdown` en `LaTeX`, la elaboración de tablas, entre otras
    entidades para `R Markdown`. Con este `script`, por ejemplo, la
    elaboración de una tabla como la siguiente para un **modelo lineal
    generalizado** dado por la función `glm()` resulta ser cuestión de
    un par de líneas de código e incluso tendrá una referencia para que
    se pueda citar usando el comando `\autoref` o `\ref` de `LaTeX`.

``` r
# Creación del modelo
fit_gamma <- glm(Crash_Score~.,family = Gamma(link = "log"),data = June)
# Creación de la tabla
kabla_fit_glm(fit_gamma,
              title = "Estadísticas del modelo lineal generalizado (Gamma) $log\\left(\\mathbb{E}[y_i]\\right) = \\underline{\\beta}^T X_i$",
              ref="fit_gamma_tabla")
```

<img src="README_files/figure-gfm/Tabla_GLM.PNG" width="50%" style="display: block; margin: auto;" />

2.  [`mis_funciones.R`](https://github.com/A1arcon/R_Actuarial/blob/main/_Edgar%20Package_/mis_funciones.R):
    son funciones que facilitan la creación y manipulación
    principalmente de gráficos. Este `script` está principalmente
    cimentado en la librería `ggplot` y llevado a un nivel fácil de usar
    para cualquier usuario de R y podremos tener gráficos como los
    siguientes

``` r
# Datos
X <- runif(100,-5,5)
Y <- ifelse(test = X>=0,yes = X^2+2,no = X^2-2)
Ymin <- Y + abs(rnorm(length(X),sd=2))
Ymax <- Y - abs(rnorm(length(X),sd=2))
Label_column <- ifelse(test = X>=0,yes = "Negativo",no = "Positivo")
Data <- data.frame(X,Y,Ymin,Ymax,Label_column)

# Gráfico
ggplot_time_series(data = Data,x = "X",y = "Y",
                   label_column = "Label_column",
                   ymin = "Ymin",ymax = "Ymax",
                   title = "Parábola partida",
                   subtitle = "En positivos y negativos",
                   xlab = "X",
                   ylab = latex2exp::TeX("$Y=X^2 + '\\epsilon'$"),
                   alpha = 0.25,x_breaks = 10,y_breaks = 20)
```

<img src="README_files/figure-gfm/unnamed-chunk-5-1.png" width="50%" style="display: block; margin: auto;" />

Entre otros ejemplos…

<img src="README_files/figure-gfm/Serie_tiemp_hist.PNG" width="75%" style="display: block; margin: auto;" />

## [Biblioteca Digital](https://drive.google.com/drive/folders/1iZ4_kcita9R3TY32G-LRIMS9HXyteApH?usp=sharing)

Les comparto todos los libros digitales con los que cuento. Hay desde
temas de cálculo, álgebra, análisis; pasando por probabilidad,
estadística, riesgo, hasta mangas y libros de música. :) Ojalá les
sirva.

## Esperen más próximamente…

### Contacto

Estoy muy atento a los comentarios que me puedan hacer, así como para
atender dudas, apoyo o participación en proyectos.

Teléfono: (55) 8718-0868

Celular y what’s app: (55) 3912-0683

Correo: <alarcon@ciencias.unam.mx>

Quedo a sus órdenes y espero esto ayude al gremio.

### FAQ

-   [¿Cómo puedo conectar/subir archivos de R con GitHub de forma fácil
    y rápida?](https://youtu.be/te1fJgn71js)

-   [¿Cómo puedo descargar los archivos que tiene un repositorio en
    GitHub?](https://youtu.be/vuvp_-cPYNA)

-   [¿Cómo puedo descargar un archivo en particular de
    GitHub?](https://stackoverflow.com/questions/4604663/download-single-files-from-github)

> Una respuesta rápida a esta última pregunta es la siguiente. 1. Ve al
> archivo que quieres descargar. 2. Dale click para ver los contenidos
> dentro de la UI (*Interfaz de Usuario*) de GitHub. 3. En el lado
> superior derecho de la visualización del archivo, da **click derecho**
> en el botón que dice `Raw`. 4. Seleccionar la opción *save as*
> (guardar cómo) y el resto es seleccionar dónde lo deseamos guardar.

/*
Edades promedio,mínimas y máximas por ocupación/estado
Columnas:
. Clave del estado
. Nombre del estado
. Nombre de la ocupación
. Edad mínima de las personas que viven en el estado y tienen esa ocupación
. Edad máxima de las personas que viven en el estado y tienen esa ocupación
. Edad promedio de las personas que viven en el estado y tienen esa ocupación
Para todos los estados y ocupaciones.
Las edades deben incluír meses y días.
*/

SELECT e.claveestado,
	   e.nombre,
	   o.nombre,
	   MIN(age(now(),p.fnac)) AS minimo, 
	   MAX(age(now(),p.fnac)) AS maximo, 
	   AVG(age(now(),p.fnac)) AS promedio
FROM estado e	
JOIN municipio m USING (idestado)
JOIN asentamiento a USING (idmunicipio)
JOIN ocupacion o ON (TRUE)     
JOIN domicilio d USING (idasentamiento)
LEFT JOIN persona p USING (iddomicilio,idocupacion)
GROUP BY 1,2,3			   

/*
Pedidos por mes
Columnas:
. Año
. Nombre del mes
. Número de pedidos realizados en ese mes y ese año
*/
			    
select extract(year from fecha), to_char(fecha,'Month'),count(*) 
from pedido 
group by 1,2			   

with meses as(
SELECT * FROM (VALUES (1, 'Enero'), (2, 'Febrero'), (3, 'Marzo'), (4, 'Abril'), 
			   (5, 'Mayo'), (6, 'junio'), (7, 'julio'), (8, 'Agosto'), (9, 'Septiembre'),
			   (10, 'Octubre'), (11, 'Noviembre'), (12, 'Diciembre')) as m (Mes,NombreMes))
select extract(year from p.fecha), m.NombreMes,count (*)
from pedido p join meses m on extract(month from p.fecha) = m.mes
group by 1,2

WITH meses AS(
SELECT * FROM (VALUES (1, 'Enero'), (2, 'Febrero'), (3, 'Marzo'), (4, 'Abril'), 			  
			  (5, 'Mayo'), (6, 'junio'), (7, 'julio'), (8, 'Agosto'), (9, 'Septiembre'),			   
			  (10, 'Octubre'), (11, 'Noviembre'), (12, 'Diciembre')) AS m (Mes,NombreMes))
SELECT EXTRACT(YEAR FROM p.fecha), m.NombreMes, COUNT(*)
FROM pedido p JOIN meses m ON EXTRACT(MONTH FROM p.fecha) = m.mes
GROUP BY 1,2
															 
/*
Pedidos por edades:
Columnas:
. Edad en años
. Número de pedidos
Para los pedidos realizados por personas de esa edad.
*/

--Torito 3:
--En la solución de René para "pedidos por edades" ¿es indispensable el join con la tabla cliente?																			
																			
select extract(year from age(now(),fnac)) as edades,count(idpedido) as numpedidos
from pedido ped
	join cliente c using(idcliente)
	join persona p using(idcliente)					
group by 1							
order by 1 asc			   
			   
/*
Codiciosos
Columnas:
. [Todas las de la tabla persona]
. Número de pedidos
. Número total de artículos
Para la persona que tenga el mayor número de articulos totales considerando todos sus pedidos.
*/

/*
Hasta en las mejores familias
Columnas:
. Ap1
. Ap2
. Número de hombres
. Número de mujeres
. Edad promedio 
. Edad mínima
. Edad máxima
Para todas las "familias" en la BD (considerando que quienes tienen los mismos apellidos son de la misma familia)
*/

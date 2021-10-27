
WITH
bawr1 AS(
--Artículo
SELECT 'articulo' AS Nombre, 
		COUNT(idarticulo) AS Registros, 
		MAX(idarticulo) AS Máximo, 
		MIN(idarticulo) AS Mínimo
FROM articulo),

bawr2 AS(
--Asentamiento
SELECT 'asentamiento' AS Nombre, 
		COUNT(idasentamiento) AS Registros, 
		MAX(idasentamiento) AS Máximo, 
		MIN(idasentamiento) AS Mínimo
FROM asentamiento),

bawr3 AS(
--Cliente
SELECT 'cliente' AS Nombre, 
		COUNT(idcliente) AS Registros, 
		MAX(idcliente) AS Máximo, 
		MIN(idcliente) AS Mínimo
FROM cliente),

bawr4 AS(
--Domicilio
SELECT 'domicilio' AS Nombre, 
		COUNT(iddomicilio) AS Registros, 
		MAX(iddomicilio) AS Máximo, 
		MIN(iddomicilio) AS Mínimo
FROM domicilio),

bawr5 AS(
--Empresa
SELECT 'empresa' AS Nombre, 
		COUNT(idempresa) AS Registros, 
		MAX(idempresa) AS Máximo, 
		MIN(idempresa) AS Mínimo
FROM empresa),

bawr6 AS(
--Estado
SELECT 'estado' AS Nombre, 
		COUNT(idestado) AS Registros, 
		MAX(idestado) AS Máximo, 
		MIN(idestado) AS Mínimo
FROM estado),

bawr7 AS(
--Fábrica
SELECT 'fabrica' AS Nombre, 
		COUNT(idfabrica) AS Registros, 
		MAX(idfabrica) AS Máximo, 
		MIN(idfabrica) AS Mínimo
FROM fabrica),

bawr8 AS(
--Municipio
SELECT 'municipio' AS Nombre, 
		COUNT(idmunicipio) AS Registros, 
		MAX(idmunicipio) AS Máximo, 
		MIN(idmunicipio) AS Mínimo
FROM municipio),

bawr9 AS(
--Ocupación
SELECT 'ocupacion' AS Nombre, 
		COUNT(idocupacion) AS Registros, 
		MAX(idocupacion) AS Máximo, 
		MIN(idocupacion) AS Mínimo
FROM ocupacion),

bawr10 AS(
--Pedido
SELECT 'pedido' AS Nombre, 
		COUNT(idpedido) AS Registros, 
		MAX(idpedido) AS Máximo, 
		MIN(idpedido) AS Mínimo
FROM pedido),

bawr11 AS (
--Persona
SELECT 'persona' AS Nombre, 
		COUNT(idpersona) AS Registros, 
		MAX(idpersona) AS Máximo, 
		MIN(idpersona) AS Mínimo
FROM persona)

SELECT * FROM bawr1
UNION
SELECT * FROM bawr2
UNION
SELECT * FROM bawr3
UNION
SELECT * FROM bawr4
UNION
SELECT * FROM bawr5
UNION
SELECT * FROM bawr6
UNION
SELECT * FROM bawr7
UNION
SELECT * FROM bawr8
UNION
SELECT * FROM bawr9
UNION
SELECT * FROM bawr10
UNION
SELECT * FROM bawr11

/*
Jóvenes.
Columnas:
. Nomble del estado
. Nombre del municipio
. Número de habitantes del municipio
. [Datos] completos de persona
Para las personas más jóvenes de cada municipio.
*/

(SELECT *
FROM
	(SELECT m.idmunicipio, e.nombre as Estado, m.nombre as Municipio, COUNT(idpersona) as Población
	FROM estado e JOIN municipio m USING (idestado)
		 JOIN asentamiento a USING (idmunicipio)
	 	JOIN domicilio d USING (idasentamiento)
	 	JOIN persona p USING (iddomicilio)
	GROUP BY 1, 2, 3
	ORDER BY e.nombre) brl

	JOIN 

	(SELECT m.idmunicipio, MIN(AGE(now(), p.fnac))
	FROM municipio m JOIN asentamiento a USING (idmunicipio)
		 JOIN domicilio d USING (idasentamiento)
	 	JOIN persona p USING (iddomicilio)
	GROUP BY 1) bawr

USING (idmunicipio)

SELECT p.idpersona
FROM municipio m JOIN asentamiento a USING (idmunicipio)
	JOIN domicilio d USING (idasentamiento)
	JOIN persona p USING (iddomicilio)							   
GROUP BY 1
HAVING MIN(AGE(now(), p.fnac)) = AGE(now(), p.fnac)) miau

JOIN			   
							   
SELECT *
FROM municipio m JOIN asentamiento a USING (idmunicipio)
	JOIN domicilio d USING (idasentamiento)
	JOIN persona p USING (iddomicilio)
							   
				
									
									 
with agrupado as (
  select estado, municipio, count(*) nhabitantes, max(fnac) as fecha
  from tuspinchesdatos
  group by 1,2)
select b.*, a.idpersona, a.nombre, a.ap1, a.ap2, a.sexo, a.iddomicilio, a.idocupacion, a.idcliente
from tuspinchesdatos a
inner join agrupado b on (a.estado=b.estado and a.municipio=b.municipio and a.fnac=b.fecha)									 
							   

							   
							   
							   
							   
							   
							   
							   
							   
							   
							   
						




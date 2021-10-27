/*1.
NINIS
Columnas:
. [Todas las de la tabla persona]
Para las personas que no tienen registrada ninguna ocupación en la BD.
*/

SELECT * 
FROM persona
WHERE idocupacion IS NULL

/*2.
Distribución de ocupaciones
Columnas:
. [Todas las de la tabla ocupacion]
. número de personas que tienen esa ocupación en la BD.
*/

SELECT o.* , count(p.idpersona)
FROM ocupacion o LEFT JOIN persona p USING (idocupacion)
GROUP BY 1,2
ORDER BY 1

/*3.
¿Pidieron?
Columnas:
.[Todas las de la tabla persona]
Para las personas que tienen algún pedido registrado en la BD.
*/

SELECT per.*
FROM persona per NATURAL JOIN cliente c
	 NATURAL JOIN pedido ped
GROUP BY idpersona

/*4.
Pedinche
Columnas:
. [Todas las de la tabla persona]
Para la persona que tiene más pedidos registrados
*/

SELECT per.* , COUNT(idpersona) pedidos
FROM persona per NATURAL JOIN cliente c
	 NATURAL JOIN pedido ped
GROUP BY 1
ORDER BY pedidos DESC
LIMIT 8

/*
Empresas
Columnas:
. Clave de estado
. Número de empresas registradas en ese estado
Para todos los estados en donde esté registrada al menos una empresa
*/

SELECT claveestado , COUNT(idempresa)
FROM estado JOIN municipio USING (idestado)
     JOIN asentamiento USING (idmunicipio)
	 JOIN domicilio USING (idasentamiento)
	 JOIN empresa USING (iddomicilio)
GROUP BY 1

/*
Estados pobres
Columnas:
. [Todas las de la tabla estado]
. Para los estados donde no está registrada ninguna empresa
*/

SELECT e.* 
FROM estado e JOIN municipio USING (idestado)
JOIN asentamiento USING (idmunicipio)
JOIN domicilio USING (idasentamiento)
LEFT JOIN empresa USING (iddomicilio)
GROUP BY 1,2,3,4
HAVING COUNT(idempresa)<1

/*
Estado sin empresas
 . [Todas las de la tabla estado] 
Para los estados en los que no hay empresas registradas.
*/

--mismo que el anterior

/*
Crear una vista similar a "tuspinchesdatos"
pero que tenga solamente las columnas de:
 estado,municipio y asentamiento
*/

DROP VIEW tuspinchesconsultas

CREATE VIEW tuspinchesconsultas AS
SELECT e.idestado, e.claveestado, e.nombre AS estado, e.poblacion,
	   m.idmunicipio, m.nombre AS municipio,
	   a.idasentamiento, a.cp, a.nombre AS asentamiento
FROM estado e JOIN municipio m USING(idestado)
	 JOIN asentamiento a USING(idmunicipio)

SELECT *
FROM tuspinchesconsultas

/*
Distribución de ocupaciones por estado
Columnas:
. Nombre del estado
. Nombre de la ocupación
. Número de personas con esa ocupación en ese estado.

*/

SELECT t.estado, o.nombre AS ocupaciones, COUNT(p.idpersona)
FROM tuspinchesconsultas t JOIN domicilio d USING (idasentamiento)
	 JOIN persona p USING (iddomicilio)
	 RIGHT JOIN ocupacion o USING (idocupacion)
GROUP BY 1,2

-- La chida:

SELECT e.nombre, o.nombre, COUNT(idpersona)
FROM estado e LEFT JOIN municipio USING (idestado)
	 JOIN asentamiento USING (idmunicipio)
	 JOIN domicilio d USING (idasentamiento)
	 JOIN persona p USING (iddomicilio)
	 RIGHT JOIN ocupacion o USING (idocupacion)
GROUP BY 1,2

-- Del profe:

select e.nombre, o.nombre, count(idpersona)
from estado e
     join municipio m using (idestado)
     join asentamiento a using (idmunicipio)
     join ocupacion o on (true)
     join domicilio d using (idasentamiento)
     left join persona p using (iddomicilio,idocupacion)
group by 1,2

--Solución 2 del profe

select e.nombre, o.nombre, count(idpersona)
from estado e
     join municipio m using (idestado)
     join asentamiento a using (idmunicipio)
     join ocupacion o on (true)
     join domicilio d using (idasentamiento)
     left join persona p using (iddomicilio,idocupacion)
group by 1,2

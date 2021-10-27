/*
Colonias por municipio

Columnas:
1. nombre de estado
2. id del municipio
3. nombre del municipio
4. número de asentamientos en el municipio

Para todos los estados y municipios
*/

select e.nombre, m.idmunicipio, m.nombre, count(a.idasentamiento)
from estado e join municipio m using(idestado)
	 join asentamiento a using (idmunicipio)
group by 1,2,3

/*
Municipios por estado

Columnas:
1. Nombre del estado
2. Número de municipios en ese estado.

Para todos los esta	dos.
*/

SELECT est.nombre,count(idmunicipio)
FROM municipio AS mun
JOIN estado AS est USING(idestado)
GROUP BY est.idestado;

/*
Porcentaje de hombres y mujeres por estado

Columnas:
1. Nombre del estado
2. Porcentaje de las personas que viven en ese estado y son mujeres
3. Porcentaje de las personas que viven en ese estado y son hombres

*/

select e.nombre,
	(count(case when sexo='f' then idpersona else null end))/(count(idpersona))::float as mujeres,
	(count(case when sexo='m' then idpersona else null end))/(count(idpersona))::float as hombres
from estado e
	join municipio m using(idestado)
	join asentamiento a using(idmunicipio)
	join domicilio d using(idasentamiento)
	join persona p using(iddomicilio)
group by 1

--
																	
SELECT  est.idestado,
		est.nombre,
		ROUND(100*(count(case when sexo='f' then idpersona else null end))/(count(idpersona))::numeric,2)||'%' as mujeres,
		ROUND(100*(count(case when sexo='m' then idpersona else null end))/(count(idpersona))::numeric,2)||'%' as hombres 
FROM persona AS per
	JOIN domicilio AS dom USING(iddomicilio)
	JOIN asentamiento AS ase USING(idasentamiento)
	JOIN municipio AS mun USING(idmunicipio)
	JOIN estado AS est USING(idestado)
GROUP BY est.idestado;																	

/*
Porcentaje de hombres y mujeres por municipio

Columnas:
1. Nombre del estado
2. Id del municipio
3. Nombre del municipio
4. Porcentaje de las personas que viven en ese municipio y son mujeres
5. Porcentaje de las personas que viven en ese municipio y son hombres

Para todos los municipios de Baja California y Baja California Sur.
*/
		
select e.nombre, m.nombre,
	ROUND((count(case when sexo='f' then idpersona else null end)*100)/(count(idpersona))::NUMERIC ,2)||'%' as mujeres,
	ROUND((count(case when sexo='m' then idpersona else null end)*100)/(count(idpersona))::NUMERIC,2) ||'%' as hombres
from estado e
	join municipio m using(idestado)
	join asentamiento a using(idmunicipio)
	join domicilio d using(idasentamiento)
	join persona p using(iddomicilio)
WHERE e.idestado=2 or 	e.idestado=3															
group by 1, 2																			
																			
/*
Porcentaje de hombres y mujeres por asentamiento

Columnas:
1. Nombre del estado
2. Id del municipio
3. Nombre del municipio
4. Porcentaje de las personas que viven en ese municipio y son mujeres
5. Porcentaje de las personas que viven en ese municipio y son hombres

Para todos los asentamientos de Baja California Sur y Coahuila de Zaragoza
*/

select e.nombre, a.idasentamiento, a.nombre,
	round(100*(count(case when sexo='f' then idpersona else null end))/(count(idpersona))::numeric, 2)||'%' as mujeres,
	round(100*(count(case when sexo='m' then idpersona else null end))/(count(idpersona))::numeric, 2)||'%' as hombres
from estado e
	join municipio m using(idestado)
	join asentamiento a using(idmunicipio)
	join domicilio d using(idasentamiento)
	join persona p using(iddomicilio)
where e.nombre='Baja California Sur' or e.nombre like 'Coahuila%'
group by 1,2,3

/*
Porcentaje población por sexo/juventud por estado

Columnas:
1. Nombre del estado
2. Porcentaje de mujeres que viven en el estado (con respecto al total de personas)
3. Porcentaje de mujeres menores de 25 años que viven en el estado (con respecto al número total de mujeres)
4. Porcentaje de mujeres de 25 años en adelante que viven en el estado (con respecto al número total de mujeres)
5. Porcentaje de hombres que viven en el estado (con respecto al total de personas)
6. Porcentaje de hombres menores de 25 años que viven en el estado (con respecto al número total de hombres)
7. Porcentaje de hombres de 25 años en adelante que viven en el estado (con respecto al número total de hombres)

*/

SELECT
est.nombre,
ROUND(100*(count(case when sexo='f' then idpersona else null end))/(SELECT COUNT  FROM persona)::numeric,2)||'%' as mujeres,
ROUND(100*(count(case when sexo='f' and extract(YEAR FROM age(fnac))<25 then idpersona else null end))/(SELECT COUNT(case when sexo='f' then idpersona else null end) FROM persona)::numeric,2)||'%' as mujeres_menores_25,
ROUND(100*(count(case when sexo='f' and extract(YEAR FROM age(fnac))>=25 then idpersona else null end))/(SELECT COUNT(case when sexo='f' then idpersona else null end) FROM persona)::numeric,2)||'%' as mujeres_mayores_25,
ROUND(100*(count(case when sexo='m' then idpersona else null end))/(SELECT COUNT  FROM persona)::numeric,2)||'%' as mujeres,
ROUND(100*(count(case when sexo='m' and extract(YEAR FROM age(fnac))<25 then idpersona else null end))/(SELECT COUNT(case when sexo='m' then idpersona else null end) FROM persona)::numeric,2)||'%' as hombres_menores_25,
ROUND(100*(count(case when sexo='m' and extract(YEAR FROM age(fnac))>=25 then idpersona else null end))/(SELECT COUNT(case when sexo='m' then idpersona else null end) FROM persona)::numeric,2)||'%' as hombres_mayores_25
FROM persona AS per
JOIN domicilio AS dom USING(iddomicilio)
JOIN asentamiento AS ase USING(idasentamiento)
JOIN municipio AS mun USING(idmunicipio)
JOIN estado AS est USING(idestado)
GROUP BY 1;																	
																	
																	
/* Ejemplo
¿Qué personas no tienen apellido materno?
*/
select * from persona where ap2 is null;

/* Ejemplo 
Mostrar los datos de los domicilios que estén en la calle
"Norte..."
*/
select *
from domicilio
where callenum ilike 'Norte%';

/* Ejemplo 
Mostrar los datos de los domicilios que estén en la calle
norte pero sin número: patrón "Norte" seguido de algo que no es número seguido de # o No. seguido de número.
*/
select *
from domicilio
where callenum ilike 'Norte%';

/* Ejemplo
Mostrar los datos de todas las personas que se llamen "Mayte Mondragón"
*/
select *
from persona
where nombre = 'Mayte'
      and ap1 = 'Mondragón';

/* Ejemplo
Mostrar los datos de tqodas las personas que se llamen "Juan"
y hayan nacido en 1985
*/
select *
from persona
where nombre = 'Juan'
      and extract(year from fnac) = 1985;

/* Ejemplo
Mostrar los datos de todas las personas que nacieron
el 14 de oct de 1974
*/
select *
from persona
where fnac = '1974-10-14';

/* Ejemplo
Mostrar los datos de todos los municipios de Hidalgo
*/
select *
from municipio
where idestado = 13;

/* Ejemplo 
Mostrar el nombre del estado, el nombre del municipio y el nombre del asentamiento para
todos los asentamientos cuyo nombre empiece con "San"
*/
select e.nombre,m.nombre,a.nombre
from estado e,municipio m,asentamiento a
where (e.idestado = m.idestado
       and m.idmunicipio = a.idmunicipio)
      and a.nombre like 'San%';

/* Ejemplo
Mostrar el nombre completo
nombre del estado, municipio y asentamiento de las personas que viven en municipios que empiezan con "San"
*/
select p.ap1 || 
              (case when p.ap2 is null then '' else ' ' || p.ap2 end)
	  || ' ' || p.nombre as "Nombre",
	   e.nombre as nEstado,
		a.nombre as nAsentamiento,
		m.nombre as nMun
from estado e, 
      asentamiento a,
	  municipio m,
	  domicilio d,
	  persona p
where (e.idestado = m.idestado
	  	and m.idmunicipio = a.idmunicipio
	    and a.idasentamiento = d.idasentamiento
	    and p.iddomicilio = d.iddomicilio)
	   and a.nombre like 'San%';
           
/* Ejemplo
Mostrar el nombre completo nombre del estado, municipio y asentamiento de las personas que viven en municipios que empiezan con "San"
anteponiendo un título según las siguientes condiciones:
 Mujer hasta 45 años: Srita.
 Hombre hasta 18 años: Srito.
 Hombre mayor de 18: Sr.
 Mujer mayor de 45: Sra.
*/
select case when sexo = 'm' and extract(year from age(now(),fnac)) <= 18 then 'Srito.'
            when sexo = 'f' and extract(year from age(now(),fnac)) <= 45 then 'Srita.'
		    when sexo = 'm' and extract(year from age(now(),fnac)) > 18 then 'Sr.'
	        when sexo = 'f' and extract(year from age(now(),fnac)) > 45 then 'Sra.'
        end as "Título",
        p.ap1 || 
              (case when p.ap2 is null then '' else ' ' || p.ap2 end)
	  || ' ' || p.nombre as "Nombre",
	   e.nombre as nEstado,
		a.nombre as nAsentamiento,
		m.nombre as nMun
from estado e, 
      asentamiento a,
	  municipio m,
	  domicilio d,
	  persona p
where (e.idestado = m.idestado
	  	and m.idmunicipio = a.idmunicipio
	    and a.idasentamiento = d.idasentamiento
	    and p.iddomicilio = d.iddomicilio)
	   and a.nombre like 'San%'
           
/* Ejemplo 
Mostrar los nombres de los estados en donde hay una colonia
"San Bernabé"
*/
select distinct e.nombre
from estado e,municipio m,asentamiento a
where (e.idestado = m.idestado
       and m.idmunicipio = a.idmunicipio)
      and a.nombre like 'San Bernabé';


/* Ejemplo
Mostrar la calle y número de las  10 direcciones que se 
repiten en más asentamientos
*/
select callenum,count(*),count(distinct idasentamiento)
from domicilio
group by callenum
order by count(*) desc limit 10

/* Ejemplo
Mostrar las 10 direcciones que se repiten en más estados diferentes 
junto con el número de estados en los que se repiten.
*/
select callenum,count(distinct e.idestado)
from domicilio d, asentamiento a, municipio m, estado e
where (d.idasentamiento = a.idasentamiento
	  	and a.idmunicipio = m.idmunicipio
	    and m.idestado = e.idestado)
group by callenum
order by count(distinct e.idestado) desc
limit 10

/* Mostrar la clave del estado, el nombre del estado,
  el nombre del municipio, número de mujeres y hombre en el municipio*/
  select e.claveestado,
  		  e.nombre,
		  m.nombre,
		  count(case when sexo = 'f' then idpersona else null end) as mujeres,
		  count(case when sexo = 'm' then idpersona else null end) as hombres
from persona p,domicilio d,asentamiento a,municipio m,estado e
where (p.iddomicilio = d.iddomicilio
	  	and a.idasentamiento = d.idasentamiento
	   and a.idmunicipio = m.idmunicipio
	  and m.idestado = e.idestado)
group by 1,2,3
having count(case when sexo = 'f' then idpersona else null end) > count(case when sexo = 'm' then idpersona else null end)


/* Ejemplo
Mostrar los datos de los domicilios abandonados
*/

/*Toma 1*/
select *
from domicilio 
where iddomicilio not in (select iddomicilio from persona)

/*Toma 2*/
select *
from domicilio
except
select d.*
from persona p,
     domicilio d
where (p.iddomicilio = d.iddomicilio)


/* Ejemplo
Desplegar los datos de los 10 estados con más
domicilios abandonados.
*/
--Common Table Expresions (CTE)
with habitados as (select d.*
	               from persona p,domicilio d
                   where (p.iddomicilio = d.iddomicilio)),
     vacios as (select * from domicilio
	            except
 	            select * from habitados)
select e.*,count(*) as vacios
from vacios,asentamiento a, municipio m, estado e
where (vacios.idasentamiento = a.idasentamiento
	  and a.idmunicipio = m.idmunicipio
	  and m.idestado = e.idestado)
group by e.idestado,
		 e.claveestado,
		e.nombre
order by count(*) desc
limit 10
/* Ejemplo
Consulta anterior sin CTE's
*/ 
select e.*,count(v.iddomicilio) as vacios
from estado e,
     (select *
                from domicilio
                 except
                 select d.*
                 from persona p,
                      domicilio d
                  where (p.iddomicilio = d.iddomicilio)) as v,
     asentamiento a,
     municipio m
where (v.idasentamiento = a.idasentamiento
       and a.idmunicipio = m.idmunicipio
       and m.idestado = e.idestado)
group by e.idestado,
         e.claveestado,
         e.nombre,
         e.poblacion
order by count(*) desc
limit 10;

/* Ejemplo
Mostrar nombre de estado, domicilios habitados y domicilios vacíos. (Toma 1)
*/
with habitados as (select d.*,true as habit
                 from persona p,
                      domicilio d
                  where (p.iddomicilio = d.iddomicilio)),
    vacios as (select *
                from domicilio
                 except
                 select iddomicilio,callenum,idasentamiento from habitados),
      todos as (select * from habitados union select *,false as habit from vacios)
select e.nombre,e.claveestado,
       count(case when habit then t.iddomicilio end) as vacios,
       count(case when not habit then t.iddomicilio end) as habitados
from estado e,
     asentamiento a,
     municipio m,
     todos t
where (t.idasentamiento = a.idasentamiento
       and a.idmunicipio = m.idmunicipio
       and m.idestado = e.idestado)
group by e.idestado,
         e.claveestado,
         e.nombre,
         e.poblacion;
/* Ejemplo
Mostrar nombre de estado, domicilios habitados y domicilios vacíos. (Toma 2)
*/
with habitados as (select d.*
                 from persona p,
                      domicilio d
                  where (p.iddomicilio = d.iddomicilio)),
    vacios as (select *
                from domicilio
                 except
                 select * from habitados)
select e.nombre,e.claveestado,
       count(distinct v.iddomicilio) as vacios,
       count(distinct h.iddomicilio) as habitados
from estado e,
     asentamiento a,
     municipio m,
     vacios v,
     habitados h
where (a.idmunicipio = m.idmunicipio
       and m.idestado = e.idestado
       and (v.idasentamiento = a.idasentamiento
            or h.idasentamiento = a.idasentamiento))
group by e.idestado,
         e.claveestado,
         e.nombre
limit 10;

/* Ejemplo
Mostrar los 10 domicilios que se repiten en más estados diferentes
junto con las claves de dichos estados.
*/
select callenum,
       count(distinct e.idestado) as cuantos,
       string_agg(distinct e.claveestado,',' order by e.claveestado) as estados
from domicilio d, asentamiento a, municipio m, estado e
where (d.idasentamiento = a.idasentamiento
	  	and a.idmunicipio = m.idmunicipio
	    and m.idestado = e.idestado)
group by callenum
order by count(distinct e.idestado) desc
limit 10

/* Ejemplo
Mostrar nombre del estado junto con el nombre del municipio con más habitantes de ese estado.
*/

/* Toma 1: Window Functions */
with hm as (select e.nombre as estado,
	   	            m.nombre as muni,
	   		        count(idpersona) as habs
             from persona p,domicilio d, asentamiento a, municipio m, estado e
             where (p.iddomicilio = d.iddomicilio
	   				 and d.idasentamiento = a.idasentamiento
	   				and a.idmunicipio = m.idmunicipio
	   				and m.idestado = e.idestado)
			 group by 1,2),
        mexh as (select *,max(habs) over (partition by estado) as mhabs
				  from hm)
select * from mexh
where habs = mhabs
rollback;

/* Toma 2: Distinct ON */

with hm as (select e.nombre as estado,
                   m.nombre as municipio,
                   m.idmunicipio,
                   count(*) as habs
            from estado e,
                 municipio m,
                 asentamiento a,
                 domicilio d,
                 persona p
            where (e.idestado = m.idestado
                  and a.idmunicipio = m.idmunicipio
                  and d.idasentamiento = a.idasentamiento
                  and p.iddomicilio = d.iddomicilio)
            group by 1,2,3
            order by e.nombre,count(*) desc)
select distinct on (estado) hm.estado,
       municipio,
       habs
from hm

with hm as (select e.nombre as estado,
                   m.nombre as municipio,
                   m.idmunicipio,
                   count(*) as habs
            from estado e,
                 municipio m,
                 asentamiento a,
                 domicilio d,
                 persona p
            where (e.idestado = m.idestado
                  and a.idmunicipio = m.idmunicipio
                  and d.idasentamiento = a.idasentamiento
                  and p.iddomicilio = d.iddomicilio) group by 1,2,3)
select hm.estado,
       first(hm.municipio order by habs desc) as masgordo,
       max(habs)
from hm
group by 1

/* Ejemplo
Mostrar nombre de estado, proporción de domiclios habitados, proporción de domicilios vacíos y el cociente de habitados y vaciós (ordenar por este criterio). (comparando resultados)
*/
with
   dompe as (select e.idestado,count(*) as doms
              from estado e,
                   municipio m,
                   asentamiento a,
                   domicilio d
            where (e.idestado = m.idestado
                  and m.idmunicipio = a.idmunicipio
                  and d.idasentamiento = a.idasentamiento)
                  group by e.idestado),
    habitados as (select d.*,true as habit
                 from persona p,
                      domicilio d
                  where (p.iddomicilio = d.iddomicilio)),
    vacios as (select *
                from domicilio
                 except
                 select iddomicilio,callenum,idasentamiento from habitados),
      todos as (select * from habitados union select *,false from vacios)
select e.nombre,e.claveestado,
       --round((count(case when habit then t.iddomicilio end)::float/count(t.iddomicilio))::numeric,2)  as vacios,
       --round((count(case when not habit then t.iddomicilio end)::float/count(t.iddomicilio))::numeric,2) as habitados,
       --round((count(case when not habit then t.iddomicilio end)::float/count(case when habit then t.iddomicilio end))::numeric,2) as indice,
       count(*) as generica, dompe.doms as oficial, count(case when not habit then t.iddomicilio end) + count(case when  habit then t.iddomicilio end) as suma
--       dompe.doms =  count(case when not habit then t.iddomicilio end) + count(case when not habit then t.iddomicilio end)
from estado e,
     dompe,
     asentamiento a,
     municipio m,
     todos t
where (t.idasentamiento = a.idasentamiento
      and dompe.idestado = e.idestado
       and a.idmunicipio = m.idmunicipio
       and m.idestado = e.idestado)
group by e.idestado,
         e.claveestado,
         e.nombre,
         e.poblacion,
          dompe.doms;
--order by indice desc;

/* Ejemplo
Conteo de domicilios por estado
*/
select e.idestado,count(*)
from estado e,
     municipio m,
     asentamiento a,
     domicilio d
where (e.idestado = m.idestado
      and m.idmunicipio = a.idmunicipio
      and d.idasentamiento = a.idasentamiento)
group by e.idestado

/* Ejemplo
Mostrar nombre de estado, domicilios habitados y domicilios vacíos. (Toma Join)
*/
select e.nombre,
       count(case when p.idpersona is null then d.iddomicilio else null end) as vacios,
       count(distinct case when p.idpersona is not null then d.iddomicilio else null end) as habitados
from domicilio d
     join asentamiento a using (idasentamiento)
     join municipio m on (m.idmunicipio = a.idmunicipio)
     join estado e using (idestado)
     left join persona p using (iddomicilio)
group by e.nombre;

/* Ejemplo 
Mostrar el nombre del estado, el nombre del municipio y el nombre del asentamiento para
todos los asentamientos cuyo nombre empiece con "San"
*/
select e.nombre,m.nombre,a.nombre
from estado e
     join municipio m using (idestado)
     join asentamiento a using (idmunicipio)
where a.nombre like 'San%'


/*Ejemplo de vista materializada*/
create materialized view tuspinchesdatos as 
select p.*,d.callenum,a.idasentamiento,a.nombre as asentamiento,a.cp,m.nombre as municipio,e.idestado,e.nombre as estado,e.claveestado,e.poblacion,idmunicipio
from  persona p
	join domicilio d using (iddomicilio)
	join asentamiento a using (idasentamiento)
	join municipio m using (idmunicipio)
	join estado e using (idestado);


--*******************SECUNDARIA****************
/*
Nombres populares
Columnas:
 .Nombre
 .Número de repeticiones
Para el nombre que se repite más veces en la tabla persona.
*/
select nombre,count(*) from persona group by nombre order by 2 limit 1;


/*
¿Misma persona?
Columnas:
 .[Datos completos de persona] (excepto iddomicilio,iducupacion,idcliente,idpersona)
 .Edad
 .Número de veces que se repite (Nombre,apellidos,sexo y fecha de nacimiento)
Para las personas que aparecen en la BD más de una vez
*/
select nombre,ap1,ap2,sexo,fnac,age(now(),fnac) as edad,count(*) from persona group by 1,2,3,4,5,6 having count(*) > 1;


/* Mostrar
 [[Colunmas]]
  . Nombre de la tabla
  . Número de registros
  . id más pequeño
  . id más grande
  Para todas las tablas de la BD
 */
--Toma 1
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

--Toma 2

select 'persona' as nombre_tabla , count(*), min(idpersona), max(idpersona) from persona
union
select 'articulo'  as nombre_tabla, count(*), min(idarticulo), max(idarticulo) from articulo
union
select 'asentamiento' as nombre_tabla, count(*), min(idasentamiento), max(idasentamiento) from asentamiento
union
select 'cliente' as nombre_tabla, count(*), min(idcliente), max(idcliente) from cliente
union
select 'domicilio ' as nombre_tabla , count(*), min(iddomicilio), max(iddomicilio) from domicilio
union
select 'empresa ' as nombre_tabla , count(*), min(idempresa), max(idempresa) from empresa
union
select 'estado ' as nombre_tabla, count(*), min(idestado), max(idestado) from estado
union
select 'fabrica ' as nombre_tabla, count(*), min(idfabrica), max(idfabrica) from fabrica
union
select 'pedido ' as nombre_tabla, count(*), min(idpedido), max(idpedido) from pedido
union
select 'municipio ' as nombre_tabla, count(*), min(idmunicipio), max(idmunicipio) from municipio
union
select 'ocupacion ' as nombre_tabla, count(*), min(idocupacion), max(idocupacion) from ocupacion;


/* Ejemplo
Jóvenes.
Columnas:
 . Nombre del estado
 . Nombre de municipio
 . número de habitantes del municipio
 . [Datos completos de persona]
Para las personas más jóvenes de cada municipio
*/
--Toma 1
with numHabMun as (select e.idestado, m.idmunicipio, count(p.idpersona) as numHab
                   from persona p
                        left join domicilio d on p.iddomicilio=d.iddomicilio
                        left join asentamiento a on d.idasentamiento=a.idasentamiento
                        left join municipio m on a.idmunicipio=m.idmunicipio
                        left join estado e on m.idestado=e.idestado
                   group by e.idestado, m.idmunicipio),
      jovenMun as (select e.idestado,
                          m.idmunicipio,
                          max(p.fnac) as fnacjm
                   from persona p
                        left join domicilio d on p.iddomicilio=d.iddomicilio
                        left join asentamiento a on d.idasentamiento=a.idasentamiento
                        left join municipio m on a.idmunicipio=m.idmunicipio
                        left join estado e on m.idestado=e.idestado
                   group by e.idestado,m.idmunicipio),
      perMun as (select e.idestado,m.idmunicipio,p.*
                 from persona p
                      left join domicilio d on p.iddomicilio=d.iddomicilio
                      left join asentamiento a on d.idasentamiento=a.idasentamiento
                      left join municipio m on a.idmunicipio=m.idmunicipio
                      left join estado e on m.idestado=e.idestado)
select e.nombre estado,m.nombre municipio,nh.numHab,pm.nombre,pm.ap1,pm.ap2,pm.sexo,pm.fnac
from jovenMun jm
     left join numHabMun nh on jm.idestado=nh.idestado and jm.idmunicipio=nh.idmunicipio
     left join estado e on e.idestado=jm.idestado
     left join municipio m on m.idmunicipio=jm.idmunicipio
     left join perMun pm on pm.fnac=jm.fnacjm and pm.idestado=jm.idestado and pm.idmunicipio=jm.idmunicipio;

--Toma 2
select e.idestado,m.idmunicipio,perso,p.*
from persona p
     left join domicilio d on p.iddomicilio=d.iddomicilio
     left join asentamiento a on a.idasentamiento=d.idasentamiento
     left join municipio  m on m.idmunicipio=a.idmunicipio
     left join estado  e on e.idestado=m.idestado
     left join (select m.idmunicipio,max(fnac) nac_max
                from persona p
                     left join domicilio d on p.iddomicilio=d.iddomicilio
                     left join asentamiento a on a.idasentamiento=d.idasentamiento
                     left join municipio  m on m.idmunicipio=a.idmunicipio
                group by m.idmunicipio) as x on m.idmunicipio=x.idmunicipio
     left join (select m.idmunicipio,count(p.idpersona) perso
                from persona p
                     left join domicilio d on p.iddomicilio=d.iddomicilio
                     left join asentamiento a on a.idasentamiento=d.idasentamiento
                     left join municipio  m on m.idmunicipio=a.idmunicipio
                 group by m.idmunicipio) as y on m.idmunicipio=y.idmunicipio
where fnac=nac_max
order by m.idmunicipio

-- Toma 3
with agrupado as (select estado, municipio, count(*) nhabitantes, max(fnac) as fecha
                  from tuspinchesdatos
                  group by 1,2)
select b.*, a.idpersona, a.nombre, a.ap1, a.ap2, a.sexo, a.iddomicilio, a.idocupacion, a.idcliente
from tuspinchesdatos a
     inner join agrupado b on (a.estado=b.estado and a.municipio=b.municipio and a.fnac=b.fecha);

--Toma 4
with kk as (select e.estado as edo,
                   e.municipio as mun,
                   e.idmunicipio,
                   count(idpersona) as habs,
                   max(fnac) as fnac
            from tuspinchesdatos e
            group by 1,2,3)
select idpersona,edo,mun,habs,p.*
from tuspinchesdatos p
     join kk using (idmunicipio,fnac)

/*
Distribución de edades
Columnas:
 . Clave del estado
 . Nombre del estado
 . Edad promedio en el estado
 . Número de personas de edad mayor al promedio
 . Número de personas de edad menor al promedio
*/

--Toma 1
select e.nombre estado,
       e.claveestado,
       promedio,
       sum (case when extract(year from age(now(),fnac))>=promedio then 1 else 0 end) personas_may_prom,
       sum (case when extract(year from age(now(),fnac))<promedio then 1 else 0 end) personas_men_prom
from persona p
     left join domicilio d on p.iddomicilio=d.iddomicilio
     left join asentamiento a on a.idasentamiento=d.idasentamiento
     left join municipio  m on m.idmunicipio=a.idmunicipio
     left join estado  e on e.idestado=m.idestado
     left join (select e.idestado,avg(extract(year from age(now(),fnac))) promedio
                from persona p
                     left join domicilio d on p.iddomicilio=d.iddomicilio
                     left join asentamiento a on a.idasentamiento=d.idasentamiento
                     left join municipio  m on m.idmunicipio=a.idmunicipio
                     left join estado  e on e.idestado=m.idestado
                 group by e.idestado)x on e.idestado=x.idestado
group by estado,e.claveestado,x.promedio

--Toma 3
with eprom as (select claveestado,
                      estado,
                      avg(age(now(),fnac)) as epr
               from tuspinchesdatos t
               group by 1,2)
select d.claveestado,
       d.estado,
       epr,
       count(case when age(now(),fnac) > epr then idpersona else null end) as mayores,
       count (case when age(now(),fnac) < epr then idpersona else null end) as menores
from tuspinchesdatos d
     join eprom using (claveestado)
group by 1,2,3
     


--***************************Funciones************************

/* Función para calcular la edad en años a partir de la fecha de nacimiento */
rollback
create function años(fnac date) returns int as $$
select extract(year from age(now(),fnac))::int;
$$ language sql;

/* Función para calcular el nombre competo de una persona */
create function nc(ap1 text, ap2 text, nombre text) returns text as $$
select ap1 || coalesce(' '||ap2,'') || nombre;
$$language sql;
/* Función para obtener a las personas más jóvenes de un estado*/
create function masjo(cve char(2)) returns setof persona as $$
with kk as (select e.estado as edo,
                   e.municipio as mun,
                   e.idmunicipio,
                   count(idpersona) as habs,
                   max(fnac) as fnac
            from tuspinchesdatos e
            group by 1,2,3)
select idpersona,nombre,ap1,ap2,sexo,fnac,iddomicilio,idocupacion,idcliente
from tuspinchesdatos p
     join kk using (idmunicipio,fnac)
	 where claveestado = cve;
$$ language sql

select masjo('DF')

select p.*,nc(ap1,ap2,nombre) from persona p limit 10

--Odín
create view conteos as 
 select null::text as "Tabla",
        null::bigint as "Registros",
        null::numeric as "IdMin",
        null::numeric as "IdMax";
create or replace function conteos() returns setof conteos as $$
  declare
    v_tn text;
    v_aux text;
    v_rec conteos;
  begin
    for v_tn in select tablename from pg_tables where schemaname = 'public' and tablename <> 'artspedido' loop
       /*select format($q$select '%s',count(*),min(id%s),max(id%s) from %s$q$,v_tn,v_tn,v_tn,v_tn) into v_aux;
          raise info '%',v_aux;*/
       execute format($q$select '%s',count(*),min(id%s),max(id%s) from %s$q$,v_tn,v_tn,v_tn,v_tn) into v_rec;
       return next v_rec;
    end loop;
  end
$$language plpgsql;


--Odín
create view conteos as 
 select null::text as "Tabla",
        null::bigint as "Registros",
        null::numeric as "IdMin",
        null::numeric as "IdMax";
create or replace function conteos() returns setof conteos as $$
  declare
    v_tn text;
    v_aux text;
    v_rec conteos;
  begin
    for v_tn in select tablename from pg_tables where schemaname = 'public' and tablename <> 'artspedido' loop
       /*select format($q$select '%s',count(*),min(id%s),max(id%s) from %s$q$,v_tn,v_tn,v_tn,v_tn) into v_aux;
          raise info '%',v_aux;*/
       execute format($q$select '%s',count(*),min(id%s),max(id%s) from %s$q$,v_tn,v_tn,v_tn,v_tn) into v_rec;
       return next v_rec;
    end loop;
  end
$$language plpgsql;

/* Función que tome la clave del estado
 y regrese la edad y el nombre completo de todos
 sus habitantes*/
 
	 create view edadNC as
	  select null::text as "Nombre Completo",
			  null::int as "Años";
	 create or replace function edadNC(cve char(2)) returns setof edadNC as $$
	 select nc(ap1,ap2,nombre),años(fnac) 
	 from tuspinchesdatos 
	 where claveestado = cve;
	 $$language sql;
	 select * from edadnc('AS')
	 
	 /*Función que regrese el número de registros
	 de una tabla que se le pasa como parámetro*/
	 create function cuenta(tb text) returns bigint as$$
	 select count(*) from tb;
	 $$language plpgsql;

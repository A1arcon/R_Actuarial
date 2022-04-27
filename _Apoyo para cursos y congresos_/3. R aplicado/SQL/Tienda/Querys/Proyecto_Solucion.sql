
/*
Alumnos:
- Aceves Vázquez Laura Lorena
- Alarcón González Edgar Gerardo
*/

/*
Ejercicio 1
*/

with
asentamientos_vacios as(
select idasentamiento 
from asentamiento left join domicilio using (idasentamiento)
where iddomicilio is null)
--
delete
from asentamiento
where idasentamiento in (select idasentamiento from asentamientos_vacios)

/*
Ejercicio 2
*/

with
casa_sola as(	
	select d.iddomicilio, count(p.idpersona) 
	from domicilio d join persona p using (iddomicilio)
	group by 1
	having count(p.idpersona) = 1)
--
select persona.*
from persona join casa_sola using (iddomicilio)

/*
Ejercicio 3
*/

with
top_estados as (
	-- 5 estados más habitados
	select e.idestado, e.nombre, count(idpersona)
	from estado e join municipio m using(idestado)
		 join asentamiento a using (idmunicipio)
		 join domicilio d using (idasentamiento)
		 join persona p using (iddomicilio)	 
	group by 1,2
	order by count(idpersona) desc
	limit 5 ),
domicilios_vacios_top as (
	--domicilios vacíos en el top
	select d.iddomicilio, count(idpersona)
	from top_estados t join municipio m using(idestado)
		 join asentamiento a using (idmunicipio)
		 join domicilio d using (idasentamiento)
		 left join persona p using (iddomicilio)	 
	group by 1
	having count(idpersona) = 0)
--
select d.iddomicilio, d.callenum, a.nombre asentamiento, m.nombre municipio, e.nombre estado 
from estado e join municipio m using(idestado)
		 join asentamiento a using (idmunicipio)
		 join domicilio d using (idasentamiento)
		 join domicilios_vacios_top dt using (iddomicilio)

/*
Ejercicio 4
*/

/* persona
select idcliente, count(idcliente)
from persona
group by 1
order by count(idcliente) desc*/

ALTER TABLE persona ADD CONSTRAINT persona_idcliente_unico UNIQUE (idcliente);

/* empresa
select idcliente, count(idcliente)
from empresa
group by 1
having count(idcliente) > 1
order by count(idcliente) desc*/

ALTER TABLE empresa ADD CONSTRAINT empresa_idcliente_unico UNIQUE (idcliente);

-- empresa en persona
create or replace function comprueba_empresa(identifica bigint) returns boolean as $$
	with
	comprueba_persona as(
	select idcliente 
	from persona
	where idcliente = identifica
	limit 1)
	--
	select 	(case
		   	when identifica is null then true
			--
			when (select * from comprueba_persona)=identifica then false
		   	--
			when 0<1 then true
		   end);
$$ language sql;

-- persona en empresa
create or replace function comprueba_persona(identifica bigint) returns boolean as $$
	with
	comprueba_empresa as(
	select idcliente 
	from empresa
	where idcliente = identifica
	limit 1)
	--
	select 	(case
		   	when identifica is null then true
			--
			when (select * from comprueba_empresa)=identifica then false
		   	--
			when 0<1 then true
		   end);
$$ language sql;

/* ver la función
select * from persona order by idcliente asc
select * from empresa order by idcliente asc
select comprueba(10002)*/

-- agregar restricción
alter table persona add check (comprueba_persona(idcliente));
alter table empresa add check (comprueba_empresa(idcliente));
										
												 
/*
Ejercicio 5
*/

with
tocayos_compartiendo as(
	select nombre, iddomicilio, count(idpersona)
	from persona
	group by 1,2
	having count(idpersona) > 1
)
--
select p.*
from persona p join tocayos_compartiendo t using (iddomicilio)
where p.nombre like t.nombre
order by p.iddomicilio, p.ap1, p.ap2, p.nombre 

/*
Ejercicio 6
*/

-- Para generar fechas válidas
create or replace function ranfecha() returns date as $$ 
declare 
	r_y int; 
	r_m int; 
	r_d int; 
begin 
	r_y := curandom(2010,2019); 
	r_m := curandom(1,13); 
	r_d := curandom(1,29); 
while not valida(r_y,r_m,r_d) loop
	raise info 'Hubo fecha inv´alida'; 
	r_y := curandom(2010,2019); 
	r_m := curandom(1,13); 
	r_d := curandom(1,29); 
end loop; 
	return (r_y::text ||'-' || r_m::text || '-' || r_d::text)::date; 
end; 
$$ language plpgsql;
						   
-- Obtener idempresa de empresas aleatorios (que seguro están en la tabla)
create or replace function randempresa() returns bigint as $$
select bawr.idempresa						   
from	(select *
		from						 
			(select * from empresa limit 1) as brl
			union
			(select *  from empresa where random() < 0.25)						   
		limit 1) as bawr
$$ language sql;
						   
-- Para obtener idcliente de clientes aleatorios (que seguro están en la tabla)
create or replace function randcliente() returns bigint as $$
select bawr.idcliente
from	(select *
		from						 
			(select * from cliente limit 1) as brl
			union
			(select *  from cliente where random() < 0.021)						   
		limit 1) as bawr
$$ language sql;

--
with
auxiliar1 as(
	select generate_series as idpedido,ranfecha() as fecha, randempresa() as idempresa,randcliente() as idcliente 
	from generate_series(4000,4000+3000-1)
),
auxiliar2 as(
	select a.idpedido, a.fecha, e.nombrecom, a.idcliente
	from auxiliar1 a join empresa e using (idempresa)
)						   
--
INSERT INTO pedido
SELECT * FROM auxiliar2

/*
Ejercicio 7
*/

-- Obtener idpedido de pedido aleatorios (que seguro están en la tabla)
create or replace function randpedido() returns bigint as $$
select bawr.idpedido
from	(select *
		from						 
			(select * from pedido limit 1) as brl
			union
			(select *  from pedido where random() < 0.25)						   
		limit 1) as bawr
$$ language sql;
-- Obtener idarticulo de articulo aleatorios (que seguro están en la tabla)
create or replace function randarticulo() returns bigint as $$
select bawr.idarticulo
from	(select *
		from						 
			(select * from articulo limit 1) as brl
			union
			(select *  from articulo where random() < 0.25)						   
		limit 1) as bawr
$$ language sql;
/* Función que regresa un valor aleatorio entre a y b*/
create or replace function aleatorio(a bigint, b bigint) returns bigint as $$
	select floor(random()*(b+1 - a))::bigint+a;
$$ language sql;
--
with 
auxiliar as(
	select  randpedido() idpedido, 
			randarticulo() idarticulo, 
			aleatorio(1,100)::int cantidad, 
			aleatorio(100000,1000000)/100::double precision costounitario
	from generate_series(1,12000))
--
INSERT INTO artspedido
SELECT * FROM auxiliar

/*
Ejercicio 8
*/

create or replace view brl as (
	with 
	personas_domicilio as(
	select idestado, avg(personas) as prom_domicilio
	from(	select e.idestado, m.idmunicipio, a.idasentamiento, d.iddomicilio, count(p.idpersona) as personas
			from estado e join municipio m using(idestado)
				 join asentamiento a using (idmunicipio)
				 join domicilio d using (idasentamiento)
				 left join persona p using (iddomicilio)	 
			group by 1,2,3,4 ) as bawr
	group by 1
	),
	personas_asentamiento as(
	select idestado, avg(personas) as prom_asentamiento
	from(	select e.idestado, m.idmunicipio, a.idasentamiento, count(p.idpersona) as personas
			from estado e join municipio m using(idestado)
				 join asentamiento a using (idmunicipio)
				 join domicilio d using (idasentamiento)
				 left join persona p using (iddomicilio)	 
			group by 1,2,3 ) as bawr
	group by 1
	),
	personas_municipio as(
	select idestado, avg(personas) as prom_municipio
	from(	select e.idestado, m.idmunicipio, count(p.idpersona) as personas
			from estado e join municipio m using(idestado)
				 join asentamiento a using (idmunicipio)
				 join domicilio d using (idasentamiento)
				 left join persona p using (iddomicilio)	 
			group by 1,2 ) as bawr
	group by 1
	),
	personas_estado as(
	select e.idestado, count(p.idpersona) as personas
	from estado e join municipio m using(idestado)
		 join asentamiento a using (idmunicipio)
		 join domicilio d using (idasentamiento)
		 left join persona p using (iddomicilio)	 
	group by 1
	)
	--
	select e.claveestado, pe.personas, pm.prom_municipio, pa.prom_asentamiento, pd.prom_domicilio
	from estado e join personas_estado pe using (idestado)
		 join personas_municipio pm using (idestado)
		 join personas_asentamiento pa using (idestado)
		 join personas_domicilio pd using (idestado)
)

--select * from brl

/*
Ejercicio 9
*/
						 
CREATE FUNCTION signo(fnac DATE) 
RETURNS TEXT AS $$
select
	(case  
	WHEN (extract(month from fnac)  = 3 AND extract(day from fnac)  >= 21) OR (extract(month from fnac)  = 4 AND extract(day from fnac)  <= 20) THEN 'Aries'
	WHEN (extract(month from fnac)  = 4 AND extract(day from fnac)  >= 21) OR (extract(month from fnac)  = 5 AND extract(day from fnac)  <= 20) THEN 'Tauro'
	WHEN (extract(month from fnac)  = 5 AND extract(day from fnac)  >= 21) OR (extract(month from fnac)  = 6 AND extract(day from fnac)  <= 21) THEN 'Géminis'
	WHEN (extract(month from fnac)  = 6 AND extract(day from fnac)  >= 22) OR (extract(month from fnac)  = 7 AND extract(day from fnac)  <= 22) THEN 'Cáncer'
	WHEN (extract(month from fnac)  = 7 AND extract(day from fnac)  >= 23) OR (extract(month from fnac)  = 8 AND extract(day from fnac)  <= 23) THEN 'Leo'
	WHEN (extract(month from fnac)  = 8 AND extract(day from fnac)  >= 24) OR (extract(month from fnac)  = 9 AND extract(day from fnac)  <= 23) THEN 'Virgo'
	WHEN (extract(month from fnac)  = 9 AND extract(day from fnac)  >= 24) OR (extract(month from fnac)  = 10 AND extract(day from fnac)  <= 22) THEN 'Libra'
	WHEN (extract(month from fnac)  = 10 AND extract(day from fnac)  >= 23) OR (extract(month from fnac)  = 11 AND extract(day from fnac)  <= 22) THEN 'Escorpio'
	WHEN (extract(month from fnac)  = 11 AND extract(day from fnac)  >= 23) OR (extract(month from fnac)  = 12 AND extract(day from fnac)  <= 21) THEN 'Sagitario'
	WHEN (extract(month from fnac)  = 12 AND extract(day from fnac)  >= 22) OR (extract(month from fnac)  = 1 AND extract(day from fnac)  <= 19) THEN 'Capricornio'
	WHEN (extract(month from fnac)  = 1 AND extract(day from fnac)  >= 20) OR (extract(month from fnac)  = 2 AND extract(day from fnac)  <= 19) THEN 'Acuario'
	WHEN (extract(month from fnac)  = 2 AND extract(day from fnac)  >= 20) OR (extract(month from fnac)  = 3 AND extract(day from fnac)  <= 20) THEN 'Piscis'
	end) as Zodiaco ; $$
LANGUAGE SQL;

--select signo('1995-11-06'::DATE)
--select signo('1996-01-29'::DATE)


/*
Ejercicio 10
*/
						 
create or replace function ejercicio10(v_edo char(2)) 
returns table ("Clave estado" char(2),"Total de clientes"  bigint, "Total de pedidos" bigint,
			   "Artículo más pedido" bigint,"Pedido más costoso" double precision) as $$
--
declare												  
--
begin
	return query 
		   with 
			clientes_completos as(
				select id, iddomicilio, idcliente
				from (select idpersona as id, iddomicilio, idcliente from persona where idcliente is not null) as brl 
					 union 
					 (select idempresa as id, iddomicilio,idcliente from empresa where idcliente is not null)),
			total_clientes as(
				select count(idcliente) as total_clientes
				from estado e join municipio m using(idestado)
					 join asentamiento a using (idmunicipio)
					 join domicilio d using (idasentamiento)
					 join clientes_completos using (iddomicilio)
				where claveestado ilike v_edo),
			total_pedidos as(
				select count(idpedido) as total_pedidos
				from estado e join municipio m using(idestado)
					 join asentamiento a using (idmunicipio)
					 join domicilio d using (idasentamiento)
					 join clientes_completos using (iddomicilio)
					 join pedido using (idcliente)
				where claveestado ilike v_edo),
			articulo_pedido as(
				select idarticulo, sum(cantidad) as total_cantidad
				from estado e join municipio m using(idestado)
					 join asentamiento a using (idmunicipio)
					 join domicilio d using (idasentamiento)
					 join clientes_completos using (iddomicilio)
					 join pedido using (idcliente)
					 join artspedido using (idpedido)
				where claveestado ilike v_edo
				group by 1
				order by sum(cantidad) desc
				limit 1),
			pedido_caro as(
				select arts.idpedido, sum(cantidad*costounitario) as monto
				from estado e join municipio m using(idestado)
					 join asentamiento a using (idmunicipio)
					 join domicilio d using (idasentamiento)
					 join clientes_completos using (iddomicilio)
					 join pedido using (idcliente)
					 join artspedido arts using (idpedido)
				where claveestado ilike v_edo	  
				group by arts.idpedido
				order by sum(cantidad*costounitario) desc
				limit 1)
			select v_edo as "Clave Estado",
				   (select total_clientes from total_clientes) as "Total de clientes",		  
				   (select total_pedidos from total_pedidos) as "Total de pedidos",		  
				   (select total_cantidad from articulo_pedido) as "Artículo más pedido",		  
				   (select monto from pedido_caro) as "Pedido más costoso";
end
$$ language plpgsql;												  

--select * from ejercicio10('DF')
					  		  
/*
Ejercicio 11
*/

create materialized view zodiaco as  (
select p.idpersona , d.iddomicilio, a.idasentamiento, m.idmunicipio, e.idestado, signo(p.fnac)
from estado e join municipio m using(idestado)
	 join asentamiento a using (idmunicipio)
	 join domicilio d using (idasentamiento)
	 join persona p using (iddomicilio)	 
)

 -- select * from zodiaco
 
/*
Ejercicio 12
*/

--DROP FUNCTION refresh_zodiaco() 

CREATE OR REPLACE FUNCTION refresh_zodiaco()
  RETURNS TRIGGER LANGUAGE plpgsql
  AS $$
  BEGIN
  REFRESH MATERIALIZED VIEW zodiaco;
  RETURN NULL;
  END $$;
 
--DROP TRIGGER refresh_zodiaco_t ON persona

CREATE TRIGGER refresh_zodiaco_t
  AFTER INSERT OR UPDATE OR DELETE
  ON persona
  FOR EACH STATEMENT
  EXECUTE PROCEDURE refresh_zodiaco(); 

/*
--revisión

select * from persona
order by  idpersona desc

select * from zodiaco
order by  idpersona desc

insert into persona 
values (1899507, 'Edgar', 'Alarcón', 'González', 'm', '1995-11-06'::DATE, 479737,1)

update persona set fnac = '1996-01-29'::DATE where idpersona = 1899507

delete from persona where  idpersona = 1899507
*/

/*

Ejercicio 13

*/

create or replace function valida(y int, m int, d int) returns boolean as $$ 
select (
	case
	-- Enero, Marzo, Mayo, Julio, Agosto, Octubre y Diciembre
	when m in (1,3,5,7,8,10,12) and 0<d and d<=31 and 1900 <= y and y <= 2018 then true
	-- Abril, Junio, Septiembre, Noviembre
	when m in (4,6,9,11) and 0<d and d<=30 and 1900 <= y and y <= 2018 then true
	-- Febrero
	when m = 2 and 0<d and d<=28 and 1900 <= y and y <= 2018 then true
	-- Febrero (año bisiesto)
	when m = 2 and 0<d and d<=29 and 1900 <= y and y <= 2018 and ((y%4=0 and y%100<>0)or y%400=0) then true
	--En otro caso...
	when 2<3 then false
	end
	);
$$ language sql; 

--select valida(1995,3,31)

create or replace function randate() returns date as $$ 
declare 
	r_y int; 
	r_m int; 
	r_d int; 
begin 
	r_y := curandom(1910,100); 
	r_m := curandom(1,11); 
	r_d := curandom(1,30); 
while not valida(r_y,r_m,r_d) loop
	raise info 'Hubo fecha inv´alida'; 
	r_y := curandom(1910,100); 
	r_m := curandom(1,11); 
	r_d := curandom(1,30); 
end loop; 
	return (r_y::text ||'-' || r_m::text || '-' || r_d::text)::date; 
end; 
$$ language plpgsql;

--select randate()

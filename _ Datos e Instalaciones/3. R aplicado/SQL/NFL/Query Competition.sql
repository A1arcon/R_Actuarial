--PRIMERA RONDA--------------------------------------------------------------------------------
--Nombres de los entrenadores cuyos apellidos son: 'Smith', 'Crennel', 'McCarthy' y cuyo año de funadcion haya sido después del 1919
SELECT nombre_entrenador
FROM entrenador NATURAL JOIN equipo_entrenador NATURAL JOIN equipo
WHERE apellido_entrenador IN ('Smith', 'Crennel', 'McCarthy') AND anyo_fund > 1919;
--Los touchdown, recepciones y yardas
SELECT yds, tds, rec
FROM receiver NATURAL JOIN jugador
WHERE fecha_nacimiento > '1985-11-04' AND id_jugador > 913
--
SELECT COUNT(id_equipo)
FROM partido  JOIN equipo ON partido.id_equipo_l = equipo.id_equipo 
WHERE marcador_L > marcador_V AND conferencia LIKE 'Americana'
--
EXPLAIN(SELECT nombre_equipo, nickname_equipo
FROM equipo NATURAL JOIN equipo_estadio NATURAL JOIN estadio
WHERE anyo_construccion >= 1950 AND anyo_construccion <= 1990 AND capacidad >=30000)
--
SELECT COUNT(id_partido)
FROM partido
GROUP BY temporada
ORDER BY temporada DESC;
--
SELECT nombre_entrenador, apellido_entrenador, cargo
FROM entrenador NATURAL JOIN equipo_entrenador NATURAL JOIN equipo
WHERE (nombre_entrenador LIKE 'C%' OR nombre_entrenador LIKE 'W%' OR nombre_entrenador LIKE 'R%') AND anyo_fund<1980
--SEGUNDA RONDA-----------------------------------------------------------------------------------
--El número de pateadores por cada universidad cuyo porcentaje de efectividad (fgm/fga)*100 > 70
SELECT universidad, COUNT(id_jugador)
FROM kicker NATURAL JOIN jugador
WHERE (fgm::NUMERIC)/(fga::NUMERIC)*100 > 70
GROUP BY universidad
--El promedio de habitantes de las ciudades con a lo más un equipo.
SELECT ciudad.nombre_ciudad, AVG(ciudad.habitantes), count(equipo_ciudad.id_equipo)
FROM equipo_ciudad RIGHT JOIN ciudad ON ciudad.id_ciudad = equipo_ciudad.id_ciudad
GROUP BY ciudad.id_ciudad
HAVING COUNT(equipo_ciudad.id_equipo)<=1
ORDER BY ciudad.nombre_ciudad
--Nombre de las universidades cuyos kicker tienen más que el promedio, ordenadas.
SELECT universidad
FROM jugador NATURAL JOIN  kicker
WHERE fga > (SELECT AVG(fga) FROM kicker) AND fecha_nacimiento < '1970-01-01' 
ORDER BY universidad ASC;
--Nombre, apellido, universidad y fecha de nacimiento el runner que menos carries tiene
SELECT nombre_jugador, apellido_jugador, universidad, fecha_nacimiento
FROM jugador NATURAL JOIN runner
WHERE carries = (SELECT MIN(carries) FROM runner)
--El numero de yardas que suman todos los runner que nacieron después de 1970
SELECT SUM(yds)
FROM runner NATURAL JOIN jugador
WHERE fecha_nacimiento >= '1971-01-01'
--TERCERA RONDA---------------------------------------------------------------------------------
/*
Identificador y nombre de los equipos que tienen más de 5 runners registrados 
ordenados de manera descendente por el nombre del equipo.
*/
SELECT id_equipo, nombre_equipo
FROM equipo NATURAL JOIN jugador_equipo NATURAL JOIN jugador NATURAL JOIN runner 
GROUP BY id_equipo
HAVING COUNT(runner.id_jugador)>5
ORDER BY nombre_equipo DESC 
/*
Total de partidos agrupados por temporada en los que ganó 
el equipo visitante y se realizaron después 
de la semana 5 de cada temporada, ordenados de manera ascendente por temporada
*/
SELECT COUNT(id_partido), temporada
FROM partido 
WHERE marcador_V > marcador_L AND semana > 5
GROUP BY temporada
ORDER BY temporada
/*
El número jugadores Kicker por cada universidad 
cuyo porcentaje de efectividad (fgm/fga)*100 es mayor a 75
*/
SELECT universidad, COUNT(id_jugador)
FROM kicker NATURAL JOIN jugador
WHERE (fgm/fga)*100 > 75
GROUP BY universidad
/*
Nombre y apellido de los Runners que han jugado en dos 
equipos diferentes y cuyo año de adquisición sea a partir de 1990.
*/
EXPLAIN(SELECT nombre_jugador, apellido_jugador
FROM jugador
WHERE id_jugador IN(
	SELECT id_jugador
	FROM jugador_equipo NATURAL JOIN jugador NATURAL JOIN runner
	WHERE anyo_adquirido >= 1990
	GROUP BY id_jugador
	HAVING COUNT(id_jugador) = 2))
--Más eficiente:
EXPLAIN(SELECT nombre_jugador, apellido_jugador
	FROM jugador_equipo NATURAL JOIN jugador NATURAL JOIN runner
	WHERE anyo_adquirido >= 1990
	GROUP BY id_jugador, nombre_jugador, apellido_jugador
	HAVING COUNT(id_jugador) = 2)
--SEMIFINAL---------------------------------------------------------------------------------
--Nombre del entrenador cuyo equipo es el más ganador de local.
SELECT nombre_entrenador,apellido_entrenador 
FROM entrenador JOIN equipo_entrenador ON entrenador.id_entrenador = equipo_entrenador.id_entrenador
		JOIN equipo ON equipo_entrenador.id_equipo = equipo.id_equipo
WHERE equipo.id_equipo IN (SELECT id_equipo
			FROM equipo JOIN partido ON equipo.id_equipo=partido.id_equipo_L
			WHERE marcador_L>marcador_V
			GROUP BY equipo.id_equipo
			HAVING count(id_partido)=(SELECT MAX(numpartido) 
				FROM (SELECT equipo.id_equipo AS Equipo, count(id_partido) AS numpartido
				FROM equipo JOIN partido ON equipo.id_equipo=partido.id_equipo_L
				WHERE marcador_L>marcador_V
				GROUP BY id_equipo) as t1));
--De las 5 ciudades con mayor número de habitantes ¿Cuál es la que tiene la menor cantidad de equipos?
SELECT nombre_ciudad
FROM(
	SELECT count(id_equipo) AS contador, nombre_ciudad
	FROM ciudad NATURAL JOIN equipo_ciudad
	WHERE id_ciudad IN(SELECT id_ciudad
			   FROM ciudad
			   ORDER BY habitantes DESC
			   LIMIT 5)
		GROUP BY id_ciudad
		ORDER BY contador
		LIMIT 1) AS brl
/*
El nombre completo de los jugadores que hayan iniciado al menos el 50% de los juegos que jugaron 
y que hayan jugado como Punter o Receiver en un equipo fundado a partir de 1960
*/
-- dado que punter iniciales no hay:
SELECT nombre_jugador, apellido_jugador
FROM jugador_equipo NATURAL JOIN jugador NATURAL JOIN receiver
WHERE id_equipo IN(SELECT id_equipo
			FROM equipo
			WHERE anyo_fund>=1960)
GROUP BY id_jugador, nombre_jugador, apellido_jugador
HAVING AVG(juegos_iniciados::numeric)>=0.5
-- contándolos aún así:
SELECT nombre_jugador, apellido_jugador
FROM (SELECT * FROM (jugador_equipo NATURAL JOIN jugador NATURAL JOIN punter) UNION SELECT * FROM(jugador_equipo NATURAL JOIN jugador NATURAL JOIN receiver)) as t
WHERE id_equipo IN(SELECT id_equipo
			FROM equipo
			WHERE anyo_fund>=1960)
GROUP BY id_jugador, nombre_jugador, apellido_jugador
HAVING AVG(juegos_iniciados::numeric)>=0.5

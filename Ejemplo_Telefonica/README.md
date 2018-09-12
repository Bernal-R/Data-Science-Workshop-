### Arbol de Decisión Clasificador
## Contexto:
Somos contratados como científicos de datos en una de las mejores compañías en la industria telefónica, la empresa últimamente ha estado perdiendo muchos de clientes... Te dispones a encontrar el porqué y dar posibles soluciones al problema, así que la empresa te da una base de datos de los clientes actuales que posee: 

* ¿Tiene plan internacionales?
* Minutos de llamada al día 
* Minutos de llamadas internacionales 
* Número de reclamos que ha hecho el cliente 
* Número de llamadas internacionales que ha realizado 
* ¿Desafiliado?


## Nuestra tarea:
Tu trabajo es encontrar un patrón del porqué se están desafiliando los clientes, vamos específicamente a crear un perfil de los clientes que tienen mayor posibilidad de desafiliarse para poder ofrecerle mejores servicios y grantizar su permanencia en la compañía.

* 1- Encontrar el perfil de clientes con posibilidad de desafiliarse.
* 2- Proponer soluciones para asegurar la permanencia de los clientes.



## Base de Datos:

Nuestros salidas de clasificación son: 
* No => No desafiliado
* Si => Desafiliado 

 Nuestro set de datos posee 7 varibales: 
* total_day_minutes => Minutos al día 
* number_customer_service_calls => Número de reclamos
* international_plan => ¿Tiene plan internacional?
* total_intl_calls => Número de llamadas internacionales realizadas
* total_intl_minutes => Número de minutos de llamadas internacionales realizadas 

# Modelo Espacio-Temporal Bayesiano de Sismicidad en la Región del Maule (Chile)

Este proyecto implementa un modelo jerárquico bayesiano espacio-temporal en **lenguaje R**, con el objetivo de analizar y predecir la actividad sísmica en la **Región del Maule, Chile**.

##  Descripción general

Se utilizaron datos sísmicos descargados desde el [Centro Sismológico Nacional de Chile](https://www.sismologia.cl/), abarcando el periodo desde **enero de 2005 hasta junio de 2025**.  
>  Los datos de julio 2025 fueron excluidos por estar incompletos.

El modelo permite:
- Estimar la intensidad sísmica mensual por celda geográfica.
- Identificar zonas con mayor riesgo sísmico.
- Generar predicciones para un horizonte de **6 meses**, desde **julio hasta diciembre de 2025**.

## Metodología

El modelo utilizado es un **modelo de conteo** con distribución **Poisson**, incluyendo:

- Efectos espaciales aleatorios (`u[i]`)
- Efectos temporales autoregresivos (`v[t]`)
- Tasa esperada de sismos (`λ[i,t]`)

El modelo se especifica en lenguaje BUGS y es estimado en R a través del paquete `R2OpenBUGS`.

## Requisitos

Este proyecto requiere **OpenBUGS** para ejecutar la estimación bayesiana.

- El instalador de OpenBUGS se encuentra disponible en la carpeta del repositorio.
- También puedes descargarlo desde el sitio oficial:  
  🔗 [https://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-openbugs/](https://www.mrc-bsu.cam.ac.uk/software/bugs/the-bugs-project-openbugs/)

- Asegúrate de que OpenBUGS esté correctamente instalado y accesible desde R (revisar ruta y permisos si usas Windows).

##  Archivos del repositorio
Todos los scripts, datos y modelos necesarios están disponibles directamente en este repositorio:


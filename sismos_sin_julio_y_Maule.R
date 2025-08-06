library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)  
library(coda)
library(R2OpenBUGS)
library(gridExtra)

# Desactivar S2 (evita errores topológicos)
sf_use_s2(FALSE)


################################################################################

# Datos
regiones <- st_read("C:\\Users\\fabia\\Desktop\\Proyectos\\Proyecto de geo\\Enviar\\Datos\\gadm41_CHL_1.json") |>
  st_make_valid()

datos <- read_csv("C:\\Users\\fabia\\Desktop\\Proyectos\\Proyecto de geo\\Enviar\\Datos\\sismos_csn_2005_2025.csv")

################################################################################

# Convertir a objeto espacial
datos_sf <- st_as_sf(datos, coords = c("longitud", "latitud"), crs = 4326)

# Unión espacial
datos_con_region <- st_join(datos_sf, regiones)

# Convertir a data.frame (si lo deseas)
datos_final <- as.data.frame(datos_con_region)
sf_use_s2(TRUE)

datos_chile <- datos_final %>%
  filter(COUNTRY == "Chile")
nrow(datos_chile)
unique(datos_chile$NAME_1)

#Conteo de Datos por region
conteo_por_region <- datos_final %>%
  filter(COUNTRY == "Chile") %>%   # filtra solo sismos dentro de Chile
  count(NAME_1, sort = TRUE)       # cuenta por región y ordena de mayor a menor

# Gráfico de barras para el conteo de sismos por región
ggplot(conteo_por_region, aes(x = reorder(NAME_1, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Cantidad de sismos por región (2005–2025)",
    x = "Región",
    y = "Cantidad de sismos"
  ) +
  theme_minimal(base_size = 14)

conteo_region_anio <- datos_final %>%
  filter(COUNTRY == "Chile") %>%              # Solo sismos en Chile
  mutate(anio = year(fecha_local)) %>%        # Crear nueva columna con el año
  group_by(NAME_1, anio) %>%                  # Agrupar por región y año
  summarise(cantidad = n(), .groups = "drop") # Contar
print(conteo_region_anio)

ggplot(conteo_region_anio, aes(x = anio, y = cantidad, color = NAME_1)) +
  geom_line() +
  labs(title = "Sismos por año en cada región de Chile",
       x = "Año", y = "Cantidad de sismos", color = "Región") +
  theme_minimal()

####ELIMINACION DEL MES DE JULIO 2025 POR QUE NO ESTA COMPLETO 

datos_filtrados <- datos_final %>%
  filter(!(year(fecha_local) == 2025 & month(fecha_local) == 7))

# Ver cuántos datos quedan de JULIO 2025 (debería ser 0)
datos_filtrados %>%
  filter(year(fecha_local) == 2025 & month(fecha_local) == 7) %>%
  nrow()

conteo_region_anio <- datos_filtrados %>%
  filter(COUNTRY == "Chile") %>%              # Solo sismos en Chile
  mutate(anio = year(fecha_local)) %>%        # Crear nueva columna con el año
  group_by(NAME_1, anio) %>%                  # Agrupar por región y año
  summarise(cantidad = n(), .groups = "drop") # Contar
print(conteo_region_anio)

ggplot(conteo_region_anio, aes(x = anio, y = cantidad, color = NAME_1)) +
  geom_line() +
  labs(title = "Sismos por año en cada región de Chile",
       x = "Año", y = "Cantidad de sismos", color = "Región") +
  theme_minimal()

#Tomaremos los datos del MAULE

# Filtrar datos solo para la región del Maule y que estén en Chile
datos_maule <- datos_final %>%
  filter(COUNTRY == "Chile", NAME_1 == "Maule")

datos_maule %>%
  mutate(anio = year(fecha_local)) %>%
  count(anio, sort = FALSE)

# Asegurarse de tener la región del Maule en sf
region_maule <- regiones %>% filter(NAME_1 == "Maule")

# Crear cuadrícula (0.1 grados de resolución, puedes ajustar)
grid_maule <- st_make_grid(region_maule,
                           cellsize = 0.1,
                           square = TRUE)

# Convertir a objeto sf y mantener solo las celdas que intersectan con Maule
grid_maule_sf <- st_sf(geometry = grid_maule) %>%
  st_intersection(region_maule)


grid_maule_sf$cell_id <- seq_len(nrow(grid_maule_sf))

ggplot() +
  geom_sf(data = grid_maule_sf, fill = NA, color = "black") +
  geom_sf(data = region_maule, fill = NA, color = "red") +
  labs(title = "Cuadrícula sobre Región del Maule") +
  theme_minimal()


# Reconvertir usando la columna de geometría existente

sismos_maule_sf <- st_as_sf(datos_maule)

coords <- st_coordinates(sismos_maule_sf)
sismos_maule_sf$longitud <- coords[, 1]
sismos_maule_sf$latitud  <- coords[, 2]

# Reparar geometría del grid
grid_maule_sf <- st_make_valid(grid_maule_sf)

# Unión espacial segura
sismos_maule_grid <- st_join(sismos_maule_sf, grid_maule_sf)

# Crear variable temporal mensual
sismos_maule_grid <- sismos_maule_grid %>%
  filter(!is.na(cell_id)) %>%
  mutate(mes = floor_date(fecha_local, "month"))

# Rango temporal
todos_los_meses <- seq(
  from = min(sismos_maule_grid$mes),
  to   = max(sismos_maule_grid$mes),
  by   = "month"
)

# Expandir combinaciones
combinaciones <- expand.grid(
  cell_id = grid_maule_sf$cell_id,
  mes     = todos_los_meses
)

# Conteo real
conteo <- sismos_maule_grid %>%
  group_by(cell_id, mes) %>%
  summarise(cantidad = n(), .groups = "drop")

# Unir con combinaciones para agregar ceros
panel_completo <- left_join(combinaciones, conteo, by = c("cell_id", "mes")) %>%
  mutate(cantidad = ifelse(is.na(cantidad), 0, cantidad))

# Codificación de IDs enteros consecutivos
panel_completo <- panel_completo %>%
  arrange(cell_id, mes) %>%
  mutate(
    id_cell = as.integer(factor(cell_id)),
    id_time = as.integer(factor(mes))
  )

# Extraer centroides del grid
grid_coords <- st_centroid(grid_maule_sf)

coords_df <- grid_coords %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(cell_id = grid_maule_sf$cell_id)

# Unir coordenadas al panel
panel_completo <- left_join(panel_completo, coords_df, by = "cell_id")

##### Preparar los datos en R para exportar a OpenBUGS ##### 


# Filtrar panel a solo celdas que han tenido al menos 1 sismo en todo el periodo
celdas_con_datos <- panel_completo %>%
  group_by(id_cell) %>%
  summarise(total = sum(cantidad)) %>%
  filter(total > 0)

# Panel filtrado
panel_filtrado <- panel_completo %>%
  filter(id_cell %in% celdas_con_datos$id_cell)

# Reindexar IDs
panel_filtrado <- panel_filtrado %>%
  arrange(id_cell, id_time) %>%
  mutate(
    id_cell = as.integer(factor(id_cell)),  # REASIGNAR
    id_time = as.integer(factor(id_time))
  )

# Reordenar
datos_bugs <- panel_completo %>%
  arrange(id_cell, id_time)

# Número de observaciones, celdas y tiempos
Nobs   <- nrow(datos_bugs)
Ncells <- length(unique(datos_bugs$id_cell))
Ntime  <- length(unique(datos_bugs$id_time))

datos_openbugs <- list(
  y      = panel_filtrado$cantidad,
  cell   = panel_filtrado$id_cell,
  time   = panel_filtrado$id_time,
  Nobs   = nrow(panel_filtrado),
  Ncells = length(unique(panel_filtrado$id_cell)),
  Ntime  = length(unique(panel_filtrado$id_time))
)

inits <- function() {
  list(
    beta0 = 0,
    u = rnorm(datos_openbugs$Ncells, 0, 0.1),
    v = cumsum(rnorm(datos_openbugs$Ntime, 0, 0.1)),
    tau_u = 1,
    tau_v = 1
  )
}

ModeloOpb= "model {
  # Likelihood
  for (i in 1:Nobs) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- beta0 + u[cell[i]] + v[time[i]]
  }

  # Priors
  beta0 ~ dnorm(0, 0.001)

  for (j in 1:Ncells) {
    u[j] ~ dnorm(0, tau_u)
  }
  tau_u ~ dgamma(0.1, 0.1)

  v[1] ~ dnorm(0, 0.001)
  for (t in 2:Ntime) {
    v[t] ~ dnorm(v[t-1], tau_v)
  }
  tau_v ~ dgamma(1, 0.1)
}
"

#Guardar y ejecutar el modelo
cat(ModeloOpb, file = "modelo_poisson_space_time.txt")

# Parámetros a monitorizar
params <- c("beta0", "u", "v", "tau_u", "tau_v")


print("Recordar instalar openbugs")
print("3 horas aprox.")

modelo_bugs <- bugs(
  data = datos_openbugs,
  inits = list(inits(), inits(), inits()),  # 3 cadenas
  parameters.to.save = params,
  model.file = "modelo_poisson_space_time.txt",
  n.chains = 3,
  n.iter = 5000,
  n.burnin = 1000,
  n.thin = 2,
  debug = FALSE,
  DIC = TRUE
)

#save.image(file = "entorno_sismos_sin_julio_y_Maule_3107.RData")

##############################################################3
#NOTA : Se puede cargar este entorno para evitar la espera 

#load("entorno_sismos_sin_julio_y_Maule_3107.RData")
###############################################################

#### Validacion del modelo  ###

par(mfrow = c(2, 3))  # 2 filas, 3 columnas

# Traceplots
ts.plot(modelo_bugs$sims.list$beta0, main = "Traceplot beta0", ylab = "beta0", col = 1:3)
ts.plot(modelo_bugs$sims.list$tau_u, main = "Traceplot tau_u", ylab = "tau_u", col = 1:3)
ts.plot(modelo_bugs$sims.list$tau_v, main = "Traceplot tau_v", ylab = "tau_v", col = 1:3)

# Histogramas
hist(modelo_bugs$sims.list$beta0, breaks = 50, col = "darkblue", main = "Histograma beta0", xlab = "beta0")
hist(modelo_bugs$sims.list$tau_u, breaks = 50, col = "darkred", main = "Histograma tau_u", xlab = "tau_u")
hist(modelo_bugs$sims.list$tau_v, breaks = 50, col = "darkgreen", main = "Histograma tau_v", xlab = "tau_v")

par(mfrow = c(1, 1))  # restaurar

##AFC
par(mfrow = c(1, 3))  

acf(modelo_bugs$sims.list$beta0,main = "ACF para beta0")
acf(modelo_bugs$sims.list$tau_u,main = "ACF para tau_u")
acf(modelo_bugs$sims.list$tau_v,main = "ACF para tau_v")

par(mfrow = c(1, 1))  # restaurar

param_names <- dimnames(modelo_bugs$sims.array)[[2]]

mcmc_list <- mcmc.list(
  lapply(1:dim(modelo_bugs$sims.array)[3], function(i) {
    chain <- modelo_bugs$sims.array[, , i]
    colnames(chain) <- param_names  
    mcmc(chain)
  })
)

gelman_diag <- gelman.diag(mcmc_list, multivariate = FALSE)
print(gelman_diag$psrf)

parameters <- c("beta0", "tau_u", "tau_v")

plots <- list() # Lista para guardar los plots

for (param in parameters) {
  if (!is.null(modelo_bugs$sims.list[[param]])) {
    values <- modelo_bugs$sims.list[[param]]
    hpd <- HPDinterval(as.mcmc(values), prob = 0.95)
    hpd_low <- hpd[1]
    hpd_high <- hpd[2]
    
    plots[[param]] <- ggplot(data.frame(values = values), aes(x = values)) +
      geom_histogram(bins = 30, color = "black", fill = "darkblue", alpha = 0.7) +
      geom_vline(xintercept = hpd_low, color = "red", linetype = "dashed", size = 1) +
      geom_vline(xintercept = hpd_high, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = paste("Distribución Posterior y HDP de", param),
        x = param,
        y = "Frecuencia"
      ) +
      theme_minimal()
  }
}
grid.arrange(grobs = plots, ncol = 2, top = "Distribuciones Posteriores con HDP al 95%")

# Calcular estimadores bayesianos
for (param in parameters) {
  if (!is.null(modelo_bugs$sims.list[[param]])) {
    muestras <- modelo_bugs$sims.list[[param]]
    
    media   <- mean(muestras)
    mediana <- median(muestras)
    sd_post <- sd(muestras)
    hpd     <- HPDinterval(as.mcmc(muestras), prob = 0.95)
    
    cat("====================================\n")
    cat("Parámetro:", param, "\n")
    cat("Media posterior     :", round(media, 4), "\n")
    cat("Mediana posterior   :", round(mediana, 4), "\n")
    cat("Desv. Est. posterior:", round(sd_post, 4), "\n")
    cat("HDP 95%             :", paste0("[", round(hpd[1], 4), ", ", round(hpd[2], 4), "]\n"))
  }
}

################################################################################3+
## Analisis de U
# Extraer solo las celdas con datos usados en el modelo
celdas_usadas <- unique(panel_filtrado$cell_id)
grid_filtrado <- grid_maule_sf %>%
  filter(cell_id %in% celdas_usadas) %>%
  arrange(match(cell_id, celdas_usadas)) 

# Asignar los efectos espaciales estimados
# Extraer muestras MCMC del efecto espacial u
muestras_u <- as.mcmc(modelo_bugs$sims.list$u)

# Calcular resumen (media, etc.)
u_est <- summary(muestras_u)$statistics

grid_filtrado$u_mean <- u_est[, "Mean"]
ggplot(grid_filtrado) +
  geom_sf(aes(fill = u_mean), color = NA) +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Efecto espacial estimado (u)", fill = "u") +
  theme_minimal()

#Mapa del error estándar o desviación (SD) del efecto espacial
grid_filtrado$u_sd <- u_est[, "SD"]
ggplot(grid_filtrado) +
  geom_sf(aes(fill = u_sd), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Desviación estándar del efecto espacial", fill = "SD(u)") +
  theme_minimal()

#Mapa de probabilidad de excedencia
grid_filtrado$prob_excede <- pnorm(0, mean = grid_filtrado$u_mean, sd = grid_filtrado$u_sd, lower.tail = FALSE)
ggplot(grid_filtrado) +
  geom_sf(aes(fill = prob_excede), color = NA) +
  scale_fill_viridis_c(option = "cividis") +
  labs(title = "Probabilidad de que u > 0", fill = "P(u > 0)") +
  theme_minimal()

#analsiss de v
# Si v es una matriz, basta con convertirla directamente:
muestras_v <- as.mcmc(modelo_bugs$sims.list$v)

#  resumen:
v_est <- summary(muestras_v)$statistics

ggplot(data.frame(v = v_est[, "Mean"]), aes(x = v)) +
  geom_histogram(bins = 30, fill = "darkblue", color = "black") +
  labs(title = "Histograma de efectos temporales v", x = "v[t]") +
  theme_minimal()

v_muestras <- modelo_bugs$sims.list$v
v_q <- apply(v_muestras, 2, quantile, probs = c(0.025, 0.5, 0.975))

v_df <- data.frame(
  tiempo = 1:ncol(v_muestras),
  q025 = v_q[1, ],
  media = v_q[2, ],
  q975 = v_q[3, ]
)

ggplot(v_df, aes(x = tiempo, y = media)) +
  geom_ribbon(aes(ymin = q025, ymax = q975), fill = "skyblue", alpha = 0.4) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Efecto temporal estimado (v[t])",
       x = "Tiempo", y = "Efecto temporal") +
  theme_minimal()

######PREDICCIONES

# Extraer muestras MCMC del efecto temporal (v) y su precisión (tau_v)
v_samples <- modelo_bugs$sims.list$v
tau_v_samples <- modelo_bugs$sims.list$tau_v

# Número de iteraciones MCMC y meses a predecir
n_samples <- nrow(v_samples)
n_future <- 6  ####PREDICCION DE LOS PROX. 6 MESES

# Inicializar matriz para guardar simulaciones futuras de v
v_future <- matrix(NA, nrow = n_samples, ncol = n_future)

# Simular secuencia autorregresiva v[t] hacia adelante
for (i in 1:n_samples) {
  # Primer valor futuro condicionado al último observado
  v_future[i, 1] <- rnorm(1, mean = v_samples[i, ncol(v_samples)],
                          sd = 1 / sqrt(tau_v_samples[i]))
  # Simulación autorregresiva para los siguientes meses
  for (t in 2:n_future) {
    v_future[i, t] <- rnorm(1, mean = v_future[i, t - 1],
                            sd = 1 / sqrt(tau_v_samples[i]))
  }
}
# Obtener muestras MCMC de beta0 y del efecto espacial u
beta0_samples <- modelo_bugs$sims.list$beta0
u_samples <- modelo_bugs$sims.list$u

# Total de celdas (según las observadas en el modelo)
Ncells <- length(unique(panel_filtrado$id_cell))

# Inicializar array para guardar predicciones λ
lambda_future <- array(NA, dim = c(n_samples, Ncells, n_future))

# Simular λ para cada celda y cada mes futuro
for (i in 1:n_samples) {
  for (j in 1:Ncells) {
    for (t in 1:n_future) {
      lambda_future[i, j, t] <- exp(beta0_samples[i] + u_samples[i, j] + v_future[i, t])
    }
  }
}
# Calcular media posterior y percentiles 2.5% y 97.5% por celda y mes
lambda_mean  <- apply(lambda_future, c(2, 3), mean)
lambda_q025  <- apply(lambda_future, c(2, 3), quantile, probs = 0.025)
lambda_q975  <- apply(lambda_future, c(2, 3), quantile, probs = 0.975)
# Agregar λ estimado del primer mes al shapefile filtrado
grid_filtrado$lambda_pred <- lambda_mean[, 1]

ggplot(grid_filtrado) +
  geom_sf(aes(fill = lambda_pred), color = NA) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Predicción de tasa de sismos (1er mes futuro) - Julio 2025", fill = "λ estimado") +
  theme_minimal()
# Calcular centroides de cada celda para mapear coordenadas
coords_df <- st_coordinates(st_centroid(grid_filtrado))
colnames(coords_df) <- c("X", "Y")

# Repetir coordenadas para cada mes futuro
coord_rep <- coords_df[rep(1:nrow(coords_df), times = n_future), ]

# Crear DataFrame con predicciones por celda y mes
df_predicciones <- data.frame(
  cell_id     = rep(grid_filtrado$cell_id, times = n_future),
  mes_futuro  = rep(1:n_future, each = nrow(grid_filtrado)),
  lambda_mean = as.vector(lambda_mean),
  lambda_q025 = as.vector(lambda_q025),
  lambda_q975 = as.vector(lambda_q975),
  X           = coord_rep[, 1],
  Y           = coord_rep[, 2]
)

head(df_predicciones)
# Filtrar predicciones del primer mes futuro
df_mes1 <- df_predicciones[df_predicciones$mes_futuro == 1, ]

# Definir umbral de riesgo como percentil 95 de λ
umbral_riesgo <- quantile(df_predicciones$lambda_mean, 0.95)

# Filtrar zonas que exceden el umbral en el mes 1
zonas_riesgo <- df_mes1 %>%
  filter(lambda_mean > umbral_riesgo)

# Mapa con zonas de mayor riesgo destacadas
ggplot() +
  # Capa de mapa de calor con lambda esperado
  geom_tile(data = df_mes1, aes(x = X, y = Y, fill = lambda_mean)) +
  # Capa de puntos rojos para zonas de mayor riesgo
  geom_point(data = zonas_riesgo, aes(x = X, y = Y), color = "red", size = 2, shape = 4) +
  # Capa de contorno de la región del Maule
  geom_sf(data = region_maule, fill = NA, color = "black", size = 0.7) +
  # Estilo y etiquetas
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Zonas de Mayor Riesgo Sísmico (1er mes futuro) - Julio 2025",
    fill = "λ esperado"
  ) +
  coord_sf() +
  theme_minimal()

zonas_riesgo_tabla <- zonas_riesgo %>%
  select(cell_id, X, Y, lambda_mean, lambda_q025, lambda_q975) %>%
  arrange(desc(lambda_mean))

# Mostrar las 10 zonas con mayor λ esperado
((zonas_riesgo_tabla))


#  los 6 mapas de predicción usando grid_filtrado y lambda_mean
graficos <- list()

for (mes in 1:n_future) {
  grid_filtrado$lambda_pred <- lambda_mean[, mes]
  
  g <- ggplot(grid_filtrado) +
    geom_sf(aes(fill = lambda_pred), color = NA) +
    scale_fill_viridis_c(option = "inferno") +
    labs(
      title = paste0("Predicción de tasa de sismos (mes ", mes, " futuro)"),
      fill = "λ estimado"
    ) +
    theme_minimal()
  
  graficos[[mes]] <- g
}

# Mostrar en grilla 2x3
do.call(grid.arrange, c(grobs = graficos, ncol = 3))

graficos_riesgo <- list()

for (mes in 1:n_future) {
  df_mes <- df_predicciones[df_predicciones$mes_futuro == mes, ]
  zonas_riesgo <- df_mes %>% filter(lambda_mean > umbral_riesgo)
  
  g <- ggplot() +
    geom_tile(data = df_mes, aes(x = X, y = Y, fill = lambda_mean)) +
    geom_point(data = zonas_riesgo, aes(x = X, y = Y), color = "red", size = 2, shape = 4) +
    geom_sf(data = region_maule, fill = NA, color = "black", size = 0.7) +
    scale_fill_viridis_c(option = "plasma") +
    labs(title = paste0("Zonas de Mayor Riesgo Sísmico (mes ", mes, " futuro)"),
         fill = "λ esperado") +
    coord_sf() +
    theme_minimal()
  
  graficos_riesgo[[mes]] <- g
}

# Mostrar todos juntos
do.call(grid.arrange, c(grobs = graficos_riesgo, ncol = 3))





















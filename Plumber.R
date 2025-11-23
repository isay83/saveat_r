# saveat_r/plumber.R

library(plumber)
library(mongolite)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(readxl)

# --- 1. CONFIGURACIÓN DE BASE DE DATOS ---
# IMPORTANTE: Reemplaza esto con TU string de conexión real (el mismo de tu .env en saveat_api)
# Si es local suele ser: "mongodb://localhost:27017/tu_nombre_de_bd"
mongo_url <- Sys.getenv("MONGO_URI", "mongodb+srv://saveat_api:saveat_api_pass@saveat.varsj0a.mongodb.net/saveat")

# Función auxiliar para conectar a una colección específica
get_collection <- function(collection_name) {
  mongo(collection = collection_name, url = mongo_url)
}

# --- 2. DEFINICIÓN DE LA API ---

#* @apiTitle Saveat Analytics API
#* @apiDescription API de R para generar gráficas y estadísticas avanzadas

#* Habilitar CORS (Para que tu Next.js pueda pedir datos sin bloqueo)
#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  plumber::forward()
}

#* Prueba de conexión
#* @get /health
function() {
  list(status = "online", message = "R is ready to cook data! 🍳")
}

#* Gráfica de Estado de Reservas (Ventas vs Cancelados)
#* Devuelve una imagen PNG
#* @serializer contentType list(type="image/png")
#* @get /charts/reservations-status
function() {
  # 1. Conectar y extraer datos de 'reservations'
  res_col <- get_collection("reservations")
  # Traemos solo el campo 'status' para ser eficientes
  data <- res_col$find(fields = '{"status": 1, "_id": 0}')
  
  # 2. Procesar datos con dplyr
  if (nrow(data) == 0) {
    # Si no hay datos, creamos un dataframe vacío para evitar error
    plot_data <- data.frame(status = c("Sin datos"), count = c(0))
  } else {
    plot_data <- data %>%
      group_by(status) %>%
      summarise(count = n())
  }
  
  # 3. Crear gráfica con ggplot2
  p <- ggplot(plot_data, aes(x = status, y = count, fill = status)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = "Estado de las Reservas", x = "Estado", y = "Cantidad") +
    scale_fill_manual(values = c("recogido" = "#465FFF", "pendiente" = "#FFB020", "cancelado" = "#D14343"))
  
  # 4. Convertir la gráfica a imagen temporal y devolverla
  tmp <- tempfile(fileext = ".png")
  ggsave(tmp, plot = p, width = 6, height = 4)
  readBin(tmp, "raw", n = file.info(tmp)$size)
}

#* Datos crudos de Inventario (Para tablas avanzadas en JS)
#* @get /data/inventory-summary
function() {
  prod_col <- get_collection("products")
  # Buscamos productos con stock bajo (ej. menos de 5)
  data <- prod_col$find(
    query = '{"quantity_available": { "$lt": 5 }}',
    fields = '{"name": 1, "quantity_available": 1, "_id": 0}'
  )
  return(data)
}










# FUENTES DE DIVERSAS DE DATOS -----------------------------------

# --- FUENTE 1: FOOD BANK (Reservas Mongo) ---

# --- SECCIÓN: MÉTRICAS GENERALES (Cards Superiores) ---

#* Devuelve totales y crecimiento diario de Usuarios y Órdenes
#* @get /api/general-metrics
function() {
  # Usamos tryCatch para capturar cualquier error y que no explote el servidor (Error 500)
  tryCatch({
    
    # 1. Definir el punto de corte (Hoy a la medianoche)
    today_start <- as.POSIXct(Sys.Date())
    
    # --- A. USUARIOS (Clientes) ---
    user_col <- get_collection("users")
    # Obtenemos los datos. Si falla la conexión, saltará al bloque 'error'
    users_data <- user_col$find(fields = '{"createdAt": 1, "_id": 0}')
    
    # Inicializamos valores seguros
    total_users <- nrow(users_data)
    new_users_today <- 0
    
    # Verificamos SIEMPRE:
    # 1. Que haya datos (total_users > 0)
    # 2. Que la columna 'createdAt' exista en el dataframe
    if (total_users > 0 && "createdAt" %in% names(users_data)) {
      # Contamos filtrando los nulos (na.rm = TRUE) para evitar errores
      new_users_today <- sum(users_data$createdAt >= today_start, na.rm = TRUE)
    }
    
    # Cálculo seguro del crecimiento (evitar división por cero)
    users_yesterday <- total_users - new_users_today
    user_growth <- 0
    
    if (users_yesterday > 0) {
      user_growth <- round((new_users_today / users_yesterday) * 100, 2)
    } else if (new_users_today > 0) {
      user_growth <- 100 # Crecimiento infinito (antes era 0, hoy hay algo)
    }
    
    # --- B. ÓRDENES (Reservations) ---
    res_col <- get_collection("reservations")
    orders_data <- res_col$find(fields = '{"createdAt": 1, "_id": 0}')
    
    total_orders <- nrow(orders_data)
    new_orders_today <- 0
    
    if (total_orders > 0 && "createdAt" %in% names(orders_data)) {
      new_orders_today <- sum(orders_data$createdAt >= today_start, na.rm = TRUE)
    }
    
    orders_yesterday <- total_orders - new_orders_today
    order_growth <- 0
    
    if (orders_yesterday > 0) {
      order_growth <- round((new_orders_today / orders_yesterday) * 100, 2)
    } else if (new_orders_today > 0) {
      order_growth <- 100
    }
    
    # Retornamos el JSON exitoso
    list(
      totalUsers = total_users,
      userGrowth = user_growth,
      totalOrders = total_orders,
      orderGrowth = order_growth
    )
    
  }, error = function(e) {
    # Si algo falla, imprimimos el error en la consola de RStudio para que tú lo veas
    print(paste("ERROR GRAVE EN METRICS:", e$message))
    
    # Y devolvemos un JSON de emergencia con ceros, para que Next.js no se rompa
    list(
      totalUsers = 0,
      userGrowth = 0,
      totalOrders = 0,
      orderGrowth = 0,
      error = e$message
    )
  })
}

#* Obtener estado de reservas en formato JSON para ApexCharts
#* @get /api/reservations-json
function() {
  # 1. Conectar a la colección 'reservations'
  res_col <- get_collection("reservations")
  
  # 2. Traer solo el campo status (eficiencia)
  data <- res_col$find(fields = '{"status": 1, "_id": 0}')
  
  if (nrow(data) == 0) {
    return(list(categories = c("Sin datos"), data = c(0)))
  }
  
  # 3. Contar cuántas hay de cada estado
  summary_data <- data %>%
    group_by(status) %>%
    summarise(total = n())
  
  # 4. Devolver JSON limpio para el Frontend
  list(
    categories = summary_data$status,
    data = summary_data$total
  )
}










# --- FUENTE 2: INEGI (Mapa Interactivo - Pobreza 2024) ---

#* Devuelve datos de pobreza 2024 filtrados para el mapa
#* @param type El tipo de pobreza ("total", "moderada", "extrema")
#* @param min_val El porcentaje mínimo para filtrar
#* @get /api/inegi-map-data
function(type = "extrema", min_val = 0) {
  library(readxl)
  library(dplyr)
  library(stringi) # Para quitar acentos si es necesario
  
  # 1. LEER EL ARCHIVO
  # Usamos 'col_names = FALSE' para leer todo como datos y nosotros poner los nombres
  # Ajustamos 'range' para leer solo las columnas de 2024 (Las últimas 3 columnas + Entidad)
  # Basado en tu archivo: Columna A es Entidad. Las ultimas 3 son 2024.
  
  # Leemos todo el archivo sin encabezados primero para no liarnos con el doble header
  raw_data <- read_excel("Hogares_15.xlsx", col_names = FALSE)
  
  # 2. SELECCIONAR DATOS (Filtrar columnas y filas)
  # La fila 1 y 2 son encabezados. Los datos empiezan en la 3.
  # Columna 1 = Entidad
  # Columna 14 = Total 2024
  # Columna 15 = Pobreza Moderada 2024
  # Columna 16 = Pobreza Extrema 2024
  
  df_clean <- raw_data %>%
    slice(3:n()) %>% # Nos saltamos las primeras 2 filas de títulos
    select(
      Entidad = ...1,
      Total = ...14,
      Moderada = ...15,
      Extrema = ...16
    ) %>%
    filter(!is.na(Entidad)) # Quitamos filas vacías al final si las hay
  
  # 3. LIMPIEZA DE DATOS
  df_clean <- df_clean %>%
    mutate(
      # Convertir a números (vienen como texto por el header sucio)
      Total = as.numeric(Total),
      Moderada = as.numeric(Moderada),
      Extrema = as.numeric(Extrema),
      
      # Calcular Porcentajes (El archivo trae miles de personas, no %, así que lo calculamos)
      # Fórmula: (Pobreza / Total Población) * 100
      Pct_Moderada = round((Moderada / Total) * 100, 1),
      Pct_Extrema = round((Extrema / Total) * 100, 1),
      Pct_Total = round(((Moderada + Extrema) / Total) * 100, 1),
      
      # Limpiar nombres de estados para el Mapa GeoJSON
      # El mapa usa nombres simples: "Veracruz" en lugar de "Veracruz de Ignacio de la Llave"
      Entidad = case_when(
        Entidad == "Coahuila de Zaragoza" ~ "Coahuila",
        Entidad == "Michoacán de Ocampo" ~ "Michoacán",
        Entidad == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
        Entidad == "México" ~ "Estado de México", # A veces el mapa lo pide así
        TRUE ~ Entidad
      )
    )
  
  # 4. SELECCIONAR COLUMNA SEGÚN EL FILTRO DEL USUARIO
  col_seleccionada <- switch(type,
                             "total" = "Pct_Total",
                             "moderada" = "Pct_Moderada",
                             "extrema" = "Pct_Extrema",
                             "Pct_Extrema" # Default
  )
  
  # 5. FILTRAR Y RESPONDER
  result <- df_clean %>%
    select(state = Entidad, value = all_of(col_seleccionada)) %>%
    filter(value >= as.numeric(min_val)) %>%
    arrange(desc(value))
  
  return(result)
}










# --- FUENTE 3: REDES SOCIALES (Google Trends - Versión Robusta Windows) ---

social_cache <- NULL
social_cache_time <- NULL

#* Devuelve tendencias (Con manejo de errores de columnas)
#* @get /api/social-trends
function() {
  library(gtrendsR)
  library(dplyr)
  library(lubridate)
  
  # 1. CACHÉ
  if (!is.null(social_cache) && difftime(Sys.time(), social_cache_time, units = "mins") < 60) {
    print("Usando datos cacheados de Social Trends...")
    return(social_cache)
  }
  
  print("Consultando a Google Trends en vivo...")
  
  keywords <- c("banco de alimentos", "desperdicio de comida")
  
  tryCatch({
    res <- gtrends(keyword = keywords, geo = "MX", time = "today 3-m")
    
    # --- A. GRÁFICA (Interest over time) ---
    interest <- res$interest_over_time
    interest$hits <- as.numeric(replace(interest$hits, interest$hits == "<1", 0))
    interest$date <- as.Date(interest$date)
    
    # Día de la semana (Manual para evitar errores de idioma)
    interest$weekday_num <- wday(interest$date) 
    dias_semana <- c("Domingo", "Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado")
    interest$weekday <- dias_semana[interest$weekday_num]
    
    # --- B. INSIGHTS (Calculados) ---
    # Tendencia
    last_week_hits <- tail(interest$hits, 7)
    avg_last <- mean(last_week_hits, na.rm = TRUE)
    avg_total <- mean(interest$hits, na.rm = TRUE)
    
    trend_status <- "Estable"
    if (avg_last > (avg_total * 1.1)) trend_status <- "En Aumento 🚀"
    if (avg_last < (avg_total * 0.9)) trend_status <- "A la baja 📉"
    
    # Mejor Día
    best_day_df <- interest %>%
      group_by(weekday) %>%
      summarise(avg_hits = mean(hits, na.rm = TRUE)) %>%
      arrange(desc(avg_hits)) %>%
      slice(1)
    best_day <- as.character(best_day_df$weekday)
    
    # --- C. PALABRAS CLAVE (Aquí estaba el error) ---
    top_topics <- c("Ayuda", "Comida", "Donación") # Default seguro
    
    # Verificamos que existan datos relacionados
    if (!is.null(res$related_queries) && is.data.frame(res$related_queries)) {
      
      # VERIFICACIÓN DE SEGURIDAD: ¿Existe la columna 'related_queries'?
      if ("related_queries" %in% names(res$related_queries)) {
        # Si existe, filtramos por "rising"
        related <- res$related_queries %>%
          filter(related_queries == "rising") %>%
          arrange(desc(subject)) %>%
          head(3)
        
        if (nrow(related) > 0) top_topics <- as.character(related$value)
        
      } else if ("value" %in% names(res$related_queries)) {
        # Si no existe la columna de tipo, tomamos los primeros valores que haya
        top_topics <- head(as.character(res$related_queries$value), 3)
      }
    }
    
    # --- D. RESPUESTA ---
    data_banco <- interest %>% filter(keyword == "banco de alimentos")
    data_desp <- interest %>% filter(keyword == "desperdicio de comida")
    
    final_response <- list(
      chart = list(
        categories = as.character(data_banco$date),
        series = list(
          list(name = "Banco de Alimentos", data = data_banco$hits),
          list(name = "Desperdicio de Comida", data = data_desp$hits)
        )
      ),
      insights = list(
        trend = trend_status,
        best_day = best_day,
        keywords = top_topics
      )
    )
    
    # Guardar Caché
    social_cache <<- final_response
    social_cache_time <<- Sys.time()
    
    return(final_response)
    
  }, error = function(e) {
    print(paste("ERROR CONTROLADO EN GTRENDS:", e$message))
    # Si falla, intentamos devolver caché antigua o error limpio
    if (!is.null(social_cache)) return(social_cache)
    return(list(error = "No se pudo conectar con Google Trends. Intenta más tarde."))
  })
}










# --- FUENTE 4: ARCHIVOS EXCEL (Metas) ---
#* Lee una meta de un Excel y devuelve el porcentaje
#* @get /api/excel-goals
function() {
  # 1. Crear un Excel temporal si no existe (Para que te funcione el ejemplo ya mismo)
  # En tu proyecto real, leerías tu archivo: "C:/mis_datos/metas.xlsx"
  file_path <- "metas_temp.xlsx"
  
  if (!file.exists(file_path)) {
    df <- data.frame(
      Concepto = c("Meta Anual", "Recaudado Actual"),
      Valor = c(100000, 75500) # 75,500 de 100,000
    )
    writexl::write_xlsx(df, file_path)
  }
  
  # 2. Leer el Excel
  datos <- readxl::read_excel(file_path)
  
  # 3. Calcular porcentaje
  meta <- datos$Valor[datos$Concepto == "Meta Anual"]
  actual <- datos$Valor[datos$Concepto == "Recaudado Actual"]
  
  porcentaje <- round((actual / meta) * 100, 1)
  
  list(
    percentage = porcentaje,
    label = paste0("$", actual / 1000, "k / $", meta / 1000, "k")
  )
}
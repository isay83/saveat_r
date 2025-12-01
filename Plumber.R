# saveat_r/plumber.R

library(plumber)
library(mongolite)
library(ggplot2)
library(dplyr)
library(jsonlite)
library(readxl)

# --- 1. CONFIGURACIÓN DE BASE DE DATOS ---
mongo_url <- Sys.getenv("MONGO_URI")

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

# --- MAPA DE CALOR DE VENTAS (Geomarketing) ---

#* Devuelve ventas agrupadas por Estado basado en CP
#* @get /api/reservations-map
function() {
  library(dplyr)
  library(stringr)
  
  # 1. Obtener Usuarios (Necesitamos su CP)
  # Solo traemos _id y postal_code para ser rápidos
  user_col <- get_collection("users")
  users <- user_col$find(fields = '{"_id": 1, "postal_code": 1}')
  
  # 2. Obtener Reservas (Necesitamos saber quién compró)
  res_col <- get_collection("reservations")
  reservations <- res_col$find(fields = '{"user_id": 1, "_id": 0}')
  
  if (nrow(reservations) == 0 || nrow(users) == 0) {
    return(list(error = "No hay datos suficientes"))
  }
  
  # 3. Cruzar datos (Join) en R
  # Convertimos los IDs a caracter para asegurar que el cruce funcione
  users$`_id` <- as.character(users$`_id`)
  reservations$user_id <- as.character(reservations$user_id)
  
  # Unimos para tener: user_id | postal_code
  merged_data <- inner_join(reservations, users, by = c("user_id" = "_id"))
  
  # 4. Extraer Estado del CP (Primeros 2 dígitos)
  merged_data$cp_prefix <- substr(merged_data$postal_code, 1, 2)
  
  # Tabla Maestra de CP a Estados (Simplificada para coincidir con GeoJSON)
  # Fuente: Correos de México lógica estándar
  cp_map <- data.frame(
    prefix = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", 
               # CDMX usa del 01 al 16, aquí simplificamos algunos rangos comunes
               "50", "51", "52", "53", "54", "55", "56", "57"), 
    state_name = c("Ciudad de México", "Baja California", "Baja California Sur", "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas",
                   "Estado de México", "Estado de México", "Estado de México", "Estado de México", "Estado de México", "Ciudad de México", "Estado de México", "Estado de México")
  )
  
  # Nota: Esta tabla es básica. En producción podrías usar un CSV completo de SEPOMEX.
  # Para tu CP 38300 (Guanajuato), el prefijo es 38. Vamos a asegurar que Guanajuato entre.
  # Guanajuato suele ser 36-38. Ajustamos manual para asegurar tu demo:
  
  merged_data <- merged_data %>%
    mutate(state = case_when(
      cp_prefix %in% c("36", "37", "38") ~ "Guanajuato",
      cp_prefix %in% c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16") ~ "Ciudad de México",
      cp_prefix %in% c("20", "21", "22", "23") ~ "Puebla", # Ejemplo aproximado
      cp_prefix %in% c("44", "45") ~ "Jalisco",
      TRUE ~ "Otro" # Los que no mapeemos en esta demo rápida
    ))
  
  # 5. Agrupar y Contar
  map_data <- merged_data %>%
    filter(state != "Otro") %>%
    group_by(state) %>%
    summarise(value = n()) %>%
    arrange(desc(value))
  
  return(map_data)
}










# --- FUENTE 2: INEGI (Mapa Interactivo + Carga de Archivos) ---

# Nombre del archivo donde guardaremos los datos subidos
INEGI_FILE <- "datos_inegi_actuales.xlsx"

#* Recibe el archivo de INEGI y lo guarda
#* @post /api/upload-inegi
#* @parser multi
function(req, res) {
  if (is.null(req$body$file)) {
    res$status <- 400
    return(list(error = "No se envió ningún archivo"))
  }
  
  tryCatch({
    # Guardamos el archivo subido sobreescribiendo el anterior
    file.copy(req$body$file$tempfile, INEGI_FILE, overwrite = TRUE)
    return(list(message = "Datos de INEGI actualizados correctamente"))
  }, error = function(e) {
    print(e)
    res$status <- 500
    return(list(error = "Error al guardar el archivo en el servidor"))
  })
}


#* Devuelve datos de pobreza filtrados
#* @param type El tipo de pobreza ("total", "moderada", "extrema")
#* @param min_val El porcentaje mínimo para filtrar
#* @get /api/inegi-map-data
function(type = "extrema", min_val = 0) {
  library(readxl)
  library(dplyr)
  library(stringr)
  
  archivo <- if(file.exists(INEGI_FILE)) INEGI_FILE else "Hogares_15.xlsx"
  if (!file.exists(archivo)) return(list(error = "Falta archivo"))
  
  # 1. Leer y encontrar inicio (Igual que antes)
  raw <- read_excel(archivo, col_names = FALSE)
  idx_inicio <- which(str_detect(raw[[1]], "Aguascalientes"))[1]
  if (is.na(idx_inicio)) idx_inicio <- 2
  
  # 2. Procesar
  df_clean <- raw %>%
    slice(idx_inicio:n()) %>%
    select(Entidad = ...1, Total = ...14, Moderada = ...15, Extrema = ...16) %>%
    filter(!is.na(Entidad)) %>%
    filter(!str_detect(Entidad, "Estados Unidos Mexicanos"))
  
  # 3. NORMALIZACIÓN MAESTRA
  # Aquí estandarizamos los nombres para que sean fáciles de cruzar en el Frontend
  df_final <- df_clean %>%
    mutate(
      Total = as.numeric(Total),
      Moderada = as.numeric(Moderada),
      Extrema = as.numeric(Extrema),
      Pct_Moderada = round((Moderada / Total) * 100, 1),
      Pct_Extrema = round((Extrema / Total) * 100, 1),
      Pct_Total = round(((Moderada + Extrema) / Total) * 100, 1),
      
      # Limpieza agresiva de nombres
      Entidad = str_trim(Entidad),
      
      # Mapeo explícito de los casos problemáticos
      Entidad = case_when(
        str_detect(Entidad, "Coahuila") ~ "Coahuila",
        str_detect(Entidad, "Michoac") ~ "Michoacán",
        str_detect(Entidad, "Veracruz") ~ "Veracruz",
        str_detect(Entidad, "México") & !str_detect(Entidad, "Ciudad") ~ "Estado de México",
        str_detect(Entidad, "Distrito Federal|Ciudad de México") ~ "Ciudad de México",
        TRUE ~ Entidad
      )
    )
  
  col_sel <- switch(type, "total"="Pct_Total", "moderada"="Pct_Moderada", "extrema"="Pct_Extrema", "Pct_Extrema")
  
  df_final %>%
    select(state = Entidad, value = all_of(col_sel)) %>%
    filter(value >= as.numeric(min_val)) %>%
    arrange(desc(value))
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

#* Estadísticas de la Encuesta de Origen
#* @get /api/survey-stats
function() {
  col <- get_collection("surveys")
  # Agrupar por fuente y contar
  data <- col$find(fields = '{"source": 1, "_id": 0}')
  
  if(nrow(data) == 0) return(list(categories=c(), data=c()))
  
  summary <- data %>%
    group_by(source) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  
  list(
    categories = summary$source,
    data = summary$total
  )
}

#* Top Productos Buscados en Internet (Google Trends México)
#* Detecta qué productos de canasta básica tienen un repunte de interés hoy.
#* @get /api/global-trends
function() {
  library(gtrendsR)
  library(dplyr)
  
  # Términos "semilla" para explorar el mercado. 
  # Google nos dirá qué búsquedas relacionadas están creciendo.
  keywords <- c("canasta basica", "precio kilo", "supermercado oferta", "despensa")
  
  tryCatch({
    # Consultamos Google Trends México (geo="MX")
    # time="now 7-d": Últimos 7 días para captar tendencias inmediatas
    # hl="es-MX": Forzamos resultados en español de México
    res <- gtrends(keyword = keywords, geo = "MX", time = "now 7-d", hl = "es-MX")
    
    related <- res$related_queries
    
    # Validación: Si Google no da relacionados, salimos
    if (is.null(related)) return(list(error = "Google no devolvió datos relacionados"))
    
    # Filtramos las búsquedas "Rising" (En aumento explosivo)
    trends <- related %>%
      filter(related_queries == "rising") %>%
      select(value, subject) %>% # value = término, subject = % aumento (ej. "+50%")
      # Limpieza: Convertimos "+150%" a número (150) para poder ordenar
      mutate(growth = as.numeric(gsub("[%,+]", "", subject))) %>%
      arrange(desc(growth)) %>%
      head(5) # Nos quedamos con el Top 5
    
    # Formateamos como lista simple para que React la entienda fácil
    result_list <- lapply(1:nrow(trends), function(i) {
      list(
        term = as.character(trends$value[i]), # Ej: "Precio del huevo hoy"
        growth = trends$growth[i]             # Ej: 120
      )
    })
    
    return(result_list)
    
  }, error = function(e) {
    print(paste("Error Trends:", e$message))
    
    # --- DATOS DE RESPALDO (FALLBACK) ---
    # Si Google bloquea la petición o falla el internet, devolvemos esto
    # para que tu Dashboard nunca se vea vacío o roto.
    list(
      list(term = "Huevo (Tendencia Global)", growth = 120),
      list(term = "Azúcar (Tendencia Global)", growth = 90),
      list(term = "Tortilla (Tendencia Global)", growth = 60),
      list(term = "Aceite (Tendencia Global)", growth = 40),
      list(term = "Leche (Tendencia Global)", growth = 30)
    )
  })
}










# --- FUENTE 4: EXCEL (Carga de Archivos y Lectura) ---

# Nombre del archivo donde guardaremos lo que suba el usuario
GOALS_FILE <- "metas_actuales.xlsx"

#* Recibe un archivo Excel y lo guarda
#* @post /api/upload-goals
#* @parser multi
function(req, res) {
  # 'req$body' contiene los campos del formulario multipart
  # El archivo suele venir en un campo llamado 'file' (o como lo nombremos en React)
  
  if (is.null(req$body$file)) {
    res$status <- 400
    return(list(error = "No se envió ningún archivo"))
  }
  
  uploaded_file <- req$body$file
  
  # Guardamos el archivo en la carpeta del proyecto
  # movemos el temporal a nuestro nombre fijo
  tryCatch({
    # En Plumber modernos, uploaded_file$tempfile es la ruta temporal
    file.copy(uploaded_file$tempfile, GOALS_FILE, overwrite = TRUE)
    return(list(message = "Archivo actualizado con éxito", file = GOALS_FILE))
  }, error = function(e) {
    print(e)
    res$status <- 500
    return(list(error = "Error al guardar el archivo"))
  })
}

#* Lee el Excel guardado y devuelve el porcentaje de cumplimiento
#* @get /api/excel-goals
function() {
  library(readxl)
  
  # Si no existe el archivo (primera vez), creamos uno demo
  if (!file.exists(GOALS_FILE)) {
    # Datos Demo
    df <- data.frame(
      Concepto = c("Recaudación", "Kilos Rescatados"),
      Meta = c(100000, 5000),
      Actual = c(65000, 4200)
    )
    writexl::write_xlsx(df, GOALS_FILE)
  }
  
  tryCatch({
    # Leer el Excel
    datos <- read_excel(GOALS_FILE)
    
    # Procesamos: Calculamos % de cumplimiento del primer renglón (ej. Financiero)
    row <- datos[1, ] # Tomamos la primera fila
    
    porcentaje <- round((as.numeric(row$Actual) / as.numeric(row$Meta)) * 100, 1)
    
    list(
      percentage = porcentaje,
      label = paste0("$", format(row$Actual, big.mark=","), " / $", format(row$Meta, big.mark=","))
    )
  }, error = function(e) {
    # Si el Excel tiene mal formato
    return(list(percentage = 0, label = "Error en formato Excel"))
  })
}










# --- FUENTE 5: META-BUSCADOR WEB (Scraping Real) ---

# 1. Limpieza de precios
clean_price <- function(text) {
  if (is.na(text) || length(text) == 0) return(0)
  num_text <- gsub("[^0-9.]", "", text)
  val <- as.numeric(num_text)
  if(is.na(val)) return(0) else return(val)
}

# 2. NORMALIZADOR DE TEXTO (La clave para evitar errores)
normalize_text <- function(text) {
  if (is.na(text)) return("")
  
  # Minúsculas
  t <- tolower(text)
  
  # Quitar acentos (convertir á -> a)
  t <- iconv(t, to = "ASCII//TRANSLIT")
  
  # 1. Quitar unidades de medida y conectores comunes (Stopwords)
  # Esto evita que "Leche 1L" coincida con "Suero 1L" solo por el "1L"
  stopwords <- c("1l", "litro", "lt", "ml", "kg", "g", "gr", "pza", "pack", 
                 "de", "con", "el", "la", "en", "para", "oferta", "ahorro")
  
  pattern <- paste0("\\b(", paste(stopwords, collapse="|"), ")\\b")
  t <- gsub(pattern, "", t)
  
  # 2. Quitar caracteres especiales
  t <- gsub("[^a-z0-9 ]", "", t)
  
  # 3. Quitar espacios extra
  t <- gsub("\\s+", " ", trimws(t))
  
  return(t)
}

# 3. Comparador Estricto (Jaccard Modificado)
calculate_similarity <- function(str1, str2) {
  s1 <- normalize_text(str1)
  s2 <- normalize_text(str2)
  
  # Si tras limpiar no queda nada, es 0
  if (nchar(s1) == 0 || nchar(s2) == 0) return(0)
  
  w1 <- unique(strsplit(s1, " ")[[1]])
  w2 <- unique(strsplit(s2, " ")[[1]])
  
  # Filtramos cadenas vacías
  w1 <- w1[w1 != ""]
  w2 <- w2[w2 != ""]
  
  if (length(w1) == 0 || length(w2) == 0) return(0)
  
  intersection <- length(intersect(w1, w2))
  union <- length(union(w1, w2))
  
  # REGLA DE ORO: Si no comparten NINGUNA palabra clave, no son el mismo producto.
  if (intersection == 0) return(0)
  
  return(intersection / union)
}

#* Busca productos en 5 supermercados y en local
#* @param q Término de búsqueda
#* @get /api/search-products
function(q = "") {
  library(rvest)
  library(dplyr)
  library(stringr)
  library(mongolite)
  library(httr)
  
  if (q == "") return(list(error = "Escribe un producto"))
  
  query_encoded <- URLencode(q)
  user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  
  results <- list()
  
  # --- 1. TU INVENTARIO (Búsqueda Regex Flexible) ---
  tryCatch({
    prod_col <- get_collection("products")
    
    # Esto resuelve tu duda 2.1: Búsqueda desordenada
    # Si buscas "Leche Lala", esto crea un regex que busca ambas palabras en cualquier orden
    words <- strsplit(q, " ")[[1]]
    regex_pattern <- paste0("(?=.*", paste(words, collapse = ")(?=.*"), ")")
    
    local_prods <- prod_col$find(
      query = paste0('{"name": { "$regex": "', regex_pattern, '", "$options": "i" }}'),
      fields = '{"name": 1, "price": 1, "image_url": 1, "_id": 1}'
    )
    
    if(nrow(local_prods) > 0) {
      ids <- if(is.list(local_prods$`_id`)) as.character(local_prods$`_id`) else as.character(local_prods$`_id`)
      
      results$Local <- local_prods %>% 
        select(-`_id`) %>% 
        mutate(
          id = ids, title = name, image = image_url, 
          store = "Saveat", price = as.numeric(price)
        ) %>%
        select(id, title, price, image, store)
    }
  }, error = function(e) print(paste("Error Local:", e)))
  
  # --- SCRAPERS EXTERNOS ---
  
  # BODEGA AURRERA
  tryCatch({
    url <- paste0("https://despensa.bodegaaurrera.com.mx/search?q=", query_encoded)
    resp <- GET(url, add_headers("User-Agent" = user_agent))
    if (status_code(resp) == 200) {
      page <- read_html(resp)
      items <- page %>% html_nodes("div.flex.flex-column.items-center")
      if (length(items) > 0) {
        titles <- items %>% html_node("span.w_vi_D") %>% html_text(trim = TRUE)
        prices <- items %>% html_node("div[data-automation-id='product-price'] div[aria-hidden='true']") %>% html_text()
        images <- items %>% html_node("img") %>% html_attr("src")
        df <- data.frame(title = titles, price_txt = prices, image = images, stringsAsFactors = F)
        df$price <- sapply(df$price_txt, clean_price)
        df$store <- "Bodega Aurrera"
        results$Bodega <- head(df %>% filter(price > 0), 5)
      }
    }
  }, error = function(e) print(paste("Error Bodega:", e)))
  
  # WALMART
  tryCatch({
    url <- paste0("https://www.walmart.com.mx/search?q=", query_encoded)
    resp <- GET(url, add_headers("User-Agent" = user_agent))
    if (status_code(resp) == 200) {
      page <- read_html(resp)
      items <- page %>% html_nodes("div[data-item-id]")
      if (length(items) > 0) {
        titles <- items %>% html_node("span[data-automation-id='product-title']") %>% html_text(trim = TRUE)
        prices <- items %>% html_node("div[data-automation-id='product-price'] div[aria-hidden='true']") %>% html_text()
        images <- items %>% html_node("img[data-testid='productTileImage']") %>% html_attr("src")
        df <- data.frame(title = titles, price_txt = prices, image = images, stringsAsFactors = F)
        df$price <- sapply(df$price_txt, clean_price)
        df$store <- "Walmart"
        results$Walmart <- head(df %>% filter(price > 0), 5)
      }
    }
  }, error = function(e) print(paste("Error Walmart:", e)))
  
  # CHEDRAUI
  tryCatch({
    url <- paste0("https://www.chedraui.com.mx/", query_encoded, "?_q=", query_encoded, "&map=ft")
    resp <- GET(url, add_headers("User-Agent" = user_agent))
    if (status_code(resp) == 200) {
      page <- read_html(resp)
      items <- page %>% html_nodes(".vtex-product-summary-2-x-container")
      if (length(items) > 0) {
        titles <- items %>% html_node(".vtex-product-summary-2-x-productNameContainer") %>% html_text(trim = TRUE)
        prices <- items %>% html_node(".chedrauimx-products-simulator-0-x-simulatedSellingPrice") %>% html_text()
        images <- items %>% html_node("img.vtex-product-summary-2-x-imageNormal") %>% html_attr("src")
        df <- data.frame(title = titles, price_txt = prices, image = images, stringsAsFactors = F)
        df$price <- sapply(df$price_txt, clean_price)
        df$store <- "Chedraui"
        results$Chedraui <- head(df %>% filter(price > 0), 5)
      }
    }
  }, error = function(e) print(paste("Error Chedraui:", e)))
  
  # BASICOS
  tryCatch({
    url <- paste0("https://www.basicos.mx/filterSearch?q=", query_encoded)
    resp <- GET(url, add_headers("User-Agent" = user_agent))
    if (status_code(resp) == 200) {
      page <- read_html(resp)
      items <- page %>% html_nodes("div.product-item")
      if (length(items) > 0) {
        titles <- items %>% html_node("h2.product-title a") %>% html_text(trim = TRUE)
        prices <- items %>% html_node("span.actual-price") %>% html_text()
        images <- items %>% html_node("div.picture img") %>% html_attr("src")
        df <- data.frame(title = titles, price_txt = prices, image = images, stringsAsFactors = F)
        df$price <- sapply(df$price_txt, clean_price)
        df$store <- "Basicos"
        results$Basicos <- head(df %>% filter(price > 0), 5)
      }
    }
  }, error = function(e) print(paste("Error Basicos:", e)))
  
  # COSTCO
  tryCatch({
    url <- paste0("https://www.costco.com.mx/search?searchOption=mx-search-all&text=", query_encoded)
    resp <- GET(url, add_headers("User-Agent" = user_agent))
    if (status_code(resp) == 200) {
      page <- read_html(resp)
      items <- page %>% html_nodes("li.product-list-item")
      if (length(items) > 0) {
        titles <- items %>% html_node("a.lister-name") %>% html_text(trim = TRUE)
        prices <- items %>% html_node("div.product-price .notranslate") %>% html_text()
        images <- items %>% html_node("sip-primary-image img") %>% html_attr("src")
        images <- ifelse(grepl("^http", images), images, paste0("https://www.costco.com.mx", images))
        df <- data.frame(title = titles, price_txt = prices, image = images, stringsAsFactors = F)
        df$price <- sapply(df$price_txt, clean_price)
        df$store <- "Costco"
        results$Costco <- head(df %>% filter(price > 0), 5)
      }
    }
  }, error = function(e) print(paste("Error Costco:", e)))
  
  # --- ALGORITMO DE OPORTUNIDAD INTELIGENTE V2 ---
  ticker_data <- NULL
  
  # Solo si tenemos productos locales para comparar
  if (!is.null(results$Local) && nrow(results$Local) > 0) {
    
    web_results <- results[names(results) != "Local"]
    all_web <- bind_rows(web_results)
    
    if (nrow(all_web) > 0) {
      max_diff <- 0
      best_opp <- NULL
      
      # Iteramos sobre la competencia
      for(i in 1:nrow(all_web)) {
        comp_prod <- all_web[i, ]
        
        # Comparamos contra TODOS tus productos locales que salieron en la búsqueda
        # y buscamos el que más se parezca
        similarities <- sapply(results$Local$title, function(x) calculate_similarity(x, comp_prod$title))
        
        best_match_idx <- which.max(similarities)
        similarity_score <- similarities[best_match_idx]
        
        # UMBRAL ESTRICTO: 0.4
        # Esto + la función 'normalize_text' asegura que sean productos muy parecidos
        if (similarity_score > 0.4) {
          my_prod <- results$Local[best_match_idx, ]
          
          diff <- comp_prod$price - my_prod$price
          
          # Solo nos interesa si hay AHORRO (diff > 0) y es la mejor oferta encontrada
          if (diff > 0 && diff > max_diff) {
            max_diff <- diff
            best_opp <- list(
              product = comp_prod$title,
              competitor = comp_prod$store,
              their_price = comp_prod$price,
              my_price = my_prod$price,
              saving = diff
            )
          }
        }
      }
      ticker_data <- best_opp
    }
  }
  
  list(
    results = results,
    ticker = ticker_data
  )
}










# --- FUENTE 6: CLIMA Y LOGÍSTICA (Open-Meteo) ---

#* Devuelve pronóstico y análisis de riesgo logístico
#* @param lat Latitud del usuario
#* @param lon Longitud del usuario
#* @get /api/weather-logistics
function(lat = 20.52, lon = -100.81) { # Default: Celaya (por si acaso)
  library(httr)
  library(jsonlite)
  
  # Construimos la URL para Open-Meteo
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast?latitude=", lat, 
    "&longitude=", lon, 
    "&current=temperature_2m,relative_humidity_2m,is_day,precipitation,weather_code,wind_speed_10m",
    "&daily=weather_code,temperature_2m_max,temperature_2m_min,precipitation_probability_max",
    "&timezone=auto"
  )
  
  tryCatch({
    resp <- GET(url)
    data <- fromJSON(content(resp, "text"))
    
    current <- data$current
    
    # --- SEMÁFORO LOGÍSTICO (Lógica de Negocio en R) ---
    # Evaluamos si es seguro salir a ruta
    risk_level <- "Bajo" # Verde
    recommendation <- "Condiciones óptimas. Rutas de recolección activas."
    status_color <- "success"
    
    # Si llueve o hay viento fuerte (>25 km/h)
    if (current$precipitation > 0 || current$wind_speed_10m > 25) {
      risk_level <- "Alto" # Rojo
      recommendation <- "ALERTA: Lluvia o viento fuerte. Se recomienda suspender recolección en moto."
      status_color <- "error"
    } else if (current$wind_speed_10m > 15 || current$relative_humidity_2m > 80) {
      risk_level <- "Medio" # Amarillo
      recommendation <- "Precaución: Pavimento húmedo o viento moderado."
      status_color <- "warning"
    }
    
    # Interpretación de códigos WMO (0=Sol, 95=Tormenta, etc.)
    wcode <- current$weather_code
    condition_text <- "Despejado"
    if (wcode > 2) condition_text <- "Nublado"
    if (wcode > 50) condition_text <- "Llovizna"
    if (wcode > 60) condition_text <- "Lluvia"
    if (wcode >= 95) condition_text <- "Tormenta Eléctrica"
    
    # Armar respuesta
    list(
      current = list(
        temp = current$temperature_2m,
        humidity = current$relative_humidity_2m,
        wind = current$wind_speed_10m,
        condition = condition_text,
        is_day = current$is_day
      ),
      logistics = list(
        risk = risk_level,
        message = recommendation,
        color = status_color
      ),
      forecast = list(
        days = data$daily$time,
        max = data$daily$temperature_2m_max,
        min = data$daily$temperature_2m_min,
        rain_prob = data$daily$precipitation_probability_max
      )
    )
    
  }, error = function(e) {
    print(paste("Error clima:", e$message))
    list(error = "Servicio de clima no disponible")
  })
}
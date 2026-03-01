#paquetes usados
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
#importar datos a R
datos <- read_excel("url_donde_tienes_tus_datos.xlsx")

#Verificar que los datos se hayan cargado correctamente (con head(), str() o View()).

head(datos)
str(datos)
View(datos)

dataset <- head(datos, 5)
resumen <- summary(datos)

#----------------------------------------------------------------------------------------------
#Calcular Medidas de tendencia central como Media, Mediana, Moda, Varianza, Desviación estandar
#----------------------------------------------------------------------------------------------

calcular_media <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]  # eliminar valores NA
  }
  
  if (length(x) == 0) {
    warning("No hay valores válidos después de remover NA")
    return(NA)
  }
  
  suma <- sum(x)
  n <- length(x)
  media <- suma / n
  
  return(media)
}

calcular_mediana <- function(x, na.rm = TRUE) {
  
  # Validar entrada
  if (!is.numeric(x) && !is.logical(x)) {
    stop("La entrada debe ser numérica o lógica")
  }
  
  # Manejar valores NA !importante para la columna Aprobad2
  if (na.rm) {
    x <- x[!is.na(x)]
  } else if (any(is.na(x))) {
    return(NA)
  }
  
  # Verificar si hay datos después de remover NA
  if (length(x) == 0) {
    warning("No hay datos válidos después de remover NA")
    return(NA)
  }
  
  # Ordenar los datos
  x_ordenado <- sort(x)
  n <- length(x_ordenado)
  
  # Calcular mediana según si n es par o impar
  if (n %% 2 == 1) {
    # n impar: mediana es el valor central
    return(x_ordenado[(n + 1) / 2])
  } else {
    # n par: mediana es el promedio de los dos valores centrales
    medio1 <- x_ordenado[n / 2]
    medio2 <- x_ordenado[(n / 2) + 1]
    return((medio1 + medio2) / 2)
  }
}

calcular_moda <- function(x) {
  # Eliminar valores NA 
  x <- na.omit(x)
  
  if (length(x) == 0) {
    return(NA)
  }
  
  # Tabla de frecuencias
  frecuencias <- table(x)
  
  # Encontrar valores con máxima frecuencia
  modas <- as.numeric(names(frecuencias)[frecuencias == max(frecuencias)])
  
  # Si hay múltiples modas, retornar todas
  if (length(modas) > 1) {
    return(paste(modas, collapse = ", "))
  } else {
    return(modas)
  }
}

calcular_varianza <- function(x, poblacional = FALSE) {
  # Eliminar valores NA !importante para la Columna Aprobad2
  x <- na.omit(x)
  n <- length(x)
  
  if (n < 2) {
    warning("Se necesitan al menos 2 observaciones para calcular varianza")
    return(NA)
  }
  
  # Calcular media
  media_x <- mean(x)
  
  # Calcular suma de cuadrados de las diferencias
  suma_cuadrados <- sum((x - media_x)^2)
  
  # Elegir divisor según sea muestral o poblacional
  if (poblacional) {
    varianza <- suma_cuadrados / n
  } else {
    varianza <- suma_cuadrados / (n - 1)
  }
  
  return(varianza)
}
desviacion_estandar <- function(x, poblacional = FALSE) {
  # Remover valores NA !importante para la columna Aprobad2
  x <- na.omit(x)
  
  # Verificar que hay suficientes datos
  if (length(x) < 2) {
    warning("Se necesitan al menos 2 observaciones para calcular la desviación estándar")
    return(NA)
  }
  
  # Calcular media
  media_x <- mean(x)
  
  # Calcular suma de diferencias al cuadrado
  suma_cuadrados <- sum((x - media_x)^2)
  
  # Calcular varianza (muestral o poblacional)
  if (poblacional) {
    varianza <- suma_cuadrados / length(x)
  } else {
    varianza <- suma_cuadrados / (length(x) - 1)
  }
  
  # Calcular desviación estándar
  desv_est <- sqrt(varianza)
  
  return(desv_est)
}
graficar_estadisticas <- function(x, nombre = "Variable") {
  # Validar y limpiar datos
  x_limpio <- na.omit(x)
  n <- length(x_limpio)
  
  if (n < 2) {
    cat("Datos insuficientes para la variable:", nombre, "\n")
    return(NULL)
  }
  
  #  Calcular medidas (Usando funciones nativas para asegurar estabilidad)
  media_val <- calcular_media(x_limpio)
  mediana_val <- calcular_mediana(x_limpio)
  sd_val <- desviacion_estandar(x_limpio)
  
  resultados <- list(
    variable = nombre,
    n = n,
    media = media_val,
    mediana = mediana_val,
    desv_est = sd_val,
    varianza = var(x_limpio),
    minimo = min(x_limpio),
    maximo = max(x_limpio),
    rango = max(x_limpio) - min(x_limpio),
    coef_variacion = (sd_val / media_val) * 100
  )
  
  # Crear el gráfico con ggplot2
  df_plot <- data.frame(valor = x_limpio)
  
  p <- ggplot(df_plot, aes(x = valor)) +
    # Histograma de fondo
    geom_histogram(aes(y = ..density..), bins = 15, fill = "steelblue", color = "white", alpha = 0.7) +
    # Línea de la Media
    geom_vline(aes(xintercept = media_val, color = "Media"), linetype = "dashed", size = 1) +
    # Línea de la Mediana
    geom_vline(aes(xintercept = mediana_val, color = "Mediana"), linetype = "dotted", size = 1) +
    # Etiquetas y Estética
    scale_color_manual(name = "Estadísticas", values = c(Media = "red", Mediana = "green")) +
    labs(title = paste("Distribución de:", nombre),
         subtitle = paste("n =", n, "| DS =", round(sd_val, 2)),
         x = "Valores") +
    theme_minimal()
  
  # Imprimir el gráfico
  print(p)
  
  return(resultados)
}

graficar_estadisticas(datos$Aprobad2)

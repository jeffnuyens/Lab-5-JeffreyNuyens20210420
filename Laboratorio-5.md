Laboratorio 5
================
Jeff
2023-10-01

# Parte 1: Predecir un eclipse solar

``` r
# Fecha del eclipse histórico
fecha_historica <- as.POSIXct("2017-08-21 18:26:40", format="%Y-%m-%d %H:%M:%S")

# Duración de un Saros en Synodic Months
saros_duracion_synodic <- 223

# Duración de un Synodic Month en segundos
synodic_month_segundos <- 29 * 24 * 3600 + 12 * 3600 + 44 * 60 + 3

# Calcular la fecha del próximo eclipse solar
fecha_siguiente_eclipse <- fecha_historica + saros_duracion_synodic * synodic_month_segundos

print(fecha_siguiente_eclipse)
```

    ## [1] "2035-09-02 02:09:49 CST"

# Parte 3: Signo Zodiacal

``` r
calcular_signo_zodiacal <- function(fecha_de_nacimiento) {
  # Dividir la fecha en día, mes y año
  partes_fecha <- unlist(strsplit(fecha_de_nacimiento, "-"))
  dia <- as.integer(partes_fecha[3])
  mes <- as.integer(partes_fecha[2])
  
  # Definir las fechas de inicio y fin de cada signo zodiacal
  fechas_inicio <- c(3, 4, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  fechas_fin <- c(4, 5, 6, 7, 8, 9, 10, 11, 12, 12, 12, 1)
  
  # Definir los nombres de los signos zodiacales
  signos <- c(
    "Aries", "Tauro", "Géminis", "Cáncer", "Leo", "Virgo",
    "Libra", "Escorpio", "Sagitario", "Capricornio", "Acuario", "Piscis"
  )
  
  # Determinar el signo zodiacal
  signo <- ifelse((mes == fechas_inicio & dia >= 21) | (mes == fechas_fin & dia <= 20), signos, NA)
  return(na.omit(signo))
}

# Ejemplo de uso
mi_fecha_de_nacimiento <- "2002-01-14"
mi_signo <- calcular_signo_zodiacal(mi_fecha_de_nacimiento)
cat("Mi signo zodiacal es:", mi_signo)
```

    ## Mi signo zodiacal es: Piscis

# Parte 4: Flights

``` r
library(nycflights13)
```

    ## Warning: package 'nycflights13' was built under R version 4.2.3

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
vuelos <- flights

# Crear nuevas columnas para fechas y horas
vuelos <- vuelos %>% 
  mutate(
    # Convertir año, mes y día en una fecha
    departure = make_date(year, month, day),
    
    # Crear una fecha y hora programada a partir de los datos disponibles
    sched_departure_time = make_datetime(year, month, day, hour, minute),
    
    # Extraer horas y minutos de las variables numéricas
    arr_time_hora = arr_time %/% 100,
    arr_time_min = arr_time %% 100,
    dep_time_hora = dep_time %/% 100,
    dep_time_min = dep_time %% 100,
    sched_arr_time_hora = sched_arr_time %/% 100,
    sched_arr_time_min = sched_arr_time %% 100,
    sched_dep_time_hora = sched_dep_time %/% 100,
    sched_dep_time_min = sched_dep_time %% 100,
    
    # Crear nuevas columnas con fechas y horas completas
    arrival_time = make_datetime(year, month, day, arr_time_hora, arr_time_min),
    departure_time = make_datetime(year, month, day, dep_time_hora, dep_time_min),
    sched_arrival_time = make_datetime(year, month, day, sched_arr_time_hora, sched_arr_time_min)
  )

# Calcular el delay total que existe en cada vuelo
vuelos <- vuelos %>% 
  mutate(
    delay_total = dep_delay + arr_delay
  )

# Visualizar los datos de vuelos con las nuevas columnas
View(vuelos)
```

1.  Las nuevas columnas son departure, sched_departure_time,
    arrival_time, y sched_arrival_time. Cada una de estas columnas
    representa una fecha y hora en diferentes momentos del vuelo.

2.  El delay total se calcula mediante la columna delay_total.

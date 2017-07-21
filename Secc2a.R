library(chron) # Manipulacion de fechas y horas
library(jsonlite) # Leer JSON


############### Lectura de datos #########

april <- read.csv("2017-04.csv")
may <- read.csv("2017-05.csv")
june <- read.csv("2017-06.csv")

data <- rbind(april, may, june)

data <- transform(data, 
                  Fecha_Retiro = chron(dates = as.character(Fecha_Retiro), format = c('d/m/y')), 
                  Hora_Retiro =  chron(times = as.character(Hora_Retiro), format = c('h:m:s')),
                  Fecha_Arribo = chron(dates = as.character(Fecha_Arribo), format = c('d/m/y')), 
                  Hora_Arribo =  chron(times = as.character(Hora_Arribo), format = c('h:m:s'))
        )
                  

rm( list = c("april", "may", "june") )
##########################################


############### Frecuencia de uso [Retiro y Arribo] por hora y estacion ###############

  # Frecuencia de uso por horarios [0-23]
h <- times(seq( from = 0, to = 1.0, by = 1.0 / 24 ))
retiro <- hist(data$Hora_Retiro, breaks = h, plot = FALSE)
arribo <- hist(data$Hora_Arribo, breaks = h, plot = FALSE)
barplot(retiro$counts+arribo$counts , names.arg = seq(0,23), main = "Retiros y Arribos por hora")

  # Frecuencia de uso por estacion
est <- as.numeric( levels( factor(c(data$Ciclo_Estacion_Retiro, data$Ciclo_Estacion_Arribo) )  ) )

h <- seq(min(est)-1, max(est))
retiro <- hist( data$Ciclo_Estacion_Retiro, breaks = h, plot = FALSE  )
arribo <- hist( data$Ciclo_Estacion_Arribo, breaks = h, plot = FALSE  )

counts <- data.frame(retiro$counts[est] + arribo$counts[est], est)
names(counts) <- c("counts", "est")
counts <- counts[order(-counts$counts),] # Ordenamos por frecuencia de uso
barplot(  counts$counts  , names.arg = counts$est, main =  "Retiros y Arribos por Estacion")

        # 10 Estaciones con mayor afluencia
barplot(  counts$counts[1:10]  , names.arg = counts$est[1:10], main =  "Estaciones con mayor Afluencia")
        # 10 Estaciones con menor afluencia
barplot(  counts$counts[451:440]  , names.arg = counts$est[451:440], main =  "Estaciones con menor Afluencia")

#rm()
#####################################################################


################### Frecuencia de uso en el tiempo por estacion ####################
est <- as.numeric( levels( factor(c(data$Ciclo_Estacion_Retiro, data$Ciclo_Estacion_Arribo) )  ) ) 
weeks <- seq( from = as.numeric(dates("03/04/17", format = c("d/m/y")))-1, to = as.numeric(dates("25/06/17", format = c("d/m/y"))), by = 7 )

x <- data.frame(1:12) # Numero de semana
names(x) <- c("x")

freq_rel_t <- data.frame(matrix(0, length(est), 12))
slope <- matrix(0, nrow = length(est), ncol = 1 )

k <- 1
for(  e in est ){
  h <- hist( data[data$Ciclo_Estacion_Retiro == e & data$Fecha_Retiro > weeks[1] & data$Fecha_Retiro <= weeks[13], "Fecha_Retiro"], breaks =  weeks, plot = FALSE )
  y <- h$density
  freq_rel_t[k,] <- h$density
  linear <- lm( y ~ x, data = x )
  slope[k] <- linear$coefficients[2] 
#  plot( x$x, h$density, type = 'l', col = e, ylim = c(0,0.05), main = paste("Est:", e, "m =", slope[k]) )
#  lines(x$x, predict(linear,x))
#  points(x$x,h$density, col = e)
  k <- k+1
}

freq_rel_t <- t(freq_rel_t)
names(freq_rel_t) <- as.character(est)

indx <- which( slope > 3e-4 ) # 194 estaciones con tendencia a la alta
matplot( x, freq_rel_t[ , indx[1:194] ], type = "l", ylim = c(0,0.04), xlab = "Semana", ylab = "Frecuencia de uso normalizada", main = "Estaciones con tendencia de uso a la alta" )

indx <- which( slope < -3e-4 )
matplot( x, freq_rel_t[ , indx ], type = "l", ylim = c(0,0.04), xlab = "Semana", ylab = "Frecuencia de uso normalizada", main = "Estaciones con tendencia de uso a la baja" )

indx <- which( slope >= -3e-4 & slope <= 3e-4 )
matplot( x, freq_rel_t[ , indx[1:251] ], type = "l", ylim = c(0,0.04), xlab = "Semana", ylab = "Frecuencia de uso normalizada", main = "Estaciones con uso sin tendencia de crecimiento/decrecimiento" )

indx <- which( est == 1001 | est == 1002 )
matplot( x, freq_rel_t[ , indx ], type = "l", ylim = c(0,0.15), xlab = "Semana", ylab = "Frecuencia de uso normalizada", main = "Estaciones de uso esporadico" )

#######################################################################################


################################### Analisis de Entradas y Salidas ############################
est <- as.numeric( levels( factor(c(data$Ciclo_Estacion_Retiro, data$Ciclo_Estacion_Arribo) )  ) ) 
b <- seq(min(est)-1, max(est))

mtx_org_dest <- matrix(0, length(est), length(est) ) # filas retiros, columnas arribos

k <- 1
for( e in est ){ # para cada estacion de retiro
  h <- hist( data$Ciclo_Estacion_Arribo[data$Ciclo_Estacion_Retiro == e ], breaks = b, plot = FALSE ) # se cuenta la estacion de arribo
  mtx_org_dest[k, ] <- h$density[est]
  k <- k+1
}

mtx_org_dest <- apply(mtx_org_dest, 2, rev)
heatmap(mtx_org_dest, Rowv = NA, Colv = NA, col = colorRampPalette( c("gray", "red") )(512) )

#######################################################################################


################################ Agrupacion de estaciones ####################################
est <- as.numeric( levels( factor(c(data$Ciclo_Estacion_Retiro, data$Ciclo_Estacion_Arribo) )  ) ) 
mtx_uso_t <- data.frame(  matrix(0, length(est), 2)  )
names(mtx_uso_t) <- c("Retiro_Maniana", "Arribo_Maniana")

k <- 1
for( e in est ){
    temp <- data[data$Ciclo_Estacion_Retiro == e, "Hora_Retiro"]
    len <- length(temp)
    mtx_uso_t[k, 1] <- sum( temp >= times("5:0:0") & temp < times("12:0:0")  )  / len
    temp <- data[data$Ciclo_Estacion_Arribo == e, "Hora_Arribo"]
    len <- length(temp)
    mtx_uso_t[k, 2] <- sum( temp >= times("5:0:0") & temp < times("12:0:0")  )  / len
    k <- k+1
}

km <- kmeans(mtx_uso_t, centers = 4 )
plot( mtx_uso_t[,1], mtx_uso_t[,2], col = km$cluster, xlab = "No. Retiros antes de 12:00pm", ylab = "No. Arribos antes de 12:00pm" )
######################################################################################


############# Informacion de distancias entre estaciones ##################

dist_data <- read_json("estaciones.json")

dist_mtx <- matrix(0, length(est), length(est) )

e <- 452

for( i in seq(e-1) ){
    s1 <- dist_data$stations[[i]]$id
    ind1 <- which(est == s1)
    if( is.na(ind1[1]) ){
      next
    }
    lat1 <- dist_data$stations[[i]]$location$lat
    lon1 <- dist_data$stations[[i]]$location$lon
    rlat1 <- lat1 * pi / 180.0
    for(j in seq(i+1,e)){
        s2 <- dist_data$stations[[j]]$id
        ind2 <- which(est == s2)
        if( is.na(ind2[1]) ){
          next
        }
        lat2 <- dist_data$stations[[j]]$location$lat
        lon2 <- dist_data$stations[[j]]$location$lon
        # Se calcula distancia entre estaciones[ Haversine Formula ]
        rlat2 <- lat2 * pi / 180.0
        diff_lat <- (rlat2 - rlat1)  # en radianes
        diff_lon <- (lon2 - lon1) * pi / 180.0 # en radianes
        d <- sin( diff_lat / 2.0 )**2 + cos(  rlat1  ) * cos(  rlat2  ) * sin(  diff_lon / 2.0  )**2
        dist_mtx[ ind1, ind2 ] <- d
        dist_mtx[ ind2, ind1 ] <- d
    }
}

dist_mtx <- apply(dist_mtx, 2, rev)
heatmap(dist_mtx, Rowv = NA, Colv = NA, col = colorRampPalette( c("gray", "red") )(512) )

plot( as.vector(dist_mtx), as.vector(mtx_org_dest), ylim = c(0,0.4) )

###########################################################################

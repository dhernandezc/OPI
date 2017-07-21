library(foreign) # Carga archivos dbf

ent <- 9 # Distrito Federal [Para base de datos de natalidad] 
mun <- 10 # Alvaro Obregon

############# Datos de censo de poblacion y vivienda ###############

all_data <- read.csv( "resultados_ageb_urbana_09_cpv2010.csv" )

selected_variables <- c("pobmas","p_0a2", "prom_hnv","pob15_64")
rows_cpv <- which(all_data[, "mun"] == mun & all_data[, "mza"] == 0 & all_data[,"ageb"] != "0000") 

#nrows <- length(rows)
#ncols <- length(selected_variables)

# Filtrar y transformar datos
data <- all_data[ rows_cpv, selected_variables ]
    #sapply(data, class)
data <- transform(data, pobmas = as.numeric(levels(pobmas))[pobmas], p_0a2 = as.numeric(levels(p_0a2))[p_0a2], 
                  prom_hnv = as.numeric(levels(prom_hnv))[prom_hnv], pob15_64 = as.numeric(levels(pob15_64))[pob15_64])

rm( list = "all_data" )
###############################################################


################# Base de Datos de Natalidad 2010 ###############

  # Se calcula el numero de nacimientos en un periodo de 6 meses [Enero-Junio 2010]
    # en la delegacion Alvaro Obregon

nat_data <-  read.dbf("NACIM10.dbf")
births <- which(nat_data$ENT_RESID == ent & nat_data$MUN_RESID == mun & nat_data$MES_NAC <= 6)
number_births <- length( births )
rm( list = "nat_data" )

  # Se asigna el numero de nacimientos en base a la proporcion 
    # de la poblacion de ni??os de edad 0-2 a??os en cada AGEB [ver pdf justificacion]

y <- number_births * data$p_0a2 / sum( data$p_0a2 ) # Numero de bebes estimado en 2010 de 0-0.5 a??os

#########################################


############## Base de Datos de Natalidad 2015 ###########

# Se calcula el numero de nacimientos en un periodo de 6 meses [Enero-Junio 2010]
# en la delegacion Alvaro Obregon

nat_data <-  read.dbf("NACIM10.dbf")
births <- which(nat_data$ENT_RESID == ent & nat_data$MUN_RESID == mun & nat_data$MES_NAC <= 6)
number_births <- length( births )
rm( list = c("nat_data", "births") )

# Se asigna el numero de nacimientos en base a la proporcion 
# de la poblacion de ni??os de edad 0-2 a??os en cada AGEB [ver pdf justificacion]

y <- number_births * data$p_0a2 / sum( data$p_0a2 ) # Numero de bebes estimado en 2010 de 0-0.5 a??os

  # Para el a??o 2015

nat_data <-  read.dbf("NACIM15.dbf")
births <- which( nat_data$ENT_RESID == paste("0",ent, sep="") & nat_data$MUN_RESID == paste("0",mun, sep="")  & nat_data$MES_NACIM <= 6 )
number_births_2015 <- length( births )
rm( list = c("nat_data", "births") )

y_2015 <- number_births_2015 * data$p_0a2 / sum( data$p_0a2 ) # Numero de bebes estimado en 2010 de 0-0.5 años

#########################################


############################ MODELO ##############################

linear_model <- lm( y ~ pobmas:prom_hnv-1, data = data)
summary(linear_model)

plot( data$pobmas*data$prom_hnv, y )
s1 <- seq( from = 0, to = max(data$pobmas), by = max(data$pobmas) / 5 )
s2 <- seq( from = 0, to = max(data$prom_hnv), by = max(data$prom_hnv) / 5 )
ds <- data.frame( s1, s2 ); names(ds) <- c("pobmas", "prom_hnv")
lines(s1*s2, predict(linear_model,ds), col = "red" )

y_hat <- predict( linear_model, data = data )

MSE <- norm( as.matrix(y - y_hat) , type="F") / length(y)
print(MSE)
#################################################################


######################  TEST [AÑO 2015]  #######################

  #Se realizan proyecciones para el año 2015 de la poblacion masculina
      #y el promedio de hijos nacidos vivos 
      # en base a los censos de poblacion y vivienda en los años 2000,2005,2010
      # en la Delegacion Alvaro Obregon

pob_masc_t <- c(  327431 , 336625, 346041 ) # Datos obtenidos de censos
  demographic_growth_rate <- 2.8 / 100 # tasa de crecimiento de la poblacion en 5 años [De acuerdo a los datos arriba]
prom_hnv_t <- c( 2.1 , 1.98, 1.87 )
  fertitilty_rate <- 94 / 100 # disminucion de la tasa de fertilidad en 5 años [De acuerdo a los datos arrida]

  pob_masc_2015 <- (1 + demographic_growth_rate) * data[,"pobmas"]
  prom_hnv_2015 <- fertitilty_rate * data[,"prom_hnv"]
  
  data_2015 <- data.frame(pob_masc_2015, prom_hnv_2015 )
  names(data_2015) <- c("pobmas", "prom_hnv")
  
  y_hat_2015 <- predict(linear_model, data_2015)
  
  plot( y_2015, y_hat_2015 )
  lines(c(0,100), c(0,100), col = "red" )
    
  MSE_2015 <- norm( as.matrix(y_hat_2015 - y_2015) , type = "F") / length(y_2015)
  print(MSE_2015)
################################################################


############### ESTIMACION DE # BEBES EN 2017 ####################
  
  # Ajustamos tasa de crecimiento y fertilidad de 2010 a 2017
  demographic_growth_rate <- (7.0 / 5.0) * 2.8 / 100
  fertitilty_rate <- ( 1 - (7.0 / 5.0) * 6.0 / 100 )
  
  pob_masc_2017 <- (1 + demographic_growth_rate) * data[,"pobmas"]
  prom_hnv_2017 <- fertitilty_rate * data[,"prom_hnv"]
  
  data_2017 <- data.frame(pob_masc_2017, prom_hnv_2017 )
  names(data_2017) <- c("pobmas", "prom_hnv")
  
  y_hat_2017 <- predict(linear_model, data_2017)
  
  total_number_births <- sum(y_hat_2017)
###############################################################
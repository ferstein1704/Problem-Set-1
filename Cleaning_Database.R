
rm(list=ls()) # Clear memory

#Paquetes a utilizar

library(foreign)
library(tidyverse)
library(lubridate)
library(dplyr)

setwd("C:/Users/ferna/Dropbox/Maestria/Microeconometria Avanzada/Problem Set 1")

####################
# Leemos los files #
####################

poblacion <- read.csv(file = 'conjunto_de_datos_poblacion_enigh_2020_ns.csv')
vivienda <- read.csv(file = 'conjunto_de_datos_viviendas_enigh_2020_ns.csv')


#######################
# Limpiamos las bases #
#######################
# De Vivienda solamente necesitamos Ubicacion Geografica
vivienda_aux <- subset(vivienda, select = c("ï..folioviv", "ubica_geo"))

# Generamos ID de vivienda estandar
colnames(vivienda_aux)[1] <- "id_vivienda"
colnames(poblacion)[1] <- "id_vivienda"

# Juntamos los 2 dataframes por id
aux_df <- merge(poblacion, vivienda_aux, by = "id_vivienda")

# Del nuevo dataframe me quedo con las variables que me van a servir 
aux_df <- subset(aux_df, select = c("id_vivienda","foliohog","sexo","edad","ubica_geo","nivelaprob","gradoaprob","trabajo_mp"))

#############################
# Variable Decision to Work #
#############################
aux_df <- aux_df %>% mutate(D = ifelse(trabajo_mp == 1, 1, 0))

###########################
# Variable Dummy Regional #
###########################

# La variable "ubica_geo" nos dice que estado es, hacemos substring de los primeros 2 elementos
aux_df <- aux_df %>% mutate(estado = ifelse(nchar(ubica_geo) == 4 , substring(ubica_geo,1,1), substring(ubica_geo,1,2)))

# Generamos Dummy por region acorde al diccionario abajo (Source: https://www.gob.mx/cms/uploads/attachment/file/67641/CAP-08.pdf)

# Noroeste (6) - BC (2), BCS (3), Chihuahua (4), Durango (10), Sinaloa (25), Sonora (26)
# Noreste (3) - Coahuila (5), NL (19), Tamaulipas (28)
# Occidente (9) - Aguascalientes(1), Colima (6), Guanajuato (11), Jalisco (14), Michoacan (16), Nayarit(18), Queretaro(22), SLP (24), Zacatecas (32)
# Centro (6) - Hidalgo (13), CDMX (9) Edo Mex (15), Morelos (17), Puebla (21), Tlaxcala (29)
# Sur (8) - Campeche (4), Chiapas (7), Guerrero (12), Oaxaca (20), QRO (23), Tabasco (27), Veracruz (30), Yucatan (31)
 
noroeste <- c(2,3,4,10,25,26)
noreste <- c(5,19,28)
occidente <- c(1,6,11,14,16,18,22,24,32)
centro <- c(13,9,15,17,21,29)
sureste <- c(4,7,12,20,23,27,30,31)

aux_df <- aux_df %>% mutate(noroeste = ifelse(estado %in% noroeste,1,0),
                  sureste = ifelse(estado %in% sureste, 1, 0),
                  noreste = ifelse(estado %in% noreste, 1, 0),
                  occidente = ifelse(estado %in% occidente, 1, 0),
                  centro = ifelse(estado %in% centro, 1, 0))

# Genero Variable de id hogar auxiliar
aux_df["ID"] <- paste(aux_df$id_vivienda,aux_df$foliohog)
aux_df$ID <- str_replace_all(aux_df$ID," ","")

############################
# Variable Menores 12 años #  
############################
aux_df <- aux_df %>% mutate(niño = ifelse(edad<=12,1,0))
aux_df<- aux_df %>% group_by(ID) %>% mutate(n_menores = sum(niño == 1))

############################
# Variable Mayores 64 años #  
############################
aux_df <- aux_df %>% mutate(adulto = ifelse(edad>=64,1,0))
aux_df<- aux_df %>% group_by(ID) %>% mutate(n_mayores = sum(adulto == 1))

#################
# Mujeres 25-54 #
#################
# Aqui ya podemos quedarnos con las mujeres 25-54
aux_df <- aux_df %>% filter(sexo==2)
aux_df <- aux_df %>% filter(edad>=25 & edad<=54)

# Tiramos variables que ya no nos sirven
aux_df <- aux_df %>% select(-c(id_vivienda,foliohog,sexo,ubica_geo,trabajo_mp,estado,niño,adulto))

#Tiramos NA
aux_df <- na.omit(aux_df)

###############################
# Variable Years of Education #
###############################

# Variable Nivel Diccionario - Años de escolaridad

# 0	Ninguno
# 1	Preescolar
# 2	Primaria
# 3	Secundaria
# 4	Preparatoria o bachillerato
# 5	Normal
# 6	Carrera técnica o comercial
# 7	Profesional
# 8	Maestría
# 9	Doctorado


# Construimos Años de Escolaridad
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==0,0,NA))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==1,0,a_escolaridad))                      
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==2,gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==3,6+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==4,9+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==5,12+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==6,12+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==7,12+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==8,15+gradoaprob,a_escolaridad))
aux_df <- aux_df %>% mutate(a_escolaridad = ifelse(nivelaprob==9,17+gradoaprob,a_escolaridad))

#Tiramos NA y Unused vars
aux_df <- na.omit(aux_df)
aux_df <- aux_df %>% select(-c(gradoaprob,nivelaprob))

#Generamos D y Z

D <- aux_df$D
Z <- aux_df %>% ungroup()%>% select(-c(D,ID))


summary(Z)



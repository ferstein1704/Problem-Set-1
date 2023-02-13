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
ingresos <- read.csv(file = 'conjunto_de_datos_ingresos_enigh_2020_ns.csv')
#######################
# Limpiamos las bases #
#######################
# De Vivienda solamente necesitamos Ubicacion Geografica
vivienda_aux <- subset(vivienda, select = c("ï..folioviv", "ubica_geo"))

# Generamos ID de vivienda estandar
colnames(vivienda_aux)[1] <- "id_vivienda"
colnames(poblacion)[1] <- "id_vivienda"
colnames(ingresos)[1] <- "id_vivienda"

# Juntamos los 2 dataframes por id
aux_df <- merge(poblacion, vivienda_aux, by = "id_vivienda")

# Genero auxiliar ID por numero de residente
aux_df["ID"] <- paste(aux_df$id_vivienda,aux_df$foliohog,aux_df$numren)
ingresos["ID"] <- paste(ingresos$id_vivienda,ingresos$foliohog,ingresos$numren)

#De ingresos solamente me interesa ID y ing_tri
ingresos_aux <- subset(ingresos, select = c("ID", "ing_tri"))
ingresos_aux$ing_tri <- ingresos_aux$ing_tri/3

#De aux, solamente queremos trabajo, sexo, edad, ubica_geo
aux_df <- subset(aux_df, select = c("ID","sexo","edad","ubica_geo","nivelaprob","trabajo_mp"))
aux_df_2 <- merge(aux_df, ingresos_aux, by = "ID")

#############################
# Variables Filtro          #
#############################
aux_df_2 <- aux_df_2 %>% mutate(D = ifelse(trabajo_mp == 1, 1, 0))
aux_df_2 <- aux_df_2 %>% filter(edad>=25 & edad<=54)
aux_df_2 <- aux_df_2 %>% filter(D==1)
aux_df_2 <- aux_df_2 %>% filter(ing_tri>0)

###############################
# Variable Higher Eduaction   #
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

aux_df_2 <- aux_df_2 %>% mutate(higher_ed = ifelse(nivelaprob >= 4, 1, 0)) #Mas que prepa

###########################
# Variable Dummy Regional #
###########################

# La variable "ubica_geo" nos dice que estado es, hacemos substring de los primeros 2 elementos
aux_df_2 <- aux_df_2 %>% mutate(estado = ifelse(nchar(ubica_geo) == 4 , substring(ubica_geo,1,1), substring(ubica_geo,1,2)))

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

aux_df_2 <- aux_df_2 %>% mutate(noroeste = ifelse(estado %in% noroeste,1,0),
                            sureste = ifelse(estado %in% sureste, 1, 0),
                            noreste = ifelse(estado %in% noreste, 1, 0),
                            occidente = ifelse(estado %in% occidente, 1, 0),
                            centro = ifelse(estado %in% centro, 1, 0))
# Dummy for male
aux_df_2 <- aux_df_2 %>% mutate(male = ifelse(sexo == 1 ,1,0))


#Tiramos NA
aux_df_2 <- na.omit(aux_df_2)

#Generate vectors
y <- log(aux_df_2$ing_tri)
D <- aux_df_2$higher_ed                                
aux_df_2["age_sq"] <- aux_df_2$edad^2

X <- subset(aux_df_2, select = c("male","edad","age_sq","noroeste","noreste","sureste","centro","occidente"))

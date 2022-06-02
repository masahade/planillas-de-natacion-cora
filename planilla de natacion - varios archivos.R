library(pdftools)
library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(openxlsx)

file='data/evento 3  hombres 18-24 200 cc metro estilo libre federados.pdf'

txt <- pdf_text(file)


destino="cora.txt"

write(txt,destino,append=TRUE)

texto_completo=readLines(destino)

#---------------- selecciona de linea 1 lugar de la competencia 
linea1=texto_completo[1]

pattern='.[:space:]{20,}.*'

indices=str_locate(linea1, pattern) 
indices[1]

lugar_competencia=substr(linea1, 1, indices[1])

#------ selecciona la fecha de la competencia y nombre de la competencia

linea2=str_trim(texto_completo[2])

pattern="[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}"

fecha_competencia=as.Date(str_extract(linea2,pattern),"%d/%m/%Y")

nombre_competencia=linea2

#------ selecciona solo los registro de tiempos

pattern="^Evento"

vector_eventos=str_detect(texto_completo,pattern)

eventos=texto_completo[vector_eventos]

# ------------ creo los dataframe solo con los registros de timpos con separadores fijos


pattern1="([0-9]{1,2}:[0-9]{1,2}\\.[0-9]{2}).+([0-9]{1,2}:[0-9]{1,2}\\.[0-9]{2}) "

vector_tiempos=str_detect(texto_completo,pattern1)

tiempos=str_trim(texto_completo[vector_tiempos])


archivo=read_fwf(tiempos,
                 fwf_widths(c(2,28,7,35,20,20),c("posicion","nombre","edad","club",'tiempo1','tiempo2'))) 

archivo1=read_fwf(tiempos,
                 fwf_widths(c(2,30),c("posicion","nombre"))) 

posicion=substr(tiempos, 1, 2)
nombre=str_trim(substr(tiempos, 3, 30))

str_trim(substr(tiempos[1], 31, length(tiempos[1]))) 

# -------------- tiene que unir dataframes y limpiar registros y columnas basura

archivo3=read_fwf(tiempos,
                  fwf_widths(c(30,150),c("posicion","nombre"))) 


write.csv(archivo3$nombre ,"columna2.csv", row.names = FALSE)
columns2=readLines("columna2.csv")

archivo4=read_fwf(columns2,
                  fwf_widths(c(1,2,40,19,19,10),c("x","edad","club",'tiempo1','tiempo2','cate'))) 

archivo4 <- archivo4[ -c(1) ]
archivo4 <- archivo4[2:length(archivo4)]
archivo4=na.omit(archivo4)
                   
conjunto<- cbind(archivo1, archivo4)                  
                   
# ---

conjunto2=conjunto[conjunto$posicion != 1,]
conjunto2$eventos=NA

conjunto1=conjunto[conjunto$posicion == 1,]


conjunto1=cbind(eventos, conjunto1)

conjunto=rbind(conjunto1,conjunto2)

# --------ordena el dataframe por indice para hacer el fillna con el registro previo

conjunto=conjunto[ order(as.numeric(row.names(conjunto))), ]


conjunto = conjunto %>%
  fill(eventos, .direction = "down")

conjunto$lugar=lugar_competencia
conjunto$nombre_competencia=nombre_competencia
conjunto$fecha=fecha_competencia

write.xlsx(conjunto, 'output/cora.xlsx')
file.remove('cora.txt')
file.remove('columna2.csv')

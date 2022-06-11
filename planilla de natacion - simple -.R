library(pdftools)
library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(openxlsx)
library(writexl)


# configuro el directorio de trabajo y borro archivos temporales para que no acumule registros
# estas lineas deberian eliminarse si se usa en una ruta distinta

setwd("C:/projectoR/Planillas Natacion")
# file.remove('cora.txt')
# file.remove('columna2.csv')
# ----------------------------------------

file="data/evento 12  hombres 18-24 100 cc metro estilo de espalda aficionados.pdf"

txt <- pdf_text(file)


destino="cora.txt"

write(txt,destino,append=TRUE)

texto_completo=readLines(destino)

#---------------- selecciona de linea 1 lugar de la competencia 
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

# ------selecciona filtra solo registros de tiempos y eventos de variable texto_completo #####################

pattern="^Evento|(.[0-9]{1,2}\\.[0-9]{2})|[:space:]{1,}---."


tiempoyeventos_cond=str_detect(texto_completo,pattern)
tiempoyeventos_indice=c(str_which(texto_completo,pattern))

tiempo_y_eventos=texto_completo[tiempoyeventos_cond]

# ------------  crea un datafrane con 2 columas (orden, y resto de informacion)
# 

df_tot_indc=data.frame(tiempoyeventos_indice,tiempo_y_eventos)

names(df_tot_indc)=c('orden','todo')
df_tot_indc$orden2=c(1:nrow(df_tot_indc))

eventos =df_tot_indc %>% filter(across(todo, ~ grepl("Evento", .)))
eventos_detalle=df_tot_indc$todo[df_tot_indc$orden2 %in% eventos$orden2]

tiempos = setdiff(df_tot_indc,eventos)

ind_primeros=c(eventos$orden2+1)

primeros= df_tot_indc %>% filter(df_tot_indc$orden2 %in% ind_primeros) 


# ---usando Regex y posicion de columna de texto va creando los campos del DataFrame


pattern='^[0-9]{1,}|^---'
posicion=str_extract(str_trim(tiempos$todo), pattern)
tiempos$posicion=posicion


pattern='(?<=([0-9]{1,2})[:space:]{1,2}).*|(?<=(^---)[:space:]{1}).*'
parcial=str_trim(str_extract(str_trim(tiempos$todo), pattern))


nombre=str_sub(parcial, 1,28)
tiempos$nombre=nombre
parcial2=str_trim(str_sub(parcial, 29,200))
pattern='^[:alpha:][:space:]{1,}+'
parcial2=str_replace_all(parcial2, pattern,'')  # corrige edad


edad=str_sub(parcial2, 1,2)
tiempos$edad=edad
parcial3=str_trim(str_sub(parcial2, 3,200))


club=str_sub(parcial3, 1,32)
tiempos$club=club
parcial4=str_trim(str_sub(parcial3, 33,nchar(parcial3)))

tiempo1=str_trim(str_sub(parcial4, 1,9))
tiempos$tiempo1=tiempo1
parcial5=str_trim(str_sub(parcial4, 10,nchar(parcial4)))

tiempo2=str_trim(str_sub(parcial5, 1,9))
tiempos$tiempo2=tiempo2
parcial6=str_trim(str_sub(parcial5, 10,nchar(parcial4)))
tiempos$puntos=parcial6

tiempos= tiempos %>% select(-todo)

tiempos$competencia=""

# del data frame tiempos solo selecciona los primeros puestos

tiempos %>% filter(tiempos$orden2 %in% primeros$orden2) 

# pega en el campo competencia solo de los primeros puestos el evento de la competencia
tiempos$competencia[tiempos$orden2 %in% primeros$orden2]=eventos_detalle

# reemplaza los campos en blanco con NA para poder aplicar la funcion fillna en proxima linea
tiempos$competencia[tiempos$competencia==''] <- NA

# fillna permite copiar los eventos de competencia de los primeros al resto de competidores
tiempos = tiempos %>% fill(competencia, .direction = "down")

tiempos = tiempos %>% select(-c(orden,orden2))

# crea las columnas de lugar , nombre y fecha de competencia en el dataframe

tiempos$lugar=lugar_competencia
tiempos$nombre_competencia=nombre_competencia
tiempos$fecha=fecha_competencia


# agrega al dataframe acumulado el dataframe creado de cada pdf



file.remove('cora.txt')  # elimina los archvivos creados temporalmente 


# funcion que modifica el formato de hora minuto agregando "0:" segun sea el caso

texto_a_hora <- function (arg1) {
  
  if (is.na(arg1)) { return(NA)}
  else if ( arg1=="NT"|arg1=="NT" ) { return(NA)}
  else if (str_detect(arg1, ':')) { return(arg1)}
  else if (str_detect(arg1, '\\.')) { return(str_c('0:',arg1))}
  else  { return(arg1)}       
}
# aplica la funcion a las columnas  de tiempo 1 y tiempo 2

tiempos$tiempo1<-  sapply(tiempos$tiempo1,texto_a_hora)
tiempos$tiempo2<-  sapply(tiempos$tiempo2,texto_a_hora)

# error edad corrige

pattern="^[:digit:]+"    #[:space:]{3,}


cond_error_edad=str_detect(tiempos$club,pattern)


tiempo_y_eventos=texto_completo[tiempoyeventos_cond]

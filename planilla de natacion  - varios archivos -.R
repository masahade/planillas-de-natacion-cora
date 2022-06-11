library(pdftools)
library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(writexl)


# PARA QUE FUNCIONE EL CODIGO SE NECESITARA INSTLARLO EN UN DIRECTORIO (NO IMPORTA EL NOMBRE)
# PERO EL ARCHIVO DE CODIGO DEBERIA ESTAR EN UN SUBDIRECTORIO "code"
# LOS ARCHIVOS PDF DEBERIAN ESTAR TODOS EN OTRA SUBCARPETA QUE SE LLAME "data"
# DEBERIA CREARSE OTRO SUBDIRECTORIO QUE SE LLAME "output" donde estaran las salidas de los archivos excel y csv
# setwd("C:/projectoR/Planillas Natacion") ejecutar el comando con la ruta elegida al directorio de trabajo elegido
# en mi caso la carpeta se llama planilla de natacion y esta dentro del directorio projector 


setwd("C:/projectoR/Planillas Natacion") # OJO CAMBIAR POR LA RUTA QUE SE ELIJA 


#----------- selecciona los archivos pdf de la carpeta data descartando los mixtos
camino='data'


files= dir(path = camino, pattern = "\\.pdf$", full.names = TRUE, recursive = TRUE)
cond_no_mixto=!str_detect(files,'.mixto.')
files=files[cond_no_mixto]


num=0

# - crea el archivo en formato DataFrame en blanco para ir agregando la info con la iteraccion

DfTotal=data.frame()

for (file in files) {
  
  
  txt <- pdf_text(file)
  
  
  destino="cora.txt"
  
  write(txt,destino,append=TRUE)    # crea archivo txt del pdf en directorio de trabajo
  
  
  texto_completo=readLines(destino)    # lee el arrchivo de texto reado en variable texto_completo
  
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
  
  DfTotal <- rbind(DfTotal,tiempos) 
  
  file.remove('cora.txt')  # elimina los archvivos creados temporalmente 
 
  
}

# funcion que modifica el formato de hora minuto agregando "0:" segun sea el caso
texto_a_hora <- function (arg1) {
  
  if (is.na(arg1)) { return(NA)}
  else if ( arg1=="NT"|arg1=="NT" ) { return(NA)}
  else if (str_detect(arg1, ':')) { return(arg1)}
  else if (str_detect(arg1, '\\.')) { return(str_c('0:',arg1))}
  else  { return(arg1)}       
}
# error edad corrige

pattern="^[:digit:]+"    #[:space:]{3,}


cond_error_edad=str_detect(DfTotal$club,pattern)


tiempo_y_eventos=texto_completo[tiempoyeventos_cond]



# aplica la funcion a las columnas  de tiempo 1 y tiempo 2

DfTotal$tiempo1<-  sapply(DfTotal$tiempo1,texto_a_hora)
DfTotal$tiempo2<-  sapply(DfTotal$tiempo2,texto_a_hora)

# selecciona de estilo / sexo / distancia

pattern='[:space:][:digit:]{2,}[:space:]CC'
comp=str_extract(DfTotal$competencia, pattern)
length(comp)

DfTotal$distancia=comp

comp1=str_to_upper(DfTotal$competencia)
pattern='HOMBRE|MUJER'
DfTotal$sexo=str_extract(comp1,pattern)

pattern='ESPALDA|PECHO|LIBRE|MARIPOSA'
DfTotal$estilo=str_extract(comp1,pattern)


# ------------------ cre los archivos de xlsx y csv del dataframe con la info acumulada

write.csv(DfTotal, 'output/cora_resultado.csv')
write_xlsx(DfTotal,'output/cora_resultado.xlsx')

  
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
file.remove('cora.txt')
file.remove('columna2.csv')
# ----------------------------------------

file="data/evento 11  hombres 25-29 100 cc metro estilo de espalda federados.pdf"

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

# ------selecciona tiempos o eventos #####################

pattern="^Evento|.([0-9]{1,2}:[0-9]{1,2}\\.[0-9]{2})"


tiempoyeventos_cond=str_detect(texto_completo,pattern)

tiempo_y_eventos=str_trim(texto_completo[tiempoyeventos_cond])

pattern="^Evento"

cond_eventos=c(str_which(tiempo_y_eventos,pattern))

cond_w=c(str_which(tiempo_y_eventos,pattern))+1

primeros=tiempo_y_eventos[cond_w]

df_primeros=read_fwf(primeros,
                 fwf_widths(c(2,28,6,35,20,20),c("posicion","nombre","edad","club",'tiempo1','tiempo2'))) 
df_primeros$orden=cond_w
# ---------------##### todos

pattern="([0-9]{1,2}:[0-9]{1,2}\\.[0-9]{2})"

cond=c(str_which(tiempo_y_eventos,pattern))
primeros=tiempo_y_eventos[cond]

df_todos=read_fwf(primeros,
                     fwf_widths(c(2,28,6,35,20,20),c("posicion","nombre","edad","club",'tiempo1','tiempo2'))) 
df_todos$orden=cond

df_diff=setdiff(df_todos,df_primeros)


df_primeros$primeros=TRUE
df_diff$primeros=FALSE

conj_x<- rbind(df_primeros, df_diff) 
# ----------------------------------- HASTA ACA
eventos=tiempo_y_eventos[cond_eventos]


evento_con_registro=data.frame(eventos,cond_w)
names(evento_con_registro)=c('eventos','orden')

df_todos_2=left_join(df_todos,evento_con_registro)

df_todos_2 <-df_todos_2[order(df_todos_2$orden),]

df_todos_2 = df_todos_2 %>%
  fill(eventos, .direction = "down")

df_todos_2$lugar=lugar_competencia
df_todos_2$nombre_competencia=nombre_competencia
df_todos_2$fecha=fecha_competencia

write_xlsx(df_todos_2,'output/cora_resultado.xlsx')
write.csv(df_todos_2, 'output/cora_resultado.csv')
file.remove('cora.txt')
file.remove('columna2.csv')

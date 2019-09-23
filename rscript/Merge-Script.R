################################################ 
### Data Management for Excel SP
### August 2018
### Denise Laroze / Mlopez
################################################


library(plyr)
library(foreign)
library(xtable)
library(tools)




#setwd("C:/Users/Mauro/Desktop/SP_excel")#################################


setwd("C:/Users/Profesor/Dropbox/CESS-Santiago/archive/Pensions - JFF/Design info/certificados SP Final")
rm(list = ls())

#Parameters
pesouf<-27205.11 ### 3 de agosto de 2018
#git<-"C:/Users/Denise Laroze P/Documents/GitHub/Experimento-Metricas-y-Formatos-SCOMP/Tratamientos/"
git<-"~/GitHub/Experimento-Metricas-y-Formatos-SCOMP/Tratamientos/"



###########################################
### Creando un sólo DF con todas los datos
###########################################

## Leer nombre de archivos

perfil.files <- list.files("./csv/", recursive = T, pattern = 'co_.+csv', full.names = T)

##Leer Columnas

all.files <- NULL
for(bf in perfil.files){
  temp <- read.csv(bf, as.is = T)
  valid_col <- ncol(temp)-3
  for(i in 1:valid_col)temp[ , i] <- gsub("\\s","", temp[, i])
  df <- sub(".+/csv/", "\\1", bf)
  df<-file_path_sans_ext(df)
  temp$csvid <- df
  all.files <- rbind.fill(all.files, temp)
}

table(table(all.files$perfil))
unique(all.files$perfil)

all.files$grupo<-ifelse(is.na(all.files$grupo), all.files$ï..grupo, all.files$grupo )

all.files$pair<-ifelse(nchar(all.files$csvid)==5, paste0(all.files$csvid, "rp") , all.files$csvid )
unique(all.files$pair)

all.files$pair2<-ifelse(grepl("rprp", all.files$pair), paste0("co_", all.files$moda, "rp"), all.files$pair)

unique(all.files$pair2)



all.files$perfil2<-substr(all.files$perfil, start = 1, stop = 2)

unique(all.files$perfil2)

table(all.files$perfil2, all.files$pair2)


##Agrego ID Unico = Grupo + Perfil
all.files$id<-paste0(all.files$grupo,".", all.files$perfil2,".", all.files$pair2)



## Transformación de pensión en pesos (valor UF 27.161,48 01/07/2018)
all.files$val_uf_pension_bru<-as.numeric(all.files$val_uf_pension_bru) #make numeric
all.files$val_uf_pension_net<-as.numeric(all.files$val_uf_pension_net) #make numeric
all.files$vpe_fne<-as.numeric(all.files$vpe_fne) #make numeric


all.files$val_uf_pension<-ifelse(is.na(all.files$val_uf_pension_net), all.files$val_uf_pension_bru, 
                                 all.files$val_uf_pension_net)
all.files$val_pesos_pension<-all.files$val_uf_pension*pesouf

all.files$VPN<-all.files$vpe_fne*pesouf

## Ajustar nombres compañias para incorporar espacios en blanco
all.files$razon_social<-all.files$razon_social
all.files$razon_social<-gsub("(AFP)([[:upper:]])", "\\1 \\2", all.files$razon_social)
all.files$razon_social<-gsub("([[:upper:]])(VIDA)", "\\1 \\2", all.files$razon_social)
all.files$razon_social<-gsub("(BTG)([[:upper:]])", "\\1 \\2", all.files$razon_social)
all.files$razon_social<-gsub("([[:upper:]])(NATIONAL)", "\\1 \\2", all.files$razon_social)
all.files$razon_social<-gsub("([[:upper:]])(NACIONAL)", "\\1 \\2", all.files$razon_social)

all.files$razon_social[all.files$razon_social=="AFP PRO VIDA"]<-"AFP PROVIDA"
all.files$razon_social[all.files$razon_social=="CHILENACONSOLIDADA"]<-"CHILENA CONSOLIDADA"
all.files$razon_social[all.files$razon_social=="CNLIFE"]<-"CN LIFE"


## Genero nuevo archivo con los cambios
save(all.files, file = "nuevaBDfinal.RData")

#################### End merge #####################################


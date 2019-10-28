################################################# 
### Data Management for Excel SP
### Jul 2018
### Denise Laroze / Mlopez
################################################



args <- commandArgs(TRUE)

#No incluir el llamado a librerias individuales, sino la carpeta donde estan instaladas
.libPaths=("/usr/lib64/R/library/")

#require(plyr)
#library(santoku)
#require(ggplot2)
#theme_set(theme_bw())
#require(scales)
#require(gridExtra)
#require(xtable)
#require(RColorBrewer)
#require(htmlTable)
#library(gridBase)
#require(grid)
#require(forcats)
#require(pacman)
#pacman::p_load(ggplot2, extrafont, scales)
#require(purrr, warn.conflicts = FALSE, quietly = TRUE)
#require(magick,  warn.conflicts = FALSE, quietly = TRUE)
#require(scales)
#require(OpenImageR)

#Amazon Server
setwd("/var/www/r.cess.cl/public_html/")


####################################################################
###########################block randmisation####################### 

if(args[1] == "reset_database"){
  time <- Sys.time()
  time <- gsub("[:alph:]", "", time)
  time <- gsub(" ", "_", time)
  
  file.copy("/var/www/r.cess.cl/public_html/sp/new.RData", sprintf("rdata_bak_%s.Rdata", time))
  file.copy("/var/www/r.cess.cl/public_html/sp/new_orig.RData", "new_SERNAC.RData", overwrite = T)
  stop()
}

#args <- as.vector(t(sim.data[i, ]))
if(length(args) != 3){
  stop()
}

# Load data
load(file="/var/www/r.cess.cl/public_html/sp/new_SERNAC.RData")


# args from Qualtrics
# 1- ID
# 2 - Birthyear
# 3 - econq


#args<-c(2345, 1987, 4) #For testing

age<-2019-args[2]
age_cat<-if (age < 35) 1 else if (age > 55) 4 else if (age >35 & age <= 45) 2 else 3
#age_cat


QID = args[1]

#QID = 236556
#age_cat<-1

#args<-c(23569, 65, "2")

if(sum(part.data$QID %in% QID)>0){
  # Retuen value to PHP via stdout
  tr <- bdata$x$Tr[which(bdata$x$QID==QID)[1]]
  
} else {
  # update the data.frame
  part.data <- rbind(part.data, 
                     data.frame(QID=args[1], 
                                age_cat_recode=as.numeric(age_cat)+rnorm(1,sd=.001),
                                econQ=as.numeric(args[3])+rnorm(1,sd=.001)))
  # update the seqblock objects
  n.idx <- nrow(part.data)
  bdata <- seqblock2k(object.name = "bdata", 
                      id.vals = part.data[n.idx, "QID"],  
                      covar.vals = part.data[n.idx,-c(1)], 
                      verbose = FALSE)
  
  tr <- bdata$x$Tr[length(bdata$x$Tr)]
  
  # Save data
 save(mahal,seqblock1,seqblock2k,bdata,part.data,file="/var/www/r.cess.cl/public_html/sp/new_SERNAC.RData")
}

#tr<-strsplit(tr,split = ",")[[1]]
tr<-as.numeric(tr)

#load("/var/www/r.cess.cl/public_html/sp/nuevaBDfinal.RData")



#### Names of treatment selection 
namedTr<-c("control", "treat1", "treat2", "treat3")

selected<-c(namedTr[tr[1]])

#envio de datos a qualtrics
to_qs<-c(selected)
cat(sprintf("%s", to_qs[1]))


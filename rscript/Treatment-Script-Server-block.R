################################################# 
### Data Management for Excel SP
### Jul 2018
### Denise Laroze / Mlopez
################################################



args <- commandArgs(TRUE)

#No incluir el llamado a librerias individuales, sino la carpeta donde estan instaladas
.libPaths=("/usr/lib64/R/library/")

require(plyr)
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

#Parameters
pesouf<-27205.11 ### 3 de agosto de 2018
path<-"/var/www/r.cess.cl/public_html/sp/"

####################################################################
###########################block randmisation####################### 

if(args[1] == "reset_database"){
  time <- Sys.time()
  time <- gsub("[:alph:]", "", time)
  time <- gsub(" ", "_", time)
  
  file.copy("/var/www/r.cess.cl/public_html/sp/new.RData", sprintf("rdata_bak_%s.Rdata", time))
  file.copy("/var/www/r.cess.cl/public_html/sp/new_orig.RData", "new.RData", overwrite = T)
  stop()
}

#args <- as.vector(t(sim.data[i, ]))
if(length(args) != 3){
  stop()
}

# Load data
load(file="/var/www/r.cess.cl/public_html/sp/new.RData")


# args from Qualtrics
# 1- ID
# 2 - Birthyear
# 3 - econq


args<-c(2345, 1985, 4)

age<-2019-args[2]
chop(x, c(300, 600, 900), labels = LETTERS[1:4])



QID = args[1]

if(sum(part.data$QID %in% QID)>0){
  # Retuen value to PHP via stdout
  tr <- bdata$x$Tr[which(bdata$x$QID==QID)[1]]
  
} else {
  # update the data.frame
  part.data <- rbind(part.data, 
                     data.frame(QID=args[6], 
                                byear=as.numeric(args[2])+rnorm(1,sd=.001),
                                econQ=as.numeric(args[3])+rnorm(1,sd=.001)))
  # update the seqblock objects
  n.idx <- nrow(part.data)
  bdata <- seqblock2k(object.name = "bdata", 
                      id.vals = part.data[n.idx, "QID"],  
                      covar.vals = part.data[n.idx,-c(1)], 
                      verbose = FALSE)
  
  tr <- bdata$x$Tr[length(bdata$x$Tr)]
  
  # Save data
  save(mahal,seqblock1,seqblock2k,bdata,part.data,file="/var/www/r.cess.cl/public_html/sp/new.RData")
}

tr<-strsplit(tr,split = ",")[[1]]
tr<-as.numeric(tr)

load("/var/www/r.cess.cl/public_html/sp/nuevaBDfinal.RData")



#### list of treatment functions
#
#namedVF<-list(control=fcn.control, treat1=fcn.treat1, treat2=fcn.treat2, treat3=fcn.treat3, treat4=fcn.treat4  )
#namedVF<-list(control=fcn.treat2, treat1=fcn.treat2, treat2=fcn.treat2, treat3=fcn.treat2, treat4=fcn.treat2 )

#### Random selection of treatment without replacement
selected<-c(namedVF[tr[1]],namedVF[tr[2]])
selectedQID<-names(selected) ## list of selected treatments to send to Qualtrics


#envio de datos a qualtrics
to_qs<-c(pay.op1, pay.op2, selectedQID, length1, length2)
cat(sprintf("%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s", to_qs[1], to_qs[2], to_qs[3], to_qs[4], to_qs[5], to_qs[6], to_qs[7], to_qs[8], to_qs[9], to_qs[10], to_qs[11],to_qs[12], to_qs[13], to_qs[14], to_qs[15], to_qs[16], to_qs[17], to_qs[18], to_qs[19], to_qs[20], to_qs[21],to_qs[22], to_qs[23], to_qs[24], to_qs[25], to_qs[26], to_qs[27], to_qs[28], to_qs[29], to_qs[30], to_qs[31], to_qs[32], to_qs[33], to_qs[34]))
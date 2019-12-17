#########################################
##### Formatos y Métricas Scomp
##### code: Denise Laroze
##### 2018-9
#########################################



library(naniar)
library(readr)
rm(list=ls())

#setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/SERNAC/Data analysis")
df<-(read_csv("Data/pilot.csv")[-1:-2,])


####### Clean

df<-droplevels(df) ## Eliminate unused levels


##### Eliminate observations with missing treatment variables

df<-df[complete.cases(df$treatv1),]
table(df$Qconsent) ## checking if there are any non-consenting observations
table(df$treatv1)

#### Eliminate duplicated observations/participations
#df$rut<-gsub('-|\\.| |.{1}$','',df$Qrut)
#df$rut<-as.numeric(df$rut) 
#View(df[is.na(df$rut),c("Qrut", "rut", "treatment1", "Qt1")]) To check if warnings are irrelevant
#df2<-df[!duplicated("rut"),] ## eliminate duplicated ruts ### Total observations, before cleaning. 



#### Identify time preference variable
# tmp<-df[, c(paste0("Q", 207:237), "Q239")]
# 
# tmp$timevalue<-NA
# for (i in 1:nrow(tmp)){
#   NonNAindex <- which(!is.na(tmp[i,]))
#   last <- max(NonNAindex)
#   tmp$timevalue[i]<-colnames(tmp)[last]
# }
# 
# df$timevalue<-tmp$timevalue



###################
### Recode 
######################

df$total_option<-as.numeric(df$treat1)
#summary(df$total_option)

df$treat<-df$treatv1


#### Fin Lit questions
table(df$Qmath5)

### Math 5 Question, price of ball
df$Qmath5num<-parse_number(df$Qmath5)

#View(df[,c("Qmath5", "Qmath5num")])
#table(df$Qmath5num)
df$Qmath5_correct<-ifelse(df$Qmath5num == 5000,1, ifelse(df$Qmath5num==5, 1, 0))
#table(df$Qmath5num, df$Qmath5_correct)

### correct answers  for Qmath3 and Qmath4

#table(df$Qmath3)
df$Qmath3_correct<-ifelse(df$Qmath3=="Más de $125.000.000", 1, 0)
#table(df$Qmath3, df$Qmath3_correct)


#table(df$Qmath4)
df$Qmath4_correct<-ifelse(df$Qmath4=="Nunca se terminaría de pagar el crédito", 1, 0)
#table(df$Qmath4, df$Qmath4_correct)

tmp<-df[, c("Qmath3_correct", "Qmath4_correct", "Qmath5_correct") ]
tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)

df$financial_lit<-tmp$financial_lit
rm(tmp)

#confianza y claridad
df$conf<-as.numeric(df$Q329_1)
df$claro<-as.numeric(df$Q332_1)


#Age categorization used in block randomization
df$age<-2019-as.numeric(df$Qbirth)
df$age_cat<-ifelse (df$age <= 35, "under 35", ifelse(df$age > 55, "55+", ifelse(df$age >35 & df$age <= 45, "36 to 45", "46 to 55")))





###########################################
### Reshape
###########################################


df$uid<-seq.int(nrow(df))
df2<-df[,c("uid", "Qgender", "Qbirth","QSEC","QEdu", 
           "total_option", "Qcotiza1a", "Qcotiza1b", "Qcotiza1c", "Qcotiza1d",
           "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit"
          )]
names(df2)<-c("uid", "Qgender", "Qbirth","QSEC","QEdu", 
              "total_option", "Qcotiza.1a", "Qcotiza.1b", "Qcotiza.1c", "Qcotiza.1d",
              "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit"
              )


df3<-df[,c("uid", "treat", "total_option", "Qcotiza1a", "Qcotiza1b", "Qcotiza1c", "Qcotiza1d", 
"Qsim1a", "Qsim1b" , "Qsim1b" , "Qsim1d")]
names(df3)<-c("uid", "treat", "total_option", "Qcotiza.1a", "Qcotiza.1b", "Qcotiza.1c", "Qcotiza.1d",
              "Qsim.1a", "Qsim.1b" , "Qsim.1b" , "Qsim.1d")

df3<-as.data.frame(df3)

myvar<-c("Qcotiza.1a", "Qcotiza.1b", "Qcotiza.1c", "Qcotiza.1d",
         "Qsim.1a", "Qsim.1b" , "Qsim.1b" , "Qsim.1d")



dfl<-reshape(df3, varying=myvar, 
             direction="long", idvar="uid", sep=".")

dfl<-dfl[order(dfl$uid),]


### Merge in relevant covariates
idvar<-dput(names(df2))
idvar<-idvar[!idvar %in% myvar]  

df2<-df[, idvar]


dfl<-merge(dfl, df2, by="uid")

rm(df2, df3)

###############
### Recode dfl
##############

## Option as numeric
dfl$opn<-substring(dfl$Qsim,8)
dfl$opn<-as.numeric(dfl$opn)
#dfl$reward<-as.numeric(dfl$reward)
dfl$Qbirth<-as.numeric(dfl$Qbirth)

#correct option
dfl$c.opn<-ifelse(dfl$time=="1a" & dfl$opn==2, 1, 
                  ifelse(dfl$time=="1b" & dfl$opn==3, 1,  
                         ifelse(dfl$time=="1c" & dfl$opn==1, 1, 
                                ifelse(dfl$time=="1d" & dfl$opn==4, 1, 0 ))))


dfl$clicked<-ifelse(dfl$Qcotiza=="Cotizar opciones de crédito", 1, 0)
#table(dfl$Qcotiza, dfl$clicked)



####### Save

save(df, dfl, file = "Data/Pilot/clean_data.Rdata")




#############
############################
### Reference proyect
#############################








##############
### Anonimise
################

# eliminate

elim<-c( "Qrut",  "Qtipeaccount", "Qnamebank", "Qnaccount1", "Qnaccount2" ,
         "Duration..in.seconds.", "ResponseId", "LocationLatitude", "LocationLongitude", "DistributionChannel",   "UserLanguage",         
         "Q250_First.Click",   "Q250_Last.Click" ,  "Q250_Page.Submit", "Q250_Click.Count" , "Q255_Browser", "Q255_Version",         
         "Q255_Operating.System", "Q255_Resolution")


df <- df[ , !(names(df) %in% elim)]

###########################################









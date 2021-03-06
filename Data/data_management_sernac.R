#########################################
##### Formatos y M�tricas Scomp
##### code: Denise Laroze
##### 2018-9
#########################################



library(naniar)
library(readr)
rm(list=ls())

setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/SERNAC/Data analysis/Experiment")
df<-(read_csv("Data/data_sernac2.csv")[-1:-3,])
df2<-(read_csv("Data/data_sernac_FB2.csv")[-1:-3,])

vars<-names(df)
df<-rbind(df, df2[,vars])
drop(df2)


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
tmp<-df[, c(113:144)]
# 
tmp$timevalue<-NA
for (i in 1:nrow(tmp)){
   NonNAindex <- which(!is.na(tmp[i,]))
   last <- max(NonNAindex)
   tmp$timevalue[i]<-colnames(tmp)[last]
 }
# 
 df$timevalue<-tmp$timevalue
 df$pb<-as.numeric(gsub('\\D+','',tmp$timevalue))

 
###################
### Recode 
######################

df$total_option<-as.numeric(df$treat1)
#summary(df$total_option)

df$Qbirth<-as.numeric(df$Qbirth)

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
df$Qmath3_correct<-ifelse(df$Qmath3=="M�s de $125.000.000", 1, 0)
#table(df$Qmath3, df$Qmath3_correct)


#table(df$Qmath4)
df$Qmath4_correct<-ifelse(df$Qmath4=="Nunca se terminar�a de pagar el cr�dito", 1, 0)
#table(df$Qmath4, df$Qmath4_correct)

tmp<-df[, c("Qmath3_correct", "Qmath4_correct", "Qmath5_correct") ]
tmp[is.na(tmp)] <- 0
tmp$financial_lit<-rowSums(tmp)

df$financial_lit<-tmp$financial_lit
rm(tmp)

#confianza y claridad
df$conf<-as.numeric(df$Q329_1)
df$claro<-as.numeric(df$Q332_1)
df$QconfBIF_1<-as.numeric(df$QconfBIF_1)


#Age categorization used in block randomization
df$age<-2019-as.numeric(df$Qbirth)
df$age_cat<-ifelse (df$age <= 35, "<36", ifelse(df$age > 55, "56+", ifelse(df$age >35 & df$age <= 45, "36 to 45", "46 to 55")))
df$age_cat<- factor(df$age_cat, levels = c("<36", "36 to 45", "46 to 55", "56+"))

# preguntas comprension
df$comp.tot<-as.numeric(df$total_reward) - 3000 - as.numeric(df$risk_q_reward) - as.numeric(df$total_option)
#summary(df$comp.tot)
#table(df$comp.tot)
df$comp<-as.numeric(df$comp)
df$allright<-ifelse(df$comp==400, 1, 0)

df$comp1.r<-ifelse(df$comp1==1, 1, 0 )
df$comp2.r<-ifelse(df$comp2==1, 1, 0 )

df$comp3.r<-ifelse(df$comp3==1, 1, 0 )
df$comp5.r<-ifelse(df$comp5==2, 1, 0 )


#Financial Literacy
df$financial_lit_fact<-ifelse(df$financial_lit==0, "No Fin. Lit.",  
                               ifelse( df$financial_lit==1, "Low Fin. Lit.",
                                       ifelse( df$financial_lit==2, "Mid. Fin. Lit.","High Fin. Lit." )))


df$financial_lit_fact<- factor(df$financial_lit_fact, levels = c("No Fin. Lit.", "Low Fin. Lit.", "Mid. Fin. Lit.", "High Fin. Lit."))                   

#table(dfl$financial_lit, dfl$financial_lit_fact)

df$risk_fact<-ifelse(df$Qrisk1=="100% probabilidades de ganar $720", "Risk level 1", 
                      ifelse(df$Qrisk1=="50% probabilidades de ganar  $1.080 y 50% probabilidades de ganar $540", "Risk level 2",
                             ifelse(df$Qrisk1=="50% probabilidades de ganar $1.440 y 50% probabilidades de ganar  $360", "Risk level 3",
                                    ifelse(df$Qrisk1=="50% probabilidades de ganar  $1.800 y 50% probabilidades de ganar $180", "Risk level 4"
                                           ,"Risk level 5"))))


###########################################
### Reshape
###########################################


df$uid<-seq.int(nrow(df))
df2<-df[,c("uid", "Qgender", "Qbirth","QSEC","QEdu", 
           "total_option", "Qcotiza1a", "Qcotiza1b", "Qcotiza1c", "Qcotiza1d",
           "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit",
           "Qrisk1", "Qrisk2","Qrisk3_1", "Qrisk3_2","Qrisk3_3", "Qrisk3_4", "Qrisk3_5", "Qrisk3_6",
           "comp", "comp3.r", "claro", "conf", "pb"
          )]
names(df2)<-c("uid", "Qgender", "Qbirth","QSEC","QEdu", 
              "total_option", "Qcotiza.1a", "Qcotiza.1b", "Qcotiza.1c", "Qcotiza.1d",
              "Qmath1","Qmath2","Qmath3","Qmath4", "Qmath5num",  "Qmath3_correct", "Qmath4_correct", "Qmath5_correct", "financial_lit",
              "Qrisk1", "Qrisk2","Qrisk3_1", "Qrisk3_2","Qrisk3_3", "Qrisk3_4", "Qrisk3_5", "Qrisk3_6",
              "comp", "comp3.r", "claro", "conf", "pb"
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


dfl$clicked<-ifelse(dfl$Qcotiza=="Cotizar opciones de cr�dito", 1, 0)
#table(dfl$Qcotiza, dfl$clicked)


#Financial Literacy
dfl$financial_lit_fact<-ifelse(dfl$financial_lit==0, "No Fin. Lit.",  
                               ifelse( dfl$financial_lit==1, "Low Fin. Lit.",
                                       ifelse( dfl$financial_lit==2, "Mid. Fin. Lit.","High Fin. Lit." )))


dfl$financial_lit_fact<- factor(dfl$financial_lit_fact, levels = c("No Fin. Lit.", "Low Fin. Lit.", "Mid. Fin. Lit.", "High Fin. Lit."))                   

#Risk Clasification
#table(dfl$financial_lit, dfl$financial_lit_fact)

dfl$risk_fact<-ifelse(dfl$Qrisk1=="100% probabilidades de ganar $720", "Risk level 1", 
                      ifelse(dfl$Qrisk1=="50% probabilidades de ganar  $1.080 y 50% probabilidades de ganar $540", "Risk level 2",
                             ifelse(dfl$Qrisk1=="50% probabilidades de ganar $1.440 y 50% probabilidades de ganar  $360", "Risk level 3",
                                    ifelse(dfl$Qrisk1=="50% probabilidades de ganar  $1.800 y 50% probabilidades de ganar $180", "Risk level 4"
                                           ,"Risk level 5"))))




### For both DF and DFL

df$d.pb<-ifelse(df$pb<mean(df$pb, na.rm=T), "Low PB", "High PB")
dfl$d.pb<-ifelse(dfl$pb<mean(dfl$pb, na.rm=T), "Low PB", "High PB")



####### Save

save(df, dfl, file = "Data/clean_data.Rdata")


















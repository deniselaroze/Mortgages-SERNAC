###########################################
### Data analysis - Mortgages SERNAC
### Dec 16, 2019
### Denise Laroze
###########################################


library(stargazer)
library(lattice)
library(dplyr)
library(Rmisc)
library(ggplot2)
library(nnet)

library(plm)
library(multiwayvcov)
library(lmtest)

library(BayesTree)
library(ggpubr)
theme_set(theme_bw())
library(rms)
library(xtable)
library(MASS)

library(margins)

rm(list=ls())

setwd("C:/Users/Denise Laroze/Dropbox/CESS-Santiago/archive/SERNAC/Data analysis/Experiment")
load("Data/clean_data.Rdata")
fig.path<-"Figures"
colors<-c("blue",  "purple", "darkgreen", "orange", "red")

####################
### Sub groups
####################

low<-c("nivel1", "nivel2")
df.low<-df[df$QSEC %in% low,]
dfl.low<-dfl[dfl$QSEC %in% low,]

####################
## Descriptives
###################

prop.table(table(df$Qgender))
prop.table(table(df$age_cat))
prop.table(table(df$QSEC))
prop.table(table(df$QCredito))
prop.table(table(df$QinteresCredito))


### Age
summary(df$age)
mean(df$age)
sd(df$age)

### Gender and credit.
tbl<-as.table(table(df$Qgender, df$QCredito))
prop.table(tbl,1)

tbl<-as.table(table(df$Qgender, df$QinteresCredito))
prop.table(tbl,1)



prop.table(table(df$QEdu))

prop.table(table(df$financial_lit))
table(df$comp)

#Total payment
df$total_reward<-as.numeric(df$total_reward)
summary(df$total_reward)


# Comprensión CAE
prop.table(table(df$QCAE, df$QEdu)) # B es correcta




## Densidad edad
ggplot(df, aes(x=age, fill=treat)) + 
  geom_density(alpha=.5,  position="identity") +
  scale_fill_manual("", values=colors,
                    labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))

ggsave(paste0("Age_dist", ".png"), path=fig.path,  width = 7, height = 4)


#preguntas de comprensión
prop.table(table(df$comp1))
prop.table(table(df$comp2))
prop.table(table(df$comp3))
prop.table(table(df$comp5))

df$c.comp1<-ifelse(df$comp1==1, 1, 0)
df$c.comp2<-ifelse(df$comp2==1, 1, 0)
df$c.comp3<-ifelse(df$comp3==1, 1, 0)
df$c.comp5<-ifelse(df$comp5==2, 1, 0)


library(Hmisc)


d.df<-df[, c("age_cat", "Qgender", "QSEC", "risk_fact", "financial_lit_fact", "c.comp1", "c.comp2", "c.comp3", "c.comp5","comp",
             "claro", "conf")]
x<-psych::describe(d.df)
xtable(x)
#############
### Balance
###############


table(df$treat)



tbl<-ddply(df, .(age_cat, QSEC, treat) , summarise,
           #n.treat = length(unique(treat)),
           subj.n = length(unique(uid))
)
tbl
xt<-xtable(tbl)
print(xt, type="latex", file=("Tables/balance_numbers.tex"), floating=FALSE, include.rownames=FALSE)




b<-multinom(treat ~ age_cat + Qgender +QSEC + risk_fact + financial_lit_fact + d.pb, data = df)
#summary(b)

stargazer(b, out="Tables/balance.tex", type="latex",
          covariate.labels = c("Age 46-55", "Age 55+", "Age >36","Male", "SEC 2", "SEC 3", "SEC 4", "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit.", "Low Present Bias"), 
          dep.var.labels = c("T.1", "T.2", "T.3"), # keep.stat=c("n", "ll"),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:balance",
          title = "Treatment balance tests", no.space=TRUE)






##################
### Click
##################

set.seed(2365)
glm1<-lrm(clicked ~ treat , data=dfl, x=T, y=T)
glm1.cl<-bootcov(glm1,cluster=dfl$uid)

set.seed(2365)
glm2<-lrm(clicked ~ treat  + Qgender  + risk_fact + d.pb  , data=dfl, x=T, y=T)
glm2.cl<-bootcov(glm2,cluster=dfl$uid)

set.seed(2365)
glm2l<-lrm(clicked ~ treat  + Qgender  + risk_fact  , data=dfl.low, x=T, y=T)
glm2l.cl<-bootcov(glm2l,cluster=dfl.low$uid)

set.seed(2365)
glm3<-lrm(clicked ~ time , data=dfl, x=T, y=T)
glm3.cl<-bootcov(glm3,cluster=dfl$uid)

set.seed(2365)
glm4<-lrm(clicked ~ time  + Qgender  + risk_fact + d.pb , data=dfl, x=T, y=T)
glm4.cl<-bootcov(glm4,cluster=dfl$uid)

set.seed(2365)
glm4l<-lrm(clicked ~ time  + Qgender  + risk_fact  , data=dfl, x=T, y=T)
glm4l.cl<-bootcov(glm4l,cluster=dfl.low$uid)


#### substantive marginal effects Clicked
tot.m<-1097546 # Credits  -  CMF Jan 2020

glm1.m<-glm(clicked ~ treat , data=dfl)
x<-margins(glm1.m)

tot.m<-1097546 # según informa la CMF
summary(x)
x<-summary(x)
low<-x[3,6]
upper<-x[3,7]
upper

lower.c<-low*tot.m
lower.c # Lower bound marginal effect
upper.c<-upper*tot.m # Upper bound marginal effect
upper.c

### Marginal effects variants
glm3.m<-glm(clicked ~ time , data=dfl)
x2<-margins(glm3.m)
summary(x2)


######## Export relevant models
stargazer(glm1.cl, glm2.cl, out="Tables/click_models.tex", type="latex",
          #title = "Regression Results", 
          out.header = F,model.names = F, 
          covariate.labels = c("T.1", "T.2", "T.3", "Male" , "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5",  "Low PB",
                               "Constant"), 
          dep.var.labels.include = F,
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:click_treat_effects",  table.placement = "H",
          title = "Estimaciones Logísticas en la probabilidad de hacer Click por tratamiento, con y sin controlar por variables desbalanceadas
          en la asignación de tratamientos, con bootstrap clustered standared errors.", no.space=TRUE)


######## Export relevant models
stargazer(glm3.cl, glm4.cl , out="Tables/click_models_var.tex", type="latex",
          #title = "Regression Results", 
          out.header = F,model.names = F, 
          covariate.labels = c("DC-AA","DL-AB", "DL-AA", "Male" , "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5",  "Low PB",
                               "Constant"), 
          dep.var.labels.include = F,
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.05, 0.01, 0.001), notes.align = "l",
          label="tbl:click_var_effects",  table.placement = "H",
          title = "Estimaciones Logísticas en la probabilidad de hacer Click por condiciones crediticias (variantes DL-AA, DL-AB, DC-AA, CD-AB), con y sin controlar por variables desbalanceadas
          en la asignación de tratamientos, con bootstrap clustered standared errors.", no.space=TRUE)


##################
### Comprensión
##################


##### Opción correcta
dfl.click<-dfl[dfl$clicked==1,]

set.seed(2365)
glm1<-lrm(c.opn ~ treat , data=dfl.click, x=T, y=T)
glm1.cl<-bootcov(glm1,cluster=dfl.click$uid)
#glm1.cl

set.seed(22365)
glm4<-lrm(c.opn ~ treat +  Qgender  + risk_fact , data=dfl, x=T, y=T)
glm4.cl<-bootcov(glm4,cluster=dfl$uid)


stargazer(glm1.cl, glm4.cl, out="Tables/c_opn_models.tex", #type="latex",
          out.header = F,model.names = F, #
          covariate.labels = c("T.1", "T.2", "T.3", "Male" , "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5","Constant"), 
          dep.var.labels.include = F,
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01), notes.align = "l",
          label="tbl:c_opn_treat_effects",  table.placement = "H",
          title = "Estimación logística sobre la probabilidad de seleccionar la oferta correcta por tratamiento, con y sin controlar por variables desbalanceadas
          en la asignación de tratamientos, con bootstrap clustered standared errors.", no.space=TRUE)

#Preguntas de compresión
set.seed(2365)
olm1<-polr(factor(comp) ~ treat , data=df, Hess=T)
olm2<-polr(factor(comp) ~ treat* +  Qgender  + risk_fact, data=df.low, Hess=T)
summary(olm2)


# Comp1 correct - Renegociar general
glm1<-glm(comp1.r ~ treat, data=df, family = "binomial")
glm2<-glm(comp1.r ~ treat +  Qgender  + risk_fact, data=df, family = "binomial")
glm3<-glm(comp1.r ~ treat +  Qgender  + risk_fact, data=df.low, family = "binomial")
summary(glm3)

# Comp2 correct - Mejor métrica evaluación
glm4<-glm(comp2.r ~ treat, data=df, family = "binomial")
glm5<-glm(comp2.r ~ treat +  Qgender  + risk_fact, data=df, family = "binomial")
glm6<-glm(comp2.r ~ treat +  Qgender  + risk_fact + financial_lit_fact, data=df.low, family = "binomial")
summary(glm4)

# Comp3 correct - Tasa propia, CAE y Comparado
glm7<-glm(comp3.r ~ treat, data=df, family = "binomial")
glm8<-glm(comp3.r ~ treat +  Qgender  + risk_fact + financial_lit_fact, data=df, family = "binomial")
glm9<-glm(comp3.r ~ treat +  Qgender  + risk_fact + financial_lit_fact, data=df.low, family = "binomial")
summary(glm7)


# Comp5 correct - Costo del Pre-pago
glm10<-glm(comp5.r ~ treat, data=df, family = "binomial")
glm11<-glm(comp5.r ~ treat +  Qgender  + risk_fact + financial_lit_fact , data=df, family = "binomial")
glm12<-glm(comp5.r ~ treat +  Qgender  + risk_fact + financial_lit_fact, data=df.low, family = "binomial")


# Todas correctas
glm.a1<-glm(allright ~ treat, data=df, family = "binomial")

glm.a2<-glm(allright ~ treat +  Qgender  + risk_fact + financial_lit_fact , data=df, family = "binomial")


# Marginal effects
margins(glm7)

margins(glm9,at = list(Qgender = "M"))



stargazer(glm.a2, glm7, glm8, glm9, out="Tables/comp3_models.tex", #type="latex",
          #title = "Regression Results", 
          out.header = F,model.names = F, #
          covariate.labels = c("T.1", "T.2", "T.3","Male" , "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Low Fin. Lit", "Mid Fin. Lit.", "High Fin. Lit.", "Constant"), 
          dep.var.labels.include = F,
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01), notes.align = "l",
          label="tbl:allright_treat_effects",  table.placement = "H",
          title = "Estimación Logística sobre la probabilidad de contestar preguntas correctamente, con y sin controlar por variables desbalanceadas
          en la asignación de tratamientos.", no.space=TRUE)



##################
### Attributes
##################

set.seed(2365)

olm1<-polr(factor(conf) ~ treat , data=df, Hess=T)
olm2<-polr(factor(conf) ~ treat +  Qgender  + risk_fact + as.numeric(QconfBIF_1)  , data=df, Hess=T)
summary(olm2)

conf<-df$conf
SBIF<-as.numeric(df$QconfBIF_1)

cor(conf, SBIF,  use = "complete.obs")
plot(conf, SBIF)

stargazer(olm1, olm2, out="Tables/atrib_models.tex", type="latex",
          out.header = F,model.names = F, covariate.labels = c("T.1", "T.2", "T.3", "Male" , "Risk level 2", "Risk level 3", "Risk level 4", "Risk level 5", "Conf. BIF", "Constant"), 
          dep.var.labels.include = F,
          #add.lines = list(c("N", nobs(glm4), nobs(glm3) )),
          dep.var.caption = "", star.cutoffs = c(0.1, 0.05, 0.01), notes.align = "l",
          label="tbl:atrib_treat_effects",  table.placement = "H",
          title = "Estimaciones ordinales (0-10) en la confianza que genera la Comunicación Trimestral por tratamiento", no.space=TRUE)







#####################################
### Effect click by treatment
#####################################



plot.df <- summarySE(dfl, measurevar="clicked", groupvars=c("treat"), na.rm=T)


ggplot(plot.df, aes(x = factor(treat), y = clicked , color=factor(treat))) + 
  geom_point(position = position_dodge(width=0.3), stat="identity", size=3) +
  scale_color_manual("", values=colors)+
  geom_errorbar(aes(ymin=(clicked-ci), ymax=(clicked+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Proporción que hizo click") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  ylim(0, 1)+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("tot_clicked_dfl", ".png"), path=fig.path,  width = 7, height = 4)



#####################################
### Effect click by variant
#####################################



plot.df <- summarySE(dfl, measurevar="clicked", groupvars=c("time"), na.rm=T)


ggplot(plot.df, aes(x = factor(time), y = clicked , color=factor(time))) + 
  geom_point(position = position_dodge(width=0.3), stat="identity", size=3) +
  scale_color_manual("", values=colors)+
  geom_errorbar(aes(ymin=(clicked-ci), ymax=(clicked+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Proporción que hizo click") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  ylim(0, 1)+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("1a"="DC-AB","1b"="DC-AA","1c"="DL-AB", "1d"="DL-AA"))


ggsave(paste0("var_clicked_dfl", ".png"), path=fig.path,  width = 7, height = 4)







#####################################
### Effect correct option by treatment
#####################################

plot.df<-dfl[dfl$clicked==1, ]

plot.df <- summarySE(plot.df, measurevar="c.opn", groupvars=c("treat"), na.rm=T)

plot.df<-plot.df[complete.cases(plot.df$c.opn),]

ggplot(plot.df, aes(x = factor(treat), y = c.opn , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(c.opn-ci), ymax=(c.opn+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Proporción elige opción correcta") + xlab("") +
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  ylim(0, 1)+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("tot_c_opn_dfl", ".png"), path=fig.path,  width = 7, height = 4)



#####################################
### Effect Trust by treatment
#####################################


# 
plot.df <- summarySE(df, measurevar="conf", groupvars=c("treat"), na.rm=T)


ggplot(plot.df, aes(x = factor(treat), y = conf , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(conf-ci), ymax=(conf+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Confianza en la comunicación") + xlab("") + ylim(0, 10)+
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("conf_treat_df", ".png"), path=fig.path,  width = 7, height = 4)




#####################################
### Effect clarity by treatment
#####################################


# 
plot.df <- summarySE(df, measurevar="claro", groupvars=c("treat"), na.rm=T)


ggplot(plot.df, aes(x = factor(treat), y = claro , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(claro-ci), ymax=(claro+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Claridad de la comunicación") + xlab("") + ylim(0, 10)+
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("claro_treat_df", ".png"), path=fig.path,  width = 7, height = 4)



#####################################
### Effect comprehension by treatment
#####################################


plot.df <- summarySE(df, measurevar="comp", groupvars=c("treat"), na.rm=T)


ggplot(plot.df, aes(x = factor(treat), y = comp , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(comp-ci), ymax=(comp+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Ganancia preguntas comprensión") + xlab("") + ylim(0, 400)+
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("tot_comp_treat_df", ".png"), path=fig.path,  width = 7, height = 4)


# 4/4 versus el resto

table(df$allright)
plot.df <- summarySE(df, measurevar="allright", groupvars=c("treat"), na.rm=T)

ggplot(plot.df, aes(x = factor(treat), y = allright , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(allright-ci), ymax=(allright+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Proporcion todo correcto") + xlab("") + ylim(0, 1)+
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("allright_treat_df", ".png"), path=fig.path,  width = 7, height = 4)




##########################33
#### Heterogeneity
##########################

###########################
### Gender Heterogeneity
###########################
set.seed(89)

# Data set up
df.b <- dfl

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("comp", "treat.het",  "Qgender", "QSEC",  "financial_lit" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$comp) 
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# Female participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$Qgender =="F" )/sum(CATE_df$Qgender == "F")

# Male participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$Qgender == "M" )/sum(CATE_df$Qgender =="M")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(Qgender))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))+
  scale_fill_discrete(name = "", labels = c("Female", "Male"))
#scale_fill_manual(name="Mode", values=colours) +
# +
#scale_x_continuous(limits = c(0,5220))

# Combine all plots into one chart
gender_het <- ggarrange(effectsPlot, modePlot,
                        ncol = 1, nrow = 2, heights = c(2,2))

ggsave(gender_het, filename = "gender_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)





#################3


###########################
### SEC Heterogeneity
###########################

set.seed(89)

# Data set up including calculating ability rank
df.b <- dfl

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("comp", "treat.het",  "Qgender", "QSEC" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$comp) 
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# SEC level 1 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel1" )/sum(CATE_df$QSEC == "nivel1")

# SEC level 2 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel2" )/sum(CATE_df$QSEC == "nivel2")

# SEC level 3 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel3" )/sum(CATE_df$QSEC == "nivel3")

# SEC level 4 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QSEC == "nivel4" )/sum(CATE_df$QSEC == "nivel4")


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=QSEC)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))+
  scale_fill_discrete(name = "", labels = c("SEC 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +


# Combine all plots into one chart
SEC_het <- ggarrange(effectsPlot, modePlot,
                     ncol = 1, nrow = 2, heights = c(2,2))

ggsave(SEC_het, filename = "SEC_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




####################################
### Financial literacy Heterogeneity
####################################
## Figure gender het - BART plot


set.seed(89)

# Data set up including calculating ability rank
df.b <- dfl

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("clicked", "treat.het",  "d.pb", "QSEC", "QEdu", "Qgender" , "financial_lit_fact" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$clicked) +rnorm(nrow(df.b), 0, 0.01)
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)

# FL 0 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 0 )/sum(CATE_df$financial_lit == 0)


# FL 1 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 1 )/sum(CATE_df$financial_lit == 1)

# FL2 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 2 )/sum(CATE_df$financial_lit == 2)

# FL 3 participant: prop. below 497 (mean)
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$financial_lit == 3 )/sum(CATE_df$financial_lit == 3)

# SEC level 1 participant: prop. below 497 (mean)


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=factor(financial_lit))) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train))) + 
  scale_fill_discrete(name = "", labels = c("No Fin. Lit.", "Low Fin. Lit", "Mid. Fin. Lit.", "High. Fin. Lit."))
#scale_fill_manual(name="Mode", values=colours) +
#modePlot

# Combine all plots into one chart
FL_het <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(FL_het, filename = "FL_het_clicked.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




################################
### Present Bias Heterogeneity  - Comp Questions
################################

set.seed(89)

# Data set up including DV
df.b <- dfl

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("comp", "treat.het",  "d.pb", "QSEC" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$comp) 
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=d.pb)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("High 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +


# Combine all plots into one chart
pb_het <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(pb_het, filename = "pb_het1_alltreats.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)





#######################################
### EDU Heterogeneity - Comp Questions
#######################################

set.seed(89)

# Data set up DV: Comp Questions
df.b <- dfl
table(df.b$QEdu)

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("comp", "treat.het",  "Qgender", "QSEC", "QEdu" )

df.b <- df.b[,vars]
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$comp) 
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train.mean[train$treat.het == 1] - bart.out$yhat.test.mean[test$treat.het == 0],
          bart.out$yhat.test.mean[test$treat.het == 1] - bart.out$yhat.train.mean[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))

# Descriptive results reported in main text:
mean(CATE_df$CATE)
summary(CATE_df$CATE)

# Proportion of CATEs that are negative:
sum(CATE_df$CATE < 0)/nrow(CATE_df)
sum(CATE_df$CATE < mean(CATE_df$CATE))/nrow(CATE_df)



# Educación basica
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QEdu == "Educación básica completa o educación media incompleta" )/sum(CATE_df$QEdu == "Educación básica completa o educación media incompleta")

# Media/Univ Incompleta
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QEdu == "Educación media completa o universitaria incompleta" )/sum(CATE_df$QEdu == "Educación media completa o universitaria incompleta")

# Universitaria
sum(CATE_df$CATE < mean(CATE_df$CATE) & CATE_df$QEdu == "Universitaria completa" )/sum(CATE_df$QEdu == "Universitaria completa")



# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 

modePlot <- ggplot(hist, aes(x=id, fill=QEdu)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train))) #+
#scale_fill_discrete(name = "", labels = c("SEC 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +


# Combine all plots into one chart
SEC_het <- ggarrange(effectsPlot, modePlot,
                     ncol = 1, nrow = 2, heights = c(2,2))

ggsave(SEC_het, filename = "Het_Comp_Edu.pdf", path=fig.path, device = "pdf", height = 8, width = 6, dpi = 300)




#############################################3
#### clicked heterogeneity
##############################################


#------------------------------------------------------



######################################
##### Heteroeneity in Click - General
######################################
set.seed(89)

###Heterogeneity var of interest
# Data set up including DV
df.b <- dfl

risk.vars<-c("control")
df.b$treat.het<-ifelse(df.b$treat %in% risk.vars, 0, 1)


#df.b$Gender <- ifelse(df.b$Qgender == "F",1,0)
df.b$treat <- as.factor(df.b$treat)
df.b$QSEC <- as.factor(df.b$QSEC)

# Define model variables incl. outcome as column 1
vars <- c("clicked", "treat.het", "QSEC", "risk_fact", "Qgender" , "financial_lit_fact", "d.pb")#,   

df.t <- df.b[,vars]

table(df.b$clicked, df.b$treat.het)
df.b <- df.b[complete.cases(df.b),]

# Separate outcome and training data
y <- as.numeric(df.b$clicked) 
train <- df.b[,-1]

# Gen. test data where those treated become untreated, for use in calculating ITT
test <- train
test$treat.het <- ifelse(test$treat.het == 1,0,ifelse(test$treat.het == 0,1,NA))

# Run BART for predicted values of observed and synthetic observations
bart.out <- bart(x.train = train, y.train = y, x.test = test)

# Recover CATE estimates and format into dataframe
# Logic: Take predictions for those actually treated and minus counterfactual
#        Then take counterfactually treated and deduct prediction for those actually in control
CATE <- c(bart.out$yhat.train[train$treat.het == 1] - bart.out$yhat.test[test$treat.het == 0],
          bart.out$yhat.test[test$treat.het == 1] - bart.out$yhat.train[train$treat.het == 0])

CATE_df <- data.frame(CATE = CATE)
covars <- rbind(train[train$treat.het == 1,c(2:ncol(train))], test[test$treat.het==1,c(2:ncol(train))])

CATE_df <- cbind(CATE_df,covars)
CATE_df <- CATE_df[order(CATE_df$CATE),]
CATE_df$id <- c(1:length(CATE))


# CATE Heterogeneity plot
hist <- CATE_df

effectsPlot <- ggplot(hist, aes(x=id, y = CATE)) +
  geom_line() +
  geom_hline(yintercept= 0, linetype="dashed", color="red") +
  geom_hline(yintercept = mean(hist$CATE), color = "blue") +
  labs(x="Individual",y = "CATE") +
  theme_minimal() +
  scale_x_continuous(limits = c(0,nrow(train)))
#ggsave(effectsPlot, filename= "test.pdf")
# Mode histogram 


#### Gender

modePlot <- ggplot(hist, aes(x=id, fill=Qgender)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("High 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +
modePlot

# Combine all plots into one chart
pb_het <- ggarrange(effectsPlot, modePlot,
                    ncol = 1, nrow = 2, heights = c(2,2))

ggsave(modePlot, filename =  "QGender_het_clicked.pdf", path=fig.path, device = "pdf", height = 7, width = 6, dpi = 300)

#### Risk

modePlot <- ggplot(hist, aes(x=id, fill=risk_fact)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("High 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +
modePlot

ggsave(modePlot, filename =  "Risk_het_clicked.pdf", path=fig.path, device = "pdf", height = 7, width = 6, dpi = 300)



#### Risk

modePlot <- ggplot(hist, aes(x=id, fill=financial_lit_fact)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("High 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +
modePlot

ggsave(modePlot, filename =  "FL_het_clicked.pdf", path=fig.path, device = "pdf", height = 7, width = 6, dpi = 300)

#### Present bias

modePlot <- ggplot(hist, aes(x=id, fill=d.pb)) +
  geom_histogram(binwidth = 60,position="stack") +
  theme(legend.position="bottom") +
  labs(y = "Count", x = "Individual")+
  scale_x_continuous(limits = c(0,nrow(train)))#+
#scale_fill_discrete(name = "", labels = c("High 1", "SEC 2", "SEC 3", "SEC 4"))
#scale_fill_manual(name="Mode", values=colours) +
modePlot

ggsave(modePlot, filename =  "PB_het_clicked.pdf", path=fig.path, device = "pdf", height = 7, width = 6, dpi = 300)

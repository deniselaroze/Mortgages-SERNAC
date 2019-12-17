###########################################
### Data analysis - Pilot Mortgages SERNAC
### Dec 6, 2019
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


rm(list=ls())

setwd("")
load("Data/Pilot/clean_data.Rdata")
fig.path<-"Figures"
colors<-c("blue",  "purple", "darkgreen", "orange", "red")




####################
## Descriptives
###################

prop.table(table(df$Qgender))
prop.table(table(df$age_cat))
mean(df$age)



prop.table(table(df$QEdu))

prop.table(table(df$financial_lit))


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


ggsave(paste0("tot_clicked_dfl_pilot", ".png"), path=fig.path,  width = 7, height = 4)



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


ggsave(paste0("var_clicked_dfl_pilot", ".png"), path=fig.path,  width = 7, height = 4)







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


ggsave(paste0("tot_c_opn_dfl_pilot", ".png"), path=fig.path,  width = 7, height = 4)



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


ggsave(paste0("conf_treat_df_pilot", ".png"), path=fig.path,  width = 7, height = 4)




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


ggsave(paste0("claro_treat_df_pilot", ".png"), path=fig.path,  width = 7, height = 4)



#####################################
### Effect comprehension by treatment
#####################################


df$comp.tot<-as.numeric(df$total_reward) - 3000 - as.numeric(df$risk_q_reward) - as.numeric(df$total_option)

summary(df$comp.tot)

table(df$comp.tot)
 df$comp.tot[df$comp.tot==1100]<-NA

plot.df <- summarySE(df, measurevar="comp.tot", groupvars=c("treat"), na.rm=T)


ggplot(plot.df, aes(x = factor(treat), y = comp.tot , fill=factor(treat))) + 
  geom_bar(position = position_dodge(width=0.3), stat="identity") +
  scale_fill_manual("", values=colors)+
  geom_errorbar(aes(ymin=(comp.tot-ci), ymax=(comp.tot+ci)), width=.3)+
  theme(legend.position = "none")+
  ylab("Ganancia preguntas comprensión") + xlab("") + ylim(0, 400)+
  theme(axis.text.x = element_text( color="black", 
                                    size=10),
        axis.text.y = element_text( color="black", 
                                    size=10, angle=0))+
  #scale_y_continuous(breaks = c(0,50,100,500,900,1200,1400,1500))+
  scale_x_discrete("",labels= c("control"="Control","treat1"="T. 1","treat2"="T. 2", "treat3"="T. 3","treat4"="T. 4"))


ggsave(paste0("tot_comp_treat_df_pilot", ".png"), path=fig.path,  width = 7, height = 4)


# 4/4 versus el resto

df$allright<-ifelse(df$comp.tot==400, 1, 0)

table(df$allright)
plot.df <- summarySE(df, measurevar="allright", groupvars=c("treat"), na.rm=T)






#############
### Balance
###############


table(df$treat1)



### wrong change gender to age
tbl<-ddply(df, .(age_cat, QSEC, treat) , summarise,
           #n.treat = length(unique(treat)),
           subj.n = length(unique(uid))
)
tbl
xt<-xtable(tbl)
print(xt, type="latex", file=("Tables/balance_numbers.tex"), floating=FALSE, include.rownames=FALSE)






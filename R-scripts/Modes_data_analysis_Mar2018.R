# author: Denise Laroze Feb 2017 (deniselaroze@gmail.com)


rm(list=ls())
#setwd("~/Dropbox/Ray_Projects/shared_folders/CESS_Aki/Lab_and_Online/APMM2015/Online_Data/R/")
#source("zTree.R")
library(foreign)
library(ggplot2)
theme_set(theme_bw())
library(stargazer)
#library(reshape2)
library(CBPS)
library(scales)
library(gridExtra)
library(effects)
library(plyr)
library(plm)
library(lmtest)
library(xtable)


##--------#---------#---------#---------#---------#---------#---------#---------
## set up the lab, online.mturk, online.uk comparison data
##--------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
#setwd("C:/Users/Denise Laroze Prehn/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment")
setwd("C:/Users/Denise Laroze P/Dropbox/CESS-Santiago/Archive/Modes/Tax Cheating Qualtrics Online Experiment")

mturk.ds<-read.csv("data/Mturk_DS_Sept2017.csv") #

cess.online.panel <- read.csv("data/CESS_Panel_DS_Feb2018.csv")
cess.online.panel<-cess.online.panel[cess.online.panel$correct>0, ]

cess.online.stgo <- read.csv("data/CESS_Panel_DS_Stgo_2017.csv")

lab.online.sync <- read.csv("data/lab_online_sync_edited.csv")


baseline.uk<-read.csv("data/baseline_uk.csv")
baseline.uk<-baseline.uk[baseline.uk$auditrate<30, ] # Only sessions with 0 and 20% audit


fig.path<-"R-Script/Figures"
v<-"Feb2018"




####################
### Data Management
####################

#Cleaning up round number in lab version of the experiment
baseline.uk$round<-ifelse(baseline.uk$auditrate>0, baseline.uk$period+10, baseline.uk$period )


# Eliminating missing values
lab.online.sync$DictGive[lab.online.sync$DictGive==-1]<-NA

# Editing Age from logical to numeric

mturk.ds$age2<-mturk.ds$age
mturk.ds$age2[mturk.ds$age=="false"]<-NA
mturk.ds$Age<-as.numeric(levels(mturk.ds$age2))[mturk.ds$age2]


cess.online.stgo$age2<-cess.online.stgo$age
cess.online.stgo$age2[cess.online.stgo$age=="false"]<-NA
cess.online.stgo$Age<-as.numeric(levels(cess.online.stgo$age2))[cess.online.stgo$age2]

# Indentifying and Eliminating non-consistent risk preferences
# Eliminating observations null risk and integrity ofvservations 
# caused by the way the online experiment is coded
cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA

consist.risk<-function(df,name.risk){
  col_idx <- grep(name.risk, names(df))
  m1 <- as.matrix(df[,col_idx])
  m1<-as.data.frame(m1)
  m1$Consistent<-NA
  m1$risk.pref.consist<-NA
  for(i in 1:nrow(m1)){
    a<-m1[i,]
    vt<-rep(NA, ncol(a) )
    for(j in 2:ncol(a)){
      vt[j-1]<-ifelse(a[j-1]>a[j], "inconsistent", "OK")
      df$Consistent[i]<-ifelse("inconsistent" %in% vt, "No" ,  "Yes") 
      df$risk.pref.consist[i]<-ifelse("inconsistent" %in% vt,  NA, df$risk.pref[i])
    }
  }
  return(df)
}

#online.uk<-consist.risk(online.uk, "Risk_")
lab.online.sync<-consist.risk(lab.online.sync, "risk")
mturk.ds<-consist.risk(mturk.ds, "risk")
cess.online.panel<-consist.risk(cess.online.panel, "risk")
cess.online.stgo<-consist.risk(cess.online.stgo, "risk")


#rename variables
names(baseline.uk)[names(baseline.uk) == 'dec1'] <- 'Risk_1'
names(baseline.uk)[names(baseline.uk) == 'dec2'] <- 'Risk_2'
names(baseline.uk)[names(baseline.uk) == 'dec3'] <- 'Risk_3'
names(baseline.uk)[names(baseline.uk) == 'dec4'] <- 'Risk_4'
names(baseline.uk)[names(baseline.uk) == 'dec5'] <- 'Risk_5'
names(baseline.uk)[names(baseline.uk) == 'dec6'] <- 'Risk_6'
names(baseline.uk)[names(baseline.uk) == 'dec7'] <- 'Risk_7'
names(baseline.uk)[names(baseline.uk) == 'dec8'] <- 'Risk_8'
names(baseline.uk)[names(baseline.uk) == 'dec9'] <- 'Risk_9'
names(baseline.uk)[names(baseline.uk) == 'dec10'] <- 'Risk_10'


baseline.uk$risk.pref<-10-baseline.uk$safechoices
baseline.uk<-consist.risk(baseline.uk, "Risk_")  

#### Eliminating risk inconsistent preferences
#lab.online.sync<-lab.online.sync[complete.cases(lab.online.sync$risk.pref.normal), ]
#baseline.uk<-baseline.uk[complete.cases(baseline.uk$risk.pref.normal), ]
#mturk.ds<-mturk.ds[complete.cases(mturk.ds$risk.pref.normal), ]
#cess.online.panel<-cess.online.panel[complete.cases(cess.online.panel$risk.pref.normal), ]
#cess.online.stgo<-cess.online.stgo[complete.cases(cess.online.stgo$risk.pref.normal), ]



###################################################################
### Eliminating observations with null risk and integrity scores
### caused by the way the online experiment is coded
##########################################################
#lab.online.sync<-lab.online.sync[complete.cases(lab.online.sync$risk.pref.normal), ]
#baseline.uk<-baseline.uk[complete.cases(baseline.uk$risk.pref.normal), ]
#mturk.ds<-mturk.ds[complete.cases(mturk.ds$risk.pref.normal), ]
#cess.online.panel<-cess.online.panel[complete.cases(cess.online.panel$risk.pref.normal), ]
#cess.online.stgo<-cess.online.stgo[complete.cases(cess.online.stgo$risk.pref.normal), ]

cess.online.panel$risk.pref[cess.online.panel$risk.pref==0]<-NA
mturk.ds$risk.pref[mturk.ds$risk.pref==0]<-NA
lab.online.sync$risk.pref[lab.online.sync$risk.pref==0]<-NA

cess.online.panel$total.integrity[cess.online.panel$total.integrity==0]<-NA
mturk.ds$total.integrity[mturk.ds$total.integrity==0]<-NA
lab.online.sync$total.integrity[lab.online.sync$total.integrity==0]<-NA


###############################
## change the unit of variables
###############################
mturk.ds$DictGive.normal <- mturk.ds$DictGive/1000
mturk.ds$total.integrity.normal <- (mturk.ds$total.integrity)/40
mturk.ds$risk.pref.normal <- (mturk.ds$risk.pref.consist)/10

lab.online.sync$DictGive.normal <- lab.online.sync$DictGive/1000
lab.online.sync$total.integrity.normal <- (lab.online.sync$total.integrity)/40
lab.online.sync$risk.pref.normal <- (lab.online.sync$risk.pref.consist)/10

cess.online.panel$DictGive.normal <- cess.online.panel$DictGive/1000
cess.online.panel$total.integrity.normal <- (cess.online.panel$total.integrity)/40
cess.online.panel$risk.pref.normal <- (cess.online.panel$risk.pref.consist)/10 

cess.online.stgo$DictGive.normal <- cess.online.stgo$DictGive/1000
cess.online.stgo$total.integrity.normal <- (cess.online.stgo$total.integrity)/40
cess.online.stgo$risk.pref.normal <- (cess.online.stgo$risk.pref.consist)/10


### adapting Baseline uk data
m1 <- as.matrix(baseline.uk[, c("publictransport","taxes", "drivingfast", "moneyfound",                 
                            "lying", "accidentaldamage", "litter",                     
                            "drivingalcohol", "jobapplication", "buyingstolen")])
class(m1)<-"numeric"


baseline.uk$total.integrity<-rowSums(m1, na.rm = T)

baseline.uk$DictGive.normal <- baseline.uk$offerdg/1000   
baseline.uk$risk.pref.normal <- baseline.uk$risk.pref.consist/10
baseline.uk$total.integrity.normal <- (baseline.uk$total.integrity)/40
baseline.uk$report.rate <- baseline.uk$declared/baseline.uk$profitret

baseline.uk$treat<-NA
baseline.uk$treat[baseline.uk$auditrate==0]<-2
baseline.uk$treat[baseline.uk$auditrate==20]<-1

#baseline.uk$taxBracket[baseline.uk$taxrate==10]<-3
#baseline.uk$taxBracket[baseline.uk$taxrate==20]<-2
#baseline.uk$taxBracket[baseline.uk$taxrate==30]<-1

baseline.uk$Gender_lab[baseline.uk$gender==0]<-"F"
baseline.uk$Gender_lab[baseline.uk$gender==1]<-"M"

### Number of correct responses
lab.online.sync$ncorrectret <- lab.online.sync$correct
mturk.ds$ncorrectret<-mturk.ds$correct
cess.online.panel$ncorrectret<-cess.online.panel$correct
cess.online.stgo$ncorrectret<-cess.online.stgo$correct


#################################
#### Figures - Prep
##################################
baseline.uk$muID<-paste0("baseline", baseline.uk$subj_id)
names(baseline.uk)[names(baseline.uk)=="age_subject"] <- "Age"
names(baseline.uk)[names(baseline.uk)=="safechoices"] <- "risk.pref"
names(baseline.uk)[names(baseline.uk)=="profitret"] <- "prelimGain"
names(baseline.uk)[names(baseline.uk)=="offerdg"] <- "DictGive"
names(baseline.uk)[names(baseline.uk)=="Gender_lab"] <- "Gender"

names(lab.online.sync)[names(lab.online.sync)=="age"] <- "Age"
names(lab.online.sync)[names(lab.online.sync)=="gender"] <- "Gender"
names(lab.online.sync)[names(lab.online.sync)=="taxRate"] <- "taxrate"

names(mturk.ds)[names(mturk.ds)=="gender"] <- "Gender"
names(mturk.ds)[names(mturk.ds)=="taxRate"] <- "taxrate"

names(cess.online.panel)[names(cess.online.panel)=="age"] <- "Age"
names(cess.online.panel)[names(cess.online.panel)=="gender"] <- "Gender"
names(cess.online.panel)[names(cess.online.panel)=="taxRate"] <- "taxrate"

names(cess.online.stgo)[names(cess.online.stgo)=="gender"] <- "Gender"
names(cess.online.stgo)[names(cess.online.stgo)=="taxRate"] <- "taxrate"



vars<-c( "muID", "ncorrectret" ,"Gender", "Age", "DictGive" ,"DictGive.normal", "total.integrity", "total.integrity.normal", 
         "risk.pref.normal", "risk.pref", "prelimGain", "report.rate", "treat", "taxrate", "round"
)
o.sync<-lab.online.sync[, vars]
o.sync$sample<-"Online Lab"
b.s<-baseline.uk[, vars]
b.s$sample<-"Lab"
mt.s<-mturk.ds[, vars]
mt.s$sample<-"Mturk"

cp.s<-cess.online.panel[, vars]
cp.s$sample<-"CESS Online UK"

#cps.s<-cess.online.stgo[, vars]
#cps.s$sample<-"CESS Online Stgo"

p.data<-rbind(o.sync, b.s, mt.s, cp.s)

rm(o.sync, b.s, mt.s, cp.s)


#p.data$study<-"BD"
#p.data$study[p.data$sample %in% c("Online Lab-DS", "Lab-DS", "Mturk-DS"  )]<-"DS"


## Only cases with rational risk preferences
#p.data<-p.data[complete.cases(p.data$risk.pref.normal), ]

## Unique Subject Ids
p.data$muID<-paste0(p.data$sample, p.data$muID)




##########
##################################################################
####                    Descriptive statistics
##################################################################
##########
###################################
### DS: Consistency in risk preferences
###################################
prop.table(table(baseline.uk$Consistent ))
prop.table(table(lab.online.sync$Consistent))
prop.table(table(mturk.ds$Consistent ))
prop.table(table(cess.online.panel$Consistent ))
#prop.table(table(cess.online.stgo$Consistent ))


################################
# DS: Treatments and subject summary
#################################
tbl<-ddply(p.data, ~ sample, summarize, 
      m.report = mean(report.rate, na.rm=T),
      subj.n = length(unique(muID))
)

names(tbl)<-c("Sample", "Mean Report Rate", "Number Subjects")
tbl<-xtable(tbl, caption="Treatment Summaries", label="table:sum")
print(tbl, type = getOption("xtable.type", "latex"), file = "R-Script/Tables/treatsum.tex")


##############################
# DS: Mean Give in dictator game
##############################

ddply(p.data, ~ sample, summarize, 
      m.report = mean(DictGive, na.rm=T)
)

lab<-c( baseline.uk$DictGive, lab.online.sync$DictGive)
others<-c(mturk.ds$DictGive, cess.online.panel$DictGive)

### Students v non-students
wilcox.test(lab, others,
            alternative = "two.sided",conf.level = 0.95)

t.test(lab, others, alternative ="two.sided", conf.level = 0.95)


### General comparisons
pairwise.t.test(p.data$DictGive, p.data$sample, # p.adjust.method = p.adjust.methods,
                pool.sd = FALSE, paired = F,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$DictGive, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


##########################
### DS: Premilinary Gains
##########################

ddply(p.data, ~ sample, summarize, 
      m.prelimgain = mean(prelimGain, na.rm=T),
      m.correct.ret= mean(ncorrectret, na.rm=T)
)

############################################################
####                         Data Analysis
############################################################

##############################
## Table 2 in the paper
##############################

#lab.online.sync$taxrate<-as.factor(lab.online.sync$taxrate)
#lab.online.sync$treat<-as.factor(lab.online.sync$treat)

#model.1.lab.online.sync <- ols(report.rate.lab.online.sync ~ ncorrectret + taxrate + treat,  x=T, y=T, data=lab.online.sync)
#fit1<-robcov(model.1.lab.online.sync, cluster=lab.online.sync$muID)

#bootcov(model.1.lab.online.sync,cluster=lab.online.sync$muID)

#### plm versions of the models
model.1.lab.online.sync <- plm(report.rate.lab.online.sync ~ ncorrectret + factor(taxrate) +factor(treat), data=lab.online.sync, index=c("muID","round"), model= "random")
model.1.lab.online.sync.pse<-coeftest(model.1.lab.online.sync, vcov=vcovHC(model.1.lab.online.sync, method="arellano"))

model.1.baseline <- plm(report.rate ~ ncorrectret+ factor(taxrate) +factor(treat), data=baseline.uk, index=c("muID","round"), model= "random")
model.1.baseline.pse<-coeftest(model.1.baseline, vcov=vcovHC(model.1.baseline, method="arellano"))

model.1.mt.ds <- plm(report.rate.mturk.ds ~ ncorrectret + factor(taxrate) +factor(treat), data=mturk.ds, index=c("muID","round"), model= "random")
model.1.mt.ds.pse<-coeftest(model.1.mt.ds, vcov=vcovHC(model.1.mt.ds, method="arellano"))

model.1.cp.ds <- plm(report.rate.cess.panel ~ ncorrectret + factor(taxrate) +factor(treat), data=cess.online.panel, index=c("muID","round"), model= "random")
model.1.cp.ds.pse<-coeftest(model.1.cp.ds, vcov=vcovHC(model.1.cp.ds, method="arellano"))

#### LM vresions of the models
#model.1.lab.online.sync <- lm(report.rate.lab.online.sync ~ ncorrectret + factor(taxrate) + factor(treat), data=lab.online.sync)
#model.1.baseline <- lm(report.rate ~ ncorrectret + factor(taxrate) +factor(treat),data=baseline.uk)
#model.1.mt.ds <- lm(report.rate.mturk.ds ~ ncorrectret + factor(taxrate) +factor(treat), data=mturk.ds)
#model.1.cp.ds <- lm(report.rate.cess.panel ~ ncorrectret + factor(taxrate) +factor(treat), data=cess.online.panel)
#model.1.cps.ds <- lm(report.rate.cess.stgo ~ ncorrectret + factor(taxrate) , data=cess.online.stgo)

models <- c( "Lab", "Online Lab", "Online UK",  "Mturk")
covariates <- c( "\\# of Additions" , "20\\% Tax","30\\% Tax","No Audit","Constant")
stargazer( model.1.baseline.pse, model.1.lab.online.sync.pse, model.1.cp.ds.pse, model.1.mt.ds.pse,
          out="R-script/Tables/table.4.4.tex",dep.var.labels = models,covariate.labels = covariates,
          add.lines=list(c("N Obs", nobs(model.1.baseline), nobs(model.1.lab.online.sync), nobs(model.1.cp.ds), nobs(model.1.mt.ds)))
          )

##############################
## Table 3 in the paper
##############################
#model.3.online.sync <- lm(report.rate.lab.online.sync ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=lab.online.sync)
#model.3.baseline <- lm(report.rate ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=baseline.uk)
#model.3.mt.ds <- lm(report.rate.mturk.ds~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal  ,data=mturk.ds)
#model.3.cp.ds <- lm(report.rate.cess.panel ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=cess.online.panel)



model.3.lab.online.sync <- plm(report.rate.lab.online.sync ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=lab.online.sync, index=c("muID","round"), model= "random")
model.3.lab.online.sync.pse<-coeftest(model.3.lab.online.sync, vcov=vcovHC(model.3.lab.online.sync, method="arellano"))

model.3.baseline <- plm(report.rate ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=baseline.uk, index=c("muID","round"), model= "random")
model.3.baseline.pse<-coeftest(model.3.baseline, vcov=vcovHC(model.3.baseline, method="arellano"))

model.3.mt.ds <- plm(report.rate.mturk.ds~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal  ,data=mturk.ds, index=c("muID","round"), model= "random")
model.3.mt.ds.pse<-coeftest(model.3.mt.ds, vcov=vcovHC(model.3.mt.ds, method="arellano"))

model.3.cp.ds <- plm(report.rate.cess.panel ~ ncorrectret + factor(taxrate) +factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,data=cess.online.panel, index=c("muID","round"), model= "random")
model.3.cp.ds.pse<-coeftest(model.3.cp.ds, vcov=vcovHC(model.3.cp.ds, method="arellano"))

models <- c( "Lab", "Online Lab", "Online UK", "Online Chile", "Mturk")
covariates <- c("\\# of Additions" , "20\\% Tax","30\\% Tax","No Audit","Dictator Game Giving","Integrity Score","Risk Preference",
                "Constant")
stargazer(model.3.baseline.pse, model.3.lab.online.sync.pse, model.3.cp.ds.pse, model.3.mt.ds.pse,
          out="R-script/Tables/table.5.4.tex",dep.var.labels = models,covariate.labels = covariates,
          add.lines=list(c("N Obs", nobs(model.3.baseline), nobs(model.3.lab.online.sync), nobs(model.3.cp.ds), nobs(model.3.mt.ds)))
)


  
# try CBPS (Imai and Ratkovic 2014)
# prepar the data for covariate balancing 
m.mat.online.sync <- model.3.lab.online.sync$model
m.mat.online.sync$type <- 1
m.mat.baseline <- model.3.baseline$model
m.mat.baseline$type <- 2
m.mat.mt.ds <- model.3.mt.ds$model
m.mat.mt.ds$type <- 3
m.mat.cp.ds <- model.3.cp.ds$model
m.mat.cp.ds$type <- 4
#m.mat.cps.ds <- model.3.cps.ds$model
#m.mat.cps.ds$type <- 5

#names(m.mat.lab) <- c("report.rate","ncorrectret","taxrate","treat","DictGive.normal","total.integrity.normal","risk.pref.normal","type")
nms<-c("report.rate","ncorrectret","taxrate","treat","DictGive.normal","total.integrity.normal","risk.pref.normal", "type")

names(m.mat.online.sync) <- nms
names(m.mat.baseline) <- nms
names(m.mat.mt.ds) <- nms
names(m.mat.cp.ds) <- nms
#names(m.mat.cps.ds) <- c("report.rate","ncorrectret","taxrate","DictGive.normal","total.integrity.normal","risk.pref.normal", "type")
#m.mat.cps.ds$treat<-2


m.mat <- rbind(m.mat.online.sync, m.mat.baseline, m.mat.mt.ds, m.mat.cp.ds)

# run CBPS
temp <- CBPS(type ~ ncorrectret + DictGive.normal+risk.pref.normal+factor(taxrate)+factor(treat)+risk.pref.normal ,data=m.mat)
# check the covariate balance after CBPS
#pdf("../../figs/covariate_balance.pdf",width=6,height=6)
plot(temp)
#dev.off()
summary(temp)

##############################
## Table 4 in the paper
##############################
m.mat$taxrate <- factor(m.mat$taxrate, levels = c("10", "20", "30"))

model.3.cbps.online.sync <- lm(report.rate ~ ncorrectret + factor(taxrate)+factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal ,
                                  data=m.mat[m.mat$type==1,],weights=temp$weights[m.mat$type==1])
model.3.cbps.baseline <- lm(report.rate ~ ncorrectret + factor(taxrate)+factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal ,
                               data=m.mat[m.mat$type==2,],weights=temp$weights[m.mat$type==2])
model.3.cbps.mt.ds <- lm(report.rate ~ ncorrectret + factor(taxrate)+factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal,
                            data=m.mat[m.mat$type==3,],weights=temp$weights[m.mat$type==3])
model.3.cbps.cp.ds <- lm(report.rate ~ ncorrectret + factor(taxrate)+factor(treat)+DictGive.normal+total.integrity.normal+risk.pref.normal ,
                         data=m.mat[m.mat$type==4,],weights=temp$weights[m.mat$type==4])

covariates <- c("\\# of Additions" , "20\\% Tax","30\\% Tax","No Audit","Dictator Game Giving","Integrity Score","Risk Preference"
                 ,"Constant")
stargazer(model.3.cbps.baseline,model.3.cbps.online.sync, model.3.cbps.cp.ds, model.3.cbps.mt.ds,
          out="R-script/Tables/table.6.4.tex",dep.var.labels = models,covariate.labels = covariates,
          keep.stat = c("n","adj.rsq"))

#######################
### Table 5 in paper
########################

# p.data$lab<-"Other"
# p.data$lab[ p.data$sample %in% c("Lab")]<-"Lab"
# p.data$lab <- factor(p.data$lab, levels = c("Lab", "Other"))
# p.data$student<-ifelse( p.data$sample %in% c("Lab", "Online Lab"), "Student", "Population")

p.data$sample <- factor(p.data$sample, levels = c("Lab", "Online Lab", "CESS Online UK", "Mturk" ))

model.4.audit <- plm(report.rate ~ ncorrectret + sample + DictGive.normal+total.integrity.normal+risk.pref.normal  + Age + Gender,data=p.data[p.data$treat==1,], index=c("muID","round"), model= "random")
model.4.audit.pse<-coeftest(model.4.audit, vcov=vcovHC(model.4.audit, method="arellano"))

model.4.no.audit <- plm(report.rate ~ ncorrectret + sample + DictGive.normal+total.integrity.normal+risk.pref.normal + Age + Gender,data=p.data[p.data$treat==2,], index=c("muID","round"), model= "random")
model.4.no.audit.pse<-coeftest(model.4.no.audit, vcov=vcovHC(model.4.no.audit, method="arellano"))



models<-c("Audit", "No Audit")
covariates <- c("\\# of Additions" ,"Online Lab", "CESS Online UK", "Mturk", "Dictator Game Giving",
                "Integrity Score","Risk Preference", "Age", "Male", "Constant")
stargazer(model.4.audit.pse, model.4.no.audit.pse,
          out="R-script/Tables/table.7.4.tex", 
          dep.var.labels = models, covariate.labels = covariates,
          add.lines=list(c("N Obs", nobs(model.4.audit), nobs(model.4.no.audit)))
)



###########################
### Mlogit models
############################




##############################
## Table 2 in the paper
##############################

library(mlogit)



cdata <- ddply(p.data[p.data$treat==1,], c("muID", "sample", "taxrate", "treat" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               mean.ncorrectret = mean(ncorrectret, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat"
               )


cdata2 <- ddply(p.data[p.data$treat==2,], c("muID", "sample", "taxrate", "treat" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               mean.ncorrectret = mean(ncorrectret, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat"
)

cdata<-rbind(cdata, cdata2)

cdata$cheat_pattern<- factor(cdata$cheat_pattern, levels = c("Always declare 100%", "Always declare 0%", "Always Cheat", "Sometimes Cheat"))
cdata$sample<- factor(cdata$sample, levels = c("Lab", "Online Lab", "CESS Online UK",  "Mturk"  ))


test <- multinom(cheat_pattern ~ sample +mean.ncorrectret +  factor(treat) + factor(taxrate), data = cdata)
summary(test)

covariates<-c("Online Lab", "CESS Online UK", "Mturk", "\\# of Additions" , "No Audit",  "20\\% Tax","30\\% Tax", "Constant")


stargazer(test, covariate.labels = covariates,
          out="R-script/Tables/table.mlogit.tex")


models <- c( "Lab", "Online Lab", "Online UK",  "Mturk")
covariates <- c( "\\# of Additions" , "20\\% Tax","30\\% Tax","No Audit","Constant")
stargazer( model.1.baseline.pse, model.1.lab.online.sync.pse, model.1.cp.ds.pse, model.1.mt.ds.pse,
           out="R-script/Tables/table.4.4.tex",dep.var.labels = models,covariate.labels = covariates,
           add.lines=list(c("N Obs", nobs(model.1.baseline), nobs(model.1.lab.online.sync), nobs(model.1.cp.ds), nobs(model.1.mt.ds)))
)







####
###############################################################
####                          Figures
###############################################################


colours<-c("Grey10", "Grey30", "Grey60" , "Grey80", "Grey90")

####################
###### Gender figure
#####################

plot.df<-p.data[,c("Gender", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
#plot.df$Gender2[plot.df$Gender==1]<-"Male"
#plot.df$Gender2[plot.df$Gender==2]<-"Female"
plot.df$Gender2[plot.df$Gender=="M"]<-"Male"
plot.df$Gender2[plot.df$Gender=="F"]<-"Female"

ggplot(plot.df, aes(x = Gender2, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("") + 
  scale_fill_manual("", values=colours) + 
  ylim(0, 100) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 


ggsave(paste0("comparative_gender", v, ".pdf"), path=fig.path,  width = 7, height = 4)


##########################
### Hist amount given DG
##########################

plot.df<-p.data[, c("DictGive", "sample")]

lab<-plot.df$DictGive[plot.df$sample=="Lab"]
online.lab<-plot.df$DictGive[plot.df$sample=="Online Lab"]
online.panel<-plot.df$DictGive[plot.df$sample=="CESS Online UK"]
Mturk<-plot.df$DictGive[plot.df$sample=="Mturk"]


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)


#plot.df$DictGive[plot.df$DictGive==-1]<-NA
plot.df$DictGive2<-ifelse(plot.df$DictGive==0, "0", 
                           ifelse(plot.df$DictGive>0 & plot.df$DictGive<500, ">0 & <500", 
                                   ifelse(plot.df$DictGive==500, "500",  ">500")))
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
plot.df$DictGive2 <- factor(plot.df$DictGive2, levels = c("0", ">0 & <500", "500",  ">500" ))

ggplot(plot.df, aes(x = DictGive2, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Offers in Dictator Game") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 100) + ylab("Percent")+  theme(legend.position="bottom") 
ggsave(paste0("comparative_hist_offers_DG", v, ".pdf"), path=fig.path,  width = 8, height = 5)








######################
### Density age
######################


pairwise.t.test(p.data$Age, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$Age, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)



lab<-p.data$Age[p.data$sample=="Lab"]
online.lab<-p.data$Age[p.data$sample=="Online Lab"]
online.panel<-p.data$Age[p.data$sample=="CESS Online UK"]
Mturk<-p.data$Age[p.data$sample=="Mturk"]


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)

ggplot(p.data, aes(x=Age)) + geom_density(aes(group=sample, fill=sample) , alpha=0.9)+
  scale_fill_manual("",  values=colours) + 
  xlab("Age") + ylab("Density") +  theme(legend.position="bottom")

ggsave(paste0("comparative_density_age", v, ".pdf"), path=fig.path,  width = 7, height = 4)


######################
### Density Integrity
######################

ddply(p.data, ~ sample, summarize, 
      m.report = mean(total.integrity.normal, na.rm=T)
)


lab<-p.data$total.integrity.normal[p.data$sample=="Lab"]
online.lab<-p.data$total.integrity.normal[p.data$sample=="Online Lab"]
online.panel<-p.data$total.integrity.normal[p.data$sample=="CESS Online UK"]
Mturk<-p.data$total.integrity.normal[p.data$sample=="Mturk"]
student<-c(lab, online.lab)
non.student<-c(Mturk, online.panel)


ks.test(lab, online.lab ,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(online.panel, Mturk,
        alternative = c("two.sided"),
        exact = NULL)

ks.test(student, non.student,
        alternative = c("two.sided"),
        exact = NULL)


pairwise.t.test(p.data$total.integrity.normal, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$total.integrity.normal, p.data$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)



ggplot(p.data, aes(x=total.integrity.normal)) + geom_density(aes(group=sample, fill=sample), alpha=0.85)+
  scale_fill_manual("",  values=colours) + 
  xlab("Integrity Score") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 
ggsave(paste0("comparative_density_integrity", v, ".pdf"), path=fig.path,  width = 7, height = 4)






##########################
### Hist Risk Preferences
##########################
plot.df<-p.data[, c("risk.pref.normal", "sample")]
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)

ggplot(plot.df, aes(x = risk.pref.normal, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Risk Preferences") + 
  scale_fill_manual("",  values=colours) + 
  ylim(0, 40) + ylab("Percent")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_hist_safe_choices", v, ".pdf"), path=fig.path,  width = 7, height = 7)



### T-test differences
plot.df<-p.data[, c("risk.pref.normal", "sample")]
pairwise.t.test(plot.df$risk.pref.normal, plot.df$sample, p.adjust.method = p.adjust.methods,
                paired = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(plot.df$risk.pref.normal, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)




#########################
### Density Prelim Gains
#########################

ggplot(p.data, aes(x=prelimGain)) + geom_density(aes(group=sample, fill=sample), alpha=0.7)+
  scale_fill_manual("",  values=colours) + 
  xlab("Preliminary Gains") + ylab("Density")+
  theme(legend.position="bottom", axis.text=element_text(size=10)) 

ggsave(paste0("comparative_density_prelimGain", v, ".pdf"), path=fig.path,  width = 7, height = 4)


pairwise.t.test(p.data$prelimGain, p.data$sample, p.adjust.method = p.adjust.methods,
                pool.sd = FALSE,
                alternative = c("two.sided"))

pairwise.wilcox.test(p.data$prelimGain, plot.df$sample, p.adjust.method = p.adjust.methods,
                     paired = FALSE)


#############################
### Hist Report Rate by audit
##############################

######
p.data$report.rate.r<-round(p.data$report.rate, 1)

plot.df<-p.data[ p.data$treat==1 , c("report.rate.r", "sample")] # With Audit
tbl<-prop.table(table(plot.df),2)

plot.df<-as.data.frame(tbl)
plot.df$treat<-"With Audit"

plot.df2<-p.data[ p.data$treat==2 , c("report.rate.r", "sample")] # Without Audit
tbl<-prop.table(table(plot.df2),2)

plot.df2<-as.data.frame(tbl)
plot.df2$treat<-"Without Audit"

plot.df<-rbind(plot.df, plot.df2)
rm(plot.df2)

p1<-ggplot(plot.df, aes(x = report.rate.r, y = Freq*100, fill=sample))+
  geom_bar(position="dodge",stat="identity") + xlab("Report Rate") + 
  scale_fill_manual("", values=colours) + 
  ylim(0, 80) + ylab("Percent")+
  facet_wrap( ~  treat ) + theme(legend.position="bottom")
p1

ggsave(paste0("comparative_hist_report_rate", v, ".pdf"), path=fig.path,  width = 12, height = 8)


#################################### 
### Cheat types by sample and audit
####################################

# With Audit
cdata <- ddply(p.data[p.data$treat==1,], c("muID", "sample" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat")

plot.data<-prop.table(table(cdata$sample, cdata$cheat_pattern), 1)

plot.data<-as.data.frame(plot.data)
names(plot.data)[names(plot.data) == "Var1"] <- "Sample"
names(plot.data)[names(plot.data) == "Var2"] <- "cheat_type"

plot.data$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))
plot.data$treat<-"With Audit"

#Without audit

cdata <- ddply(p.data[p.data$treat==2,], c("muID", "sample" ), summarise,
               mean.report.rate = mean(report.rate, na.rm=T),
               cheat_pattern = if (mean.report.rate=="0") "Always declare 0%" 
               else if (mean.report.rate=="1") "Always declare 100%" 
               else if ("1" %in% report.rate) "Sometimes Cheat" else "Always Cheat")

plot.data2<-prop.table(table(cdata$sample, cdata$cheat_pattern), 1)

plot.data2<-as.data.frame(plot.data2)
names(plot.data2)[names(plot.data2) == "Var1"] <- "Sample"
names(plot.data2)[names(plot.data2) == "Var2"] <- "cheat_type"

plot.data2$cheat_type <- factor(plot.data$cheat_type, levels = c("Always declare 0%", "Always Cheat", "Sometimes Cheat" , "Always declare 100%"))
plot.data2$treat<-"Without Audit"

plot.data<-rbind(plot.data, plot.data2)
rm(plot.data2)

ggplot(plot.data, aes(x = Sample, y = Freq, fill = cheat_type)) + geom_bar(stat = "identity", position = "dodge") + 
  ylab("Percent") + scale_y_continuous(labels = scales::percent, limits=c(0,1)) +
  scale_fill_manual(values = c("grey10","grey30", "grey50", "grey80"),guide = guide_legend(title = "")) +
  xlab("") + theme(legend.position="bottom", axis.text=element_text(size=10, angle = 45, hjust = 1) ) +
  facet_wrap( ~  treat)

ggsave(paste0("comparative_4types_cheaters", v, ".pdf"), path=fig.path,  width = 10, height = 6)










# Dataset: Employee Engagement Survey 2010

# Program Flow:
# 1. Loading dataset and required libraries
# 2. Cleaning and subsetting dataset
# 3. Creating Indexes
# 4. Univariable Analysis
# 5. Bivariate Analysis
# 6. Hypothesis Testing

# 1. Loading the dataset and required libraries
eei <- read.csv(file.choose(), stringsAsFactors = T, header = T, na.strings = c(""," ","NA"))
library(plotrix)
library(psych)
library(ggplot2)
library(gplots)
library(dplyr)

# 2. a) Subsetting the required variables from the top 5 federal agencies on the basis of number of employees 
eei_subset1 <- eei[,c("AGENCY","DLOC","DSUPER","DSEX","DAGEGRP","DLEAVING","Q3","Q4","Q5","Q6","Q11","Q13","Q47","Q48","Q53","Q56","Q63","Q67","Q69","Q70")]
eei_subset1 <- subset(eei_subset1,AGENCY=="AR"|AGENCY=="DJ"|AGENCY=="TR"|AGENCY=="CM"|AGENCY=="AG")

# b) Data cleaning
eei_subset1[eei_subset1=="X"]<-NA
eei_subset1<-na.omit(eei_subset1)

# c) Converting variable data types and cleaning messy data labels
eei_subset1$Q47<- as.integer(eei_subset1$Q47)
eei_subset1$Q11<- as.integer(eei_subset1$Q11)
eei_subset1$Q13<- as.integer(eei_subset1$Q13)
eei_subset1$Q53<- as.integer(eei_subset1$Q53)
eei_subset1$Q56<- as.integer(eei_subset1$Q56)

eei_subset1$DLOC <- gsub("A","HQ",eei_subset1$DLOC)
eei_subset1$DLOC <- gsub("B","F",eei_subset1$DLOC)
eei_subset1$DLOC<- as.factor(eei_subset1$DLOC)

eei_subset1$DSEX <- gsub("A","M",eei_subset1$DSEX)
eei_subset1$DSEX <- gsub("B","F",eei_subset1$DSEX)
eei_subset1$DSEX <- as.factor(eei_subset1$DSEX)

eei_subset1$DSUPER <- gsub("A","Non-Supervior/Team-Leader",eei_subset1$DSUPER)
eei_subset1$DSUPER <- gsub("B","Supervisor",eei_subset1$DSUPER)
eei_subset1$DSUPER <- gsub("C","Manager/Executive",eei_subset1$DSUPER)
eei_subset1$DSUPER <- as.factor(eei_subset1$DSUPER)

eei_subset1$DAGEGRP <- gsub("B","29 and Under",eei_subset1$DAGEGRP)
eei_subset1$DAGEGRP <- gsub("C","30-39",eei_subset1$DAGEGRP)
eei_subset1$DAGEGRP <- gsub("D","40-49",eei_subset1$DAGEGRP)
eei_subset1$DAGEGRP <- gsub("E","50-59",eei_subset1$DAGEGRP)
eei_subset1$DAGEGRP <- gsub("F","60 or Above",eei_subset1$DAGEGRP)
eei_subset1$DAGEGRP <- as.factor(eei_subset1$DAGEGRP)

eei_subset1$DLEAVING <- gsub("A","N",eei_subset1$DLEAVING)
eei_subset1$DLEAVING <- gsub("B","R",eei_subset1$DLEAVING)
eei_subset1$DLEAVING <- gsub("C","Y",eei_subset1$DLEAVING)
eei_subset1$DLEAVING <- gsub("D","Y",eei_subset1$DLEAVING)
eei_subset1$DLEAVING <- gsub("E","Y",eei_subset1$DLEAVING)
eei_subset1$DLEAVING <- as.factor(eei_subset1$DLEAVING)

# d) Dropping additional unused levels and re-indexing
eei_subset1 <- droplevels(eei_subset1)
rownames(eei_subset1) <- 1:nrow(eei_subset1)
str(eei_subset1)
dim(eei_subset1)
summary(eei_subset1)

# 3. Creating new indexes, EEI and JSI, varying from 0.0 to 1.0
attach(eei_subset1)
eei_subset1$EEI<- round((Q3+Q4+Q6+Q11+Q47+Q48+Q53+Q56)/40,2) 
eei_subset1$JSI<- round((Q4+Q5+Q13+Q63+Q67+Q69+Q70)/35,2)

# 4. Univariate Analysis
# a) Control Variable: Agency (AGENCY)
dev.off()

summary(eei_subset1$AGENCY)

percentlabelAGENCY<- round(100*table(AGENCY)/sum(table(AGENCY)), 1)
pie3D(table(AGENCY), labels = percentlabelAGENCY, explode = 0.1, main = " % Distribution by Top 5 Agencies")
legend("topright", cex = 0.70, legend = c("Army", "Agriculture","Commerce","Justice","Treasury"), 
       fill = c("yellow", "red","green","blue","purple"))	

# b) Control Variable: Location (DLOC)
dev.off()

summary(eei_subset1$DLOC)

barplotLOC<- barplot(round(prop.table(table(eei_subset1$DLOC))*100,1), main = "% Distribution by Location ", horiz = TRUE, col=c("orange","grey"),xlim=c(0,60))
text(0,barplotLOC,round(prop.table(table(eei_subset1$DLOC))*100,1),cex=1,pos=4) 
legend("topright", legend = c("Field","Headquaters"), 
       fill = c("grey","orange"), cex = 0.75)	
box()

# c) Control Variable: Supervisory Status (DSUP)
dev.off()

summary(eei_subset1$DSUPER)

barplotSUP<-barplot(round(100*prop.table(table(eei_subset1$DSUPER)),1), main = "% Distribution by Supervisory Status ", horiz = TRUE, col=c("lightgreen","orange","steelblue"),xlim=c(0,75))
text(0,barplotSUP,round(100*prop.table(table(eei_subset1$DSUPER)),1),cex=1,pos=4) 
legend("topright", legend = c("Supervisor","Non-Supervisor/Team-Leader","Manager/Executive"), 
       fill = c("steelblue","orange","lightgreen"), cex = 0.75)	
box()

# d) Control Variable: Sex (DSEX)
dev.off()

summary(eei_subset1$DSEX)

barplotSEX<-barplot(round(100*prop.table(table(eei_subset1$DSEX)),1), main = "% Distribution by Sex", col=c("orange","green"),ylim=c(0,60))
text(barplotSEX,5,round(100*prop.table(table(eei_subset1$DSEX)),1),cex=1,pos=3) 
legend("topright", legend = c("Female","Male"), 
       fill = c("orange","green"), cex = 0.75)	
box()

# e) Control Variable: Leaving Status (DLEAVING)
dev.off()

summary(eei_subset1$DLEAVING)

barplotLEAVING<-barplot(round(100*prop.table(table(eei_subset1$DLEAVING)),1), main = "% Distribution by Leaving Status", col=c("yellow","lightgreen","steelblue"),ylim=c(0,80))
text(barplotLEAVING,5, round(100*prop.table(table(eei_subset1$DLEAVING)),1),cex=1,pos=3) 
legend("topright", legend = c("No","Retire","Yes"), 
       fill = c("yellow","lightgreen","steelblue"), cex = 0.75)	
box()

# f) Control Variable: Age (DAGE)
dev.off()

summary(eei_subset1$DAGEGRP)

barplotAGE<-barplot(table(eei_subset1$DAGEGRP), main = "Distribution by Age Group", col=c("orange","grey","lightgreen","steelblue","red"),ylim=c(0,30000))
text(barplotAGE,5000,table(eei_subset1$DAGEGRP),cex=1,pos=3)
legend("topright", legend = c("<29","30-39","40-49","50-59",">60"), 
       fill = c("orange","grey","lightgreen","steelblue","red"), cex = 0.75)	
box()

percentlabelAGE<- round(100*table(DAGEGRP)/sum(table(DAGEGRP)), 1)
pie3D(table(DAGEGRP), labels = percentlabelAGE, explode = 0.1, main = " % Distribution by Age Group")
legend("topright", legend = c("29 and Under", "30-39","40-49","50-59","60 or Above"), 
       fill = c("yellow", "red","green","blue","purple"), cex = 0.65)	

# g) Dependent Index: Job Satisfaction Index (JSI)
dev.off()
par(mfrow=c(2,1))

summary(eei_subset1$JSI)
attach(eei_subset1)
hist(JSI, col=c("orange"), freq = F)
lines(density(JSI),col=c("blue"),lwd =3.0)
box()

boxplot(JSI, horizontal = TRUE,col=c("orange"), main="Boxplot of JSI")
text(x=fivenum(JSI), labels =fivenum(JSI), y=1.35)

# Squaring variable JSI to reduce left skewness in the distribution
 

qplot(JSI^2, data=eei_subset1, geom="density",fill=DLEAVING, alpha=I(.5), 
      main="Distribution of Squared JSI", xlab="Squared JSI", 
      ylab="Density")

describe(JSI)

# h) Dependent Index: Employee Engagement Index (EEI)
dev.off()
par(mfrow=c(2,1))

summary(eei_subset1$EEI)

hist(EEI, col=c("steelblue"),freq=F, ylim = c(0,4))
lines(density(EEI),col=c("red"),lwd =3.0)
box()

boxplot(EEI, horizontal = TRUE,col=c("steelblue"), main="Boxplot of EEI")
text(x=fivenum(EEI), labels =fivenum(EEI), y=1.35)

# Squaring variable EEI to reduce left skewness in the distribution

qplot(EEI^2, data=eei_subset1, geom="density",fill=DLEAVING, alpha=I(.5), 
      main="Distribution of Squared EEI", xlab="Squared EEI", 
      ylab="Density")

describe(EEI)

# 5. Bivariate Analysis

# a) EEI and Location
dev.off()
install.packages("ggplot2")
library(ggplot2)
qplot(EEI,data=eei_subset1, facets = . ~eei_subset1$DLOC,colour = factor(DLOC))
# Distribution of EEI by Location

# The median EEI of Field is almost same as Headquarter.

qplot(DLOC,EEI, data=eei_subset1, geom=c("boxplot","jitter"), 
      fill=DLOC, main="EEI by LOCATION",
      xlab="", ylab="EEI")


qplot(DLOC,EEI, data=eei_subset1, geom=c("boxplot"), 
      fill=DLOC, main="EEI by Location",
      xlab="", ylab="EEI")

library(gplots)
plotmeans(eei_subset1$EEI~eei_subset1$DLOC, xlab="Location", ylab="EEI Index", lwd=3, col="red", p=0.99)
library(psych)
describeBy(eei_subset1$EEI, eei_subset1$DLOC)
# b) EEI and AGEGRP
dev.off()

qplot(EEI,data=eei_subset1, facets = . ~eei_subset1$DAGEGRP,colour = factor(DAGEGRP))
# Distribution of EEI by Age

# The median EEI is similar between different age groups. Slightly higher for 60 or Above
qplot(DAGEGRP,EEI, data=eei_subset1, geom=c("boxplot"), 
      fill=DAGEGRP, main="EEI by Age",
      xlab="", ylab="EEI")

plotmeans(eei_subset1$EEI~eei_subset1$DAGEGRP, xlab="AGE GROUP", ylab="EEI Index", lwd=3, col="red", p=0.99)

# c) EEI and SEX
dev.off()

qplot(EEI,data=eei_subset1, facets = . ~eei_subset1$DSEX,colour = factor(DSEX))
# Distribution of EEI by Sex

# The median EEI is the same for males and females in the top 5 federal agencies
qplot(DSEX,EEI, data=eei_subset1, geom=c("boxplot"), 
      fill=DSEX, main="EEI by Sex",
      xlab="", ylab="EEI")

plotmeans(eei_subset1$EEI~eei_subset1$DSEX, xlab="SEX", ylab="EEI Index", lwd=3, col="red", p=0.99)


# d) EEI and SUPERVISORY STATUS

qplot(EEI,data=eei_subset1, facets = . ~eei_subset1$DSUPER,colour = factor(DSUPER))
# Distribution of EEI by Supervisory status

# The median EEI for Manager level is slightly higher than Non-Supervisor level and Supervisor level
qplot(DSUPER,EEI, data=eei_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="EEI by Supervisory status",
      xlab="", ylab="EEI")

plotmeans(eei_subset1$EEI~eei_subset1$DSUPER, xlab="SUPERVISORY STATUS", ylab="EEI Index", lwd=3, col="red", p=0.99)

# e) EEI and LEAVING STATUS

qplot(EEI,data=eei_subset1, facets = . ~eei_subset1$DLEAVING,colour = factor(DLEAVING))
# Distribution of EEI by Leaving status

# The median EEI value for employees not leaving or retiring from their organisations is higher than employees leaving as expected. 
qplot(DLEAVING,EEI, data=eei_subset1, geom=c("boxplot"), 
      fill=DLEAVING, main="EEI by Leaving Status",
      xlab="", ylab="EEI")

plotmeans(eei_subset1$EEI~eei_subset1$DLEAVING, xlab="LEAVING STATUS", ylab="EEI Index", lwd=3, col="red", p=0.99)

# f) JSI and AGENCY
dev.off()

qplot(JSI,data=eei_subset1, facets = . ~eei_subset1$AGENCY,colour = factor(AGENCY))
# Distribution of JSI by Agency

# The median JSI is almost similar lying between the range 0.6 - 0.65 between the top 5 agencies

qplot(AGENCY,JSI, data=eei_subset1, geom=c("boxplot","jitter"), 
      fill=AGENCY, main="JSI by Agency",
      xlab="", ylab="JSI")

qplot(AGENCY,JSI, data=eei_subset1, geom=c("boxplot"), 
      fill=AGENCY, main="JSI by Agency",
      xlab="", ylab="JSI")

plotmeans(eei_subset1$JSI~eei_subset1$AGENCY, xlab="AGENCY", ylab="JSI Index", lwd=3, col="red", p=0.99)


# g) JSI and AGEGRP
dev.off()

qplot(JSI,data=eei_subset1, facets = . ~eei_subset1$DAGEGRP,colour = factor(DAGEGRP))
# Distribution of JSI by Age Group

# The median JSI is similar between different age groups. Slightly lower for 29 and Under and 30 - 39 groups in comparison with other groups

qplot(DAGEGRP,JSI, data=eei_subset1, geom=c("boxplot"), 
      fill=DAGEGRP, main="JSI by Age",
      xlab="", ylab="JSI")

plotmeans(eei_subset1$JSI~eei_subset1$DAGEGRP, xlab="AGE GROUP", ylab="JSI Index", lwd=3, col="red", p=0.99)


# h) JSI and SEX
dev.off()

qplot(JSI,data=eei_subset1, facets = . ~eei_subset1$DSEX,colour = factor(DSEX))
# Distribution of JSI by Sex

# The median JSI is slighly higher for males in comparison to females in the top 5 federal agencies
qplot(DSEX,JSI, data=eei_subset1, geom=c("boxplot"), 
      fill=DSEX, main="JSI by Sex",
      xlab="", ylab="JSI")

plotmeans(eei_subset1$JSI~eei_subset1$DSEX, xlab="SEX", ylab="JSI Index", lwd=3, col="red", p=0.99)


# i) JSI and SUPERVISORY STATUS

qplot(JSI,data=eei_subset1, facets = . ~eei_subset1$DSUPER,colour = factor(DSUPER))
# Distribution of JSI by Supervisory Status

# The median JSI for Manager level is slightly higher than Non-Supervisor level and Supervisor level. The pattern is similar to the bivariable relation between EEI and Supervisory Status.
qplot(DSUPER,JSI, data=eei_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="JSI by Supervisory status",
      xlab="", ylab="JSI")

plotmeans(eei_subset1$JSI~eei_subset1$DSUPER, xlab="SUPERVISORY STATUS", ylab="JSI Index", lwd=3, col="red", p=0.99)


# j) JSI and LEAVING STATUS

qplot(JSI,data=eei_subset1, facets = . ~eei_subset1$DLEAVING,colour = factor(DLEAVING))
# Distribution of JSI by Leaving status

# The median JSI value for employees not leaving or retiring from their organisations is higher than employees leaving as expected. The pattern is similar to the bivariable relation between JSI and Supervisory Status.
qplot(DLEAVING,JSI, data=eei_subset1, geom=c("boxplot"), 
      fill=DLEAVING, main="JSI by Leaving Status",
      xlab="", ylab="JSI")

plotmeans(eei_subset1$JSI~eei_subset1$DLEAVING, xlab="LEAVING STATUS", ylab="JSI Index", lwd=3, col="red", p=0.99)


#k) JSI and EEI in relation with Q70

# High correlation between JSI and EEI when satisfaction with pay is high. 
qplot(JSI, EEI, data=eei_subset1, geom=c( "smooth","point"),
      color=Q70, 
      main="JSI and EEI by Pay Satisfaction levels", 
      xlab="JSI", ylab="EEI")

# l)  Agency and Q5 in relation with Supervisory Status

# The Likeness to work is similar and high between the top 5 agencies in various supervisory statues.
qplot(AGENCY,Q5, data=eei_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="Agency and likeness to work with Supervisory Status",
      xlab="AGENCY", ylab="Q5")

# n) Location and Q69 in relation with Supervisory Status

# The Job Satisfaction levels is similar and high between the locations in various supervisory statues.
qplot(DLOC,Q69, data=eei_subset1, geom=c("boxplot"), 
      fill=DSUPER, main="Location and Job Satisfaction levels with Supervisory Status",
      xlab="LOCATION", ylab="Q69")
# ------------------------------------------------------- #

# 6. Hypothesis Testing

# a) Positive relation between JSI and EEI
attach(eei_subset1)
cor(JSI,EEI)
# The correlation between JSI and EEI is 0.848 indicating a high positive relation between the two indexes.

remod <- lm(EEI ~ JSI, data = eei_subset1)
summary(remod)
# Adjusted R-squared:  0.7201 and p-value: < 2.2e-16

# b) There is a difference in the average EEI between Males and Females

# Null Hypothesis: There is no difference in the average EEI between Males and Females
# Alternative Hypothesis: There is a difference in the average EEI between Males and Females

# Since the distribution is not normal, therefore wilcox test is better suited to test the hypothesis
wilcox.test(eei_subset1$EEI[eei_subset1$DSEX=="M"],eei_subset1$EEI[eei_subset1$DSEX=="F"], alternative="two.sided") 
# p-value = 0.0069 implies that the null hypothsis can be rejected at 99% confidence

# c) There is a difference in the average EEI between employees of different age groups
# Null Hypothesis: There is no difference in the average EEI between different age groups
# Alternative Hypothesis: There is a difference in the average EEI between different age groups

eei_subset1 %>% group_by(DAGEGRP) %>% summarize(avg = mean(EEI), std = sd(EEI), med = median(EEI)) 

boxplot(EEI~DAGEGRP, data=eei_subset1, col=2:6, xlab="EEI Index")
library(gplots)
plotmeans(eei_subset1$EEI~eei_subset1$DAGEGRP, xlab="EEI Index", ylab="AGE GROUP", lwd=3, col="red", p=0.99)

# ANOVA Test
eeiage.aov <- aov(EEI~DAGEGRP, data=eei_subset1)
eeiage.aov
summary(eeiage.aov)
# p-value = 7.23e-1 implies that the null hypothsis can be rejected at 99% confidence

# Tukey Test
eeiage.tk <- TukeyHSD(eeiage.aov)
round(eeiage.tk$DAGEGRP,2)
# The difference in mean values of EEI is mainly between the following groups:
#   i) 30-39 & 29 and Under
#   ii) 40-49 & 29 and Under
#   iii) 50-59 & 29 and Under 
#   iv) 40-49 & 30-39 
#   v) 50-59 & 30-39 
#   vi) 50-59 & 40-49 

# d) There is a difference in the average EEI of employees located in Field and HQ

# Null Hypothesis: There is no difference in the average EEI of employees located in Field and HQ
# Alternative Hypothesis: There is a difference in the average EEI of  located in Field and HQ

# Since the distribution is not normal, therefore wilcox test is better suited to test the hypothesis
wilcox.test(eei_subset1$EEI[eei_subset1$DLOC=="F"],eei_subset1$EEI[eei_subset1$DLOC=="HQ"], alternative="two.sided") 
# p-value = 2.924e-15 implies that the null hypothsis can be rejected at 99% confidence

# e) There is a difference in the average EEI between employees of different supervisory statuses
# Null Hypothesis: There is no difference in the average EEI between different supervisory statuses
# Alternative Hypothesis: There is a difference in the average EEI between different supervisory statuses

eei_subset1 %>% group_by(DSUPER) %>% summarize(avg = mean(EEI), std = sd(EEI), med = median(EEI)) 

boxplot(EEI~DSUPER, data=eei_subset1, col=2:5, xlab="EEI Index")
plotmeans(eei_subset1$EEI~eei_subset1$DSUPER, xlab="EEI Index", ylab="Supervisory Status", lwd=3, col="red", p=0.99)

# ANOVA Test
eeisuper.aov <- aov(EEI~DSUPER, data=eei_subset1)
eeisuper.aov
summary(eeisuper.aov)
# p-value = 2e-16 implies that the null hypothsis can be rejected at 99% confidence

# Tukey Test
eeisuper.tk <- TukeyHSD(eeisuper.aov)
eeisuper.tk
# The difference in mean values of EEI between different supervisory statuses

# f) There is a difference in the average EEI between employees of different leaving statuses
# Null Hypothesis: There is no difference in the average EEI between different leaving statuses
# Alternative Hypothesis: There is a difference in the average EEI between different leaving statuses
library(dplyr)
eei_subset1 %>% group_by(DLEAVING) %>% summarize(avg = mean(EEI), std = sd(EEI), med = median(EEI)) 

boxplot(EEI~DLEAVING, data=eei_subset1, col=2:5, xlab="EEI Index")
plotmeans(eei_subset1$EEI~eei_subset1$DLEAVING, xlab="EEI Index", ylab="Leaving Status", lwd=3, col="red", p=0.99)

# ANOVA Test
eeileaving.aov <- aov(EEI~DLEAVING, data=eei_subset1)
eeileaving.aov
summary(eeileaving.aov)
# p-value = 2e-16 implies that the null hypothsis can be rejected at 99% confidence

# Tukey Test
eeileaving.tk <- TukeyHSD(eeileaving.aov)
eeileaving.tk
# The difference in mean values of EEI between different leaving statuses
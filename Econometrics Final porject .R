getwd()
setwd("~/Desktop/Econometrics Project")
library(ipumsr)
require(stargazer)
require(AER)
library(dplyr)
library(tidyr)
library(ggplot2)

library(ggpubr)
library(permute)
library(lattice)
library(vegan)
library(nlme)
library(ordinal)


ddi <- read_ipums_ddi("nhis_00004.xml")
data <- read_ipums_micro(ddi)
str(data)
View(data)
summary(data)
colnames(data)
nrow(data)


##################
# Picking Dataset
##################


#data$USBORN <- as.factor(data$USBORN)
levels(data$USBORN) <- c("Yes", "NO")


##data$FAMTOTINC<-as.factor(data$FAMTOTINC)
levels(data$FAMTOTINC)<-c()


data$SEX <- as.factor(data$SEX)
levels(data$SEX) <- c("Male","Female","Refused","dont know")

data$SEXORIEN <- as.factor(data$SEXORIEN)
levels(data$SEXORIEN) <- c("NIU","Lesbian or gay","straight","bisexual","something else","dont know","refused","NA")

data$REGION <- as.factor(data$REGION)
levels(data$REGION) <- c("Northeast","Midwest","South","West")

data$MARST <- as.factor(data$MARST)
levels(data$MARST) <- c("NIU","Married","Married spouse not there","Married spouse NA","Widowed","Divorced","Separated","never married","unknown")

data$RACENEW <- as.factor(data$RACENEW)
levels(data$RACENEW) <- c("white","Black","Aleut Alaskan","American Indian","Asian","Other","refused","not ascertained","unknown")


data$EMPFT <- as.factor(data$EMPFT)
levels(data$EMPFT) <- c("NIU","parttime","fulltime","refused","NA","dont know")

data$EDUC <- as.factor(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")


data$CITIZEN <- as.factor(data$CITIZEN)
levels(data$CITIZEN) <- c("No not US citizen","yes US citizen","refused","NA","dont know")


data$EMPSTAT <- as.factor(data$EMPSTAT)
levels(data$EMPSTAT) <- c("NIU","Employed","not employed","dont know")

data$EMPHI <- as.factor(data$EMPHI)
levels(data$EMPHI) <- c("NIU","no workplace did not offer health insurance","yes workplace offer health insurance","refused","NA","dont know")

data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$EMPSTAT<-as.factor(data$EMPSTAT)
levels(data$EMPSTAT)<-c("not employed","employed","dont know","NIU")









mydata = select(data,-c("YEAR","RACENEW","CITIZEN","HINOTCOVE","USBORN", "SEXORIEN", "EDUC", "EMPHI","AGE", "HINOTCOVE"))


mydata= select(data,-c("YEAR","RACENEW","CITIZEN","HINOTCOVE","USBORN", "SEXORIEN", "EDUC", "EMPHI", "FAMTOTINC", "POVERTY", "AGE", "HINOTCOVE"))






######################
# LM
######################
attach(data)
library(jtools)
model_temp1 <- lm(HINOTCOVE ~ SEXORIEN + SEX + RACENEW + CITIZEN + EMPSTAT + EDUC)
summary(model_temp1)
plot(model_temp1)
abline(model_temp1)
print(model_temp1)
xtabs(model_temp1)
summ(model_temp1, confint = TRUE, digits = 3)
summ(model_temp1, robust = "HC1")
summ(model_temp1, confint = TRUE, digits = 3)






library(jtools)


model_temp2 <- lm(HINOTCOVE ~ OCC + EDUC)
summary(model_temp2)
model_temp3 <- lm(EMPHI ~ CITIZEN + USBORN + EMPSTAT)
summary(model_temp3)
plot(model_temp2)



fitg <- glm(HINOTCOVE ~ SEX + EDUC + OCC)
summary(fitg)
plot(fitg)
summ(fitg)
tab_model(fitg)
plot(model_temp1, fitg)




model_temp5 <- lm(HINOTCOVE ~ SEX)
summary(model_temp5)
plot(model_temp5)
print(model_temp5)
xtabs(model_temp5)





data$RACENEW <- as.factor(data$RACENEW)
levels(data$RACENEW) <- c("white","Black","Aleut Alaskan","American Indian","Asian","Other","refused","not ascertained","unknown")


data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")


model_temp6 <- lm(HINOTCOVE ~ RACENEW)
summary(model_temp6)
plot(model_temp6)
tab_model(model_temp6)

model_temp7 <- lm(HINOTCOVE ~ CITIZEN)
summary(model_temp7)
plot(model_temp7)



model_temp8 <- lm(HINOTCOVE ~ OCC)
summary(model_temp8)
plot(model_temp8)





#######################
#Practice LM
######################

library(AER)
library(MASS)

attach(data)
# create a scatterplot of the data
plot(HINOTCOVE ~ EDUC)

# add the systematic relationship to the plot
abline(a = 713, b = -3)


head(data)

model22 <- mean(data$HINOTCOVE)


plot(HINOTCOVE ~ EDUC, 
     data = data,
     main = "Scatterplot of Health Coverage and Education", 
     xlab = "EDUCATION (X)",
     ylab = "Healthcoverage (Y)")







linear_model0 <- lm(HINOTCOVE ~ RACENEW,CITIZEN)
summary(linear_model0)
plot(linear_model0)




linear_model3 <- lm(HINOTCOVE ~ RACENEW,CITIZEN,USBORN, AGE)
summary(linear_model3)
plot(linear_model3)



linear_model4 <- lm(HINOTCOVE ~ RACENEW,USBORN,CITIZEN,EDUC)
summary(linear_model4)
plot(linear_model4)






data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$CITIZEN <- as.factor(data$CITIZEN)
levels(data$CITIZEN) <- c("No not US citizen","yes US citizen","refused","NA","dont know")


model101 <- lm(HINOTCOVE ~ CITIZEN)
summary(model101)
# plot the data
plot(HINOTCOVE ~ CITIZEN, 
     data = data,
     main = "Scatterplot of Health Coverage  and Citizenship", 
     xlab = "Citizenship (X)",
     ylab = "EDUCATION (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))

# add the regression line
abline(model101) 





#################################
#Correlation

cor(data$HINOTCOVE, data$EDUC)

#################################









################################################################################
data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$EDUC <- as.factor(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")



data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$EDUC <- as.numeric(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")




mod_simple <- lm(HINOTCOVE ~ EDUC)
prediction <- predict(lm(HINOTCOVE ~ EDUC + I(EDUC^2)), data.frame(EDUC = sort(EDUC)))

plot(HINOTCOVE ~ EDUC)
abline(mod_simple, col = "red")
lines(sort(EDUC), prediction)
tab_model(mod_simple)






data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$RACENEW <- as.factor(data$RACENEW)
levels(data$RACENEW) <- c("white","Black","Aleut Alaskan","American Indian","Asian","Other","refused","not ascertained","unknown")



####################################################

###############MULTIPLE REGRESSION#################

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$RACENEW <- as.numeric(data$RACENEW)
levels(data$RACENEW) <- c("white","Black","Aleut Alaskan","American Indian","Asian","Other","refused","not ascertained","unknown")

data$EDUC <- as.numeric(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")

data$CITIZEN <- as.numeric(data$CITIZEN)
levels(data$CITIZEN) <- c("No not US citizen","yes US citizen","refused","NA","dont know")






mod001 <- lm(HINOTCOVE ~CITIZEN, data = data)
mult.mod <- lm(HINOTCOVE ~ RACENEW, EDUC, CITIZEN, data = data)



mod001
mult.mod
summary(mult.mod)$coef
summary(mod001)
tab_model(mod001)
summary(mult.mod)
tab_model(mult.mod)




#creating subset


use_varb1 <- (AGE >= 25) & (AGE <= 55)  & (EDUC==1)  & (RACENEW==1)  & (SEX==1)  & (USBORN==1)
dat_use <- subset(data,use_varb1)







model <- lm(HINOTCOVE ~ EDUC + RACENEW, data = data)
coeftest(model, vcov. = vcovHC, type = "HC1")



model22 <- lm(HINOTCOVE ~ RACENEW + USBORN, data = data)
confint(model22)
confint(model22, level = 0.9)


library(AER)
library(mvtnorm)
library(stargazer)




data$EMPSTAT <- as.factor(data$EMPSTAT)
levels(data$EMPSTAT) <- c("NIU","Employed","not employed","dont know")

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")


ms_mod1 <- lm(HINOTCOVE ~ EMPSTAT)
summary(ms_mod1)
tab_model(ms_mod1)
ms_mod1


# plot the data
plot(EMPSTAT, HINOTCOVE, 
     main = "TYPE OF EMPLOYMENT",
     pch = 20,
     col = "steelblue")

# plot the linear regression line
abline(ms_mod1, 
       col = "darkred",
       lwd = 2)




#######Regression with a Binary Dependent Variable

denymod1 <- lm(HINOTCOVE ~ EMPSTAT, data = data)
summary(denymod1)



denymod2 <- lm(EDUC ~ EMPSTAT, data = data)
summary(denymod2)
plot(denymod2)
abline(denymod2)
coef(denymod2)
summary(coef(denymod2))
4
denymod2

# plot the data
plot(x = data$EMPSTAT, 
     y = data$HINOTCOVE,
     main = "Scatterplot Mortgage Application Denial and the Payment-to-Income Ratio",
     xlab = "Type of Employment",
     ylab = "Health Coverage",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Has Health Coverage")
text(2.5, -0.1, cex= 0.8, "No Healthcoverage")

# add the estimated regression line
abline(denymod1, 
       lwd = 1.8, 
       col = "steelblue")


###############################################################

plot(data$SEXORIEN)
plot(data$HINOTCOVE)
plot(data$SEXORIEN, data$HINOTCOVE, main = "HINOTCOVE/SEXORIEN", xlab = "SEXORIEN", ylab = "HINOTCOVE", col = "red")
abline(lm(data$HINOTCOVE~data$SEXORIEN), col="blue")




 


###############################################################################


attach(data)
model_v1 <- lm(HINOTCOV ~ RACENEW)
detach()


model_v1 <- lm(HINOTCOV ~ RACENEW)
summary(model_v1)

data$RACENEW <- as.numeric(data$RACENEW)
levels(data$RACENEW) <- c("white","Black","Aleut Alaskan","American Indian","Asian","Other","refused","not ascertained","unknown")



model_v2 <- lm(data$HINOTCOVE ~ data$RACENEW)
summary(model_v2)


model_v3 <- lm(HINOTCOV ~ RACENEW, data = data)
summary(model_v3)





##########################
#glm
#########################3

data$HINOTCOVE <- (data$HINOTCOVE)
is.na(data$HINOTCOVE) <- (data$HINOTCOVE)

table(data$HINOTCOVE,data$EDUC)


model_logit <- glm(HINOTCOVE ~ EDUC)
summary(model_logit)




model_logit00 <- glm(HINOTCOVE ~ EDUC)
summary(model_logit00)                   

summary(EDUC)
summary(HINOTCOVE)


##data$FAMTOTINC<-as.factor(data$FAMTOTINC)
levels(data$FAMTOTINC)<-c()




##################################################
#Graph
##################################################
library(ipumsr)
library(hrbrthemes)
library(ggplot2)


p2 <- ggplot(data, aes(x=HINOTCOVE, y=SEX)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()
print(p2)
summary(p2)


p3 <- ggplot(data, aes(x=HINOTCOVE, y=EDUC)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
print(p3)



p4 <- ggplot(data, aes(x=HINOTCOVE, y=CITIZEN)) +
  geom_point() +
  geom_smooth(method=glm , color="BLUE", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
print(p4)







######################################################
#GLM  
################################
data$CITIZEN <- as.factor(data$CITIZEN)
levels(data$CITIZEN) <- c("No not US citizen","yes US citizen","refused","NA","dont know")
data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")
data$EDUC <- as.factor(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")
data$EMPSTAT<-as.factor(data$EMPSTAT)
levels(data$EMPSTAT)<-c("not employed","employed","dont know","NIU")


data$EDUC <- as.numeric(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

attach(data)
library(tidyverse)
library(sjPlot)
library(lme4)

#1
g <- glm(HINOTCOVE ~ EDUC + CITIZEN + EMPSTAT, family = binomial)
summary(g)
head(g)
tab_model(g)



#2
gt <- glm(HINOTCOVE ~ EDUC, family = binomial)
summary(gt)
tab_model(gt)

#anavo test
anova(g, gt, test= "Chisq") 

tab_model(g, gt)


p = plot_model(g, type = 'pred')
plot_grid(p)


p = plot_model(gt, type = 'pred')
plot_grid(p)





################################



#######################################
#LM
#######################################

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")

data$SEXORIEN <- as.numeric(data$SEXORIEN)
levels(data$SEXORIEN) <- c("NIU","Lesbian or gay","straight","bisexual","something else","dont know","refused","NA")




attach(data)
plot(HINOTCOVE, SEXORIEN)
cor(HINOTCOVE,SEXORIEN)

sample1 <- lm(HINOTCOVE ~ SEXORIEN)
sample1
summary(sample1)

# 95% confidence interval of the regression coefficients
confint(sample1, level = 0.95)


est_y = fitted.values (sample1)
sum(est_y)


#residuals

resi = residuals(sample1)
resi



data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")



glimpse(data)

ggplot(data, aes(x=HINOTCOVE,
                 y = EDUC,
                 col = HINOTCOVE)) +
  geom_point()+
  geom_smooth(method= "lm", se = FALSE)


ggplot(data, aes(x=HINOTCOVE,
                 y = EDUC,
                 col = HINOTCOVE)) +
  geom_point()


ford <-lm(HINOTCOVE ~ EDUC)
summary(ford)



ggplot(data, aes(x=HINOTCOVE,
                 y = EDUC,
                 col = HINOTCOVE)) +
  geom_point()+
  geom_abline(aes(intercept = 1.050950,
                  slope= 0.254135,
                  col="Chinstrap"))



                



#######################################



#######################################

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")
data$EMPSTAT <- as.numeric(data$EMPSTAT)
levels(data$EMPSTAT) <- c("NIU","Employed","not employed","dont know")



data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")
data$EMPSTAT <- as.factor(data$EMPSTAT)
levels(data$EMPSTAT) <- c("NIU","Employed","not employed","dont know")


ggplot(data = data, aes(x = EMPSTAT, y = HINOTCOVE))+
  geom_boxplot(aes(x = EMPSTAT, y = HINOTCOVE, fill = EMPSTAT))+
  scale_color_viridis_d()+
  theme_classic()+
  labs(y= "HINOTCOVE")










library(tidyverse)
library(ggplot2)
library(mosaic)
library(sjPlot)
library(stargazer)

attach(data)


lml <-lm(HINOTCOVE ~ RACENEW + CITIZEN + USBORN)
summary(lml)
coef(lml)
tab_model(lml)

lml2 <-lm(HINOTCOVE ~ RACENEW + CITIZEN + USBORN + EDUC)
summary(lml2)
coef(lml2)
tab_model(lml2)

stargazer(lml, type ="text")
stargazer(lml)


stargazer(lml, lml2, type ="text")
stargazer(lml, lml2)


#######################################































# Exploratory Data Analysis
################################################################################
#1 Health Insurance By US BORN


data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")




sjPlot::tab_xtab(var.row=data$USBORN, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By USBORN",
                 show.row.prc = TRUE)

ggplot(data,
       aes(x= USBORN, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green") + labs(x = "USBORN", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By USBORN")








#2 Health Insurance By Citizenship

sjPlot::tab_xtab(var.row=data$CITIZEN, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Citizenship",
                 show.row.prc = TRUE)

ggplot(data,
       aes(x= CITIZEN, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green",) + labs(x = "Citizenship", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Citizenship")




sjPlot::tab_xtab(var.row = data$CITIZEN,
                 var.col = data$HINOTCOVE, title="Table Health insurance by CITIZENSHIP",
                 show.row.prc  =TRUE)




#3 Health Insurance By EDUCATION


sjPlot::tab_xtab(var.row=data$EDUC, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Education",
                 show.row.prc = TRUE)


ggplot(data,
       aes(x= EDUC, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="blue") + labs(x = "EDUCATION", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By EDUCATION")








#4 Health Insurance by Workplace

sjPlot::tab_xtab(var.row=data$EMPHI, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance Offered at  Workplace",
                 show.row.prc = TRUE)







#5 Health Insurance by Sexual Orientation 

sjPlot::tab_xtab(var.row=data$SEXORIEN, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance by Sexual Orientation ",
                 show.row.prc = TRUE)




ggplot(data,
       aes(x= SEXORIEN, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green") + labs(x = "Sexual Orientation", y = "Health Insurance by Sexual Orientation")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Sexual Orientation")







#6 Health Insurance by Race

sjPlot::tab_xtab(var.row=data$RACENEW, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Race",
                 show.row.prc = TRUE)

ggplot(data,
       aes(x= RACENEW, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green") + labs(x = "RACE", y = "Health Insurance by Race")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Race")






########### Health Insurance by Income

data$FAMTOTINC <- as.factor(data$FAMTOTINC)
levels(data$FAMTOTINC) <-  c("$0-$40,000", "$40,000+")





sjPlot::tab_xtab(var.row=data$FAMTOTINC, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Income",
                 show.row.prc = TRUE)

ggplot(data,
       aes(x= FAMTOTINC, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="purple") + labs(x = "Income", y = "Health Insurance by Income")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Income")








#8 Health Insurance by Years in US


sjPlot::tab_xtab(var.row=data$YRSINUSG, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Years in US",
                 show.row.prc = TRUE)

ggplot(data,
       aes(x= YRSINUSG, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green") + labs(x = "Income", y = "Health Insurance by Years in US")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Years in US")






#8 Health Insurance By US Born Citizenship Status 
############################################













#9 Health Insurance By Health Status 


sjPlot::tab_xtab(var.row = data$HEALTH, 
                 var.col = data$HINOTCOVE,
                 title = "Health Insurance By Health Status",
                 show.row.prc = TRUE)






ggplot(data, aes(x=HEALTH, y=HINOTCOVE)) + 
  geom_bar(stat = "identity", width=0.2, color="Black")+
  xlab("Health Condition") + 
  ylab("Health Insurance Status")+
  ggtitle("Health Insurance By Health Status")










###################################################








ggplot(data,
       aes(x= SEXORIEN, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="Green") + labs(x = "Sexual Orientation", y = "Health Insurance by Sexual Orientation")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Sexual Orientation")








sjPlot::tab_xtab(var.row=data$SEX, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Sex",
                 show.row.prc = TRUE,drop.empty = TRUE)



plot(data,
     aes(x= RACEA, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="blue") + labs(x = "Race", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .1) +
  ggtitle("Health Insurance By Race")



ggplot(data,
       aes(x= CITIZEN, y=HINOTCOVE)) +
  geom_point(color = "firebrick", shape = "diamond", size = 2)+ labs(x = "Citizen", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance by Citzenship")




ggplot(data, aes(x=SEXORIEN, y=HINOTCOVE)) + 
  geom_bar(stat = "identity", width=0.5, color="Purple") + labs(x = "Sexual Orientation", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By Sexual Orientation")



ggplot(data,
       aes(x= HIMCAIDE, y= HINOTCOVE)) + geom_point(color= "purple", alpha = 2) + labs(x = "Medicaid Status", y = "Health Insurance Status") + 
  
  stat_smooth() +
  theme(panel.grid.major = element_line(size = .1, linetype = "dashed"),
        panel.grid.minor = element_line(size = .2, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "red1"),
        panel.grid.major.y = element_line(color = "blue1"),
        panel.grid.minor.x = element_line(color = "red4"),
        panel.grid.minor.y = element_line(color = "blue4"))+
  ggtitle("Health Insurance By Medicaid Status") 





ggplot(data,
       aes(x= HIMCAREE, y= HINOTCOVE)) + geom_point() + labs(x = "Medicare Status", y = "Health Insurance Status")+
  geom_violin(draw_quantiles   = c(.25, .5, .75))+
  geom_jitter(aes(y= HINOTCOVE,
                  x= HIMCAREE),
              color="Purple",
              height = 0,
              width=0.5, alpha=100)+
  
  ggtitle("Health Insurance By Medicare Status")
xlab("")



ggplot(aes(x= YRSINUS, 
           y= HINOTCOVE, 
           color=YRSINUS), data = data)+
  geom_point()+
  geom_smooth(method="lm", se=FALSE) + 
  facet_wrap(~YRSINUS + HINOTCOVE) +
  ggtitle("Health Insurance by Years lived in the United States")




data$H_insurance = ifelse(data$HINOTCOVE == "has health insurance coverage","has  insurance coverage","no insurance") #drop the - HINOTCOVE

colnames(data)




library(Boruta)
set.seed(123)
boruta.train <- Boruta(H_insurance~. ,data = data_sample, doTrace = 2)
print(boruta.train)



plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)




require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)


data$HINOTCOVE <- relevel(factor(data$HINOTCOVE), ref = "has health insurance coverage")
test <- multinom(data$HINOTCOVE ~ data$RACENEW + data$YEAR +
                   data$SEX + data$MARST + data$EDUC)




sjPlot::plot_xtab(data$RACENEW,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)




sjPlot::plot_xtab(data$FAMTOTINC,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)


sjPlot::plot_xtab(data$citizen,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)






sjPlot::plot_xtab(data$MARST,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)



sjPlot::plot_xtab(data$EMPSTAT,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)



sjPlot::plot_xtab(data$EDUC,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)
















barplot(table(data$HINOTCOVE, data$EDUC), main = 'EDUCATION' , xlab = "Health Coverage", ylab = "HINOTCOVE")


barplot(table(data$HINOTCOVE, data$EDUC), main = 'Health Coverage Base on Education' , xlab = "HEDUCATION", ylab = "HINOTCOVE", col = "blue")


pie(table(data$HINOTCOVE, data$SEX), main = 'Health Coverage Base on SEX')





pie(table(data$SEX), main = 'SEX')



data$EDUC <- as.factor(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")

data$HINOTCOVE <- as.factor(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")



data$EDUC <- as.numeric(data$EDUC)
levels(data$EDUC) <- c("NIU","no school","less than hs","12th grade no diploma","HS diploma","GED","some college","assoc deg in tech or occ","assoc deg academic","bachelors","masters","professional degree","doctoral","refused","dont know")

data$HINOTCOVE <- as.numeric(data$HINOTCOVE)
levels(data$HINOTCOVE) <- c("has health insurance coverage","no health insurance coverage","dont know")



hist(data$EDUC, breaks = 7, main = "Education")

hist(data$EDUC, breaks = 7, main = "Education", xlab= "Education", ylab = "Health Coverage")



histogram(~HINOTCOVE + EDUC, data = data)


bwplot(~ HINOTCOVE + CITIZEN + RACENEW, data = data) 









sjPlot::tab_xtab(var.row=data$EDUC, 
                 var.col =data$HINOTCOVE,
                 title = "Health Insurance By Education",
                 show.row.prc = TRUE)


ggplot(data,
       aes(x= EDUC, y=HINOTCOVE)) +
  geom_bar(stat = "identity", width=0.5, color="red") + labs(x = "EDUCATION", y = "Health Insurance Status")+
  geom_line(color = "firebrick", linetype = "dotted", size = .3) +
  ggtitle("Health Insurance By EDUCATION")


sjPlot::plot_xtab(data$EDUC,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)



sjPlot::tab_xtab(var.row = data$HEALTH, 
                 var.col = data$HINOTCOVE,
                 title = "Health Insurance By Health Status",
                 show.row.prc = TRUE)



ggplot(data, aes(x=HEALTH, y=HINOTCOVE)) + 
  geom_bar(stat = "identity", width=0.2, color="Black")+
  xlab("Health Condition") + 
  ylab("Health Insurance Status")+
  ggtitle("Health Insurance By Health Status")



sjPlot::plot_xtab(data$HEALTH,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)

data$YRSINUSG

ggplot(aes(x= YRSINUS, 
           y= HINOTCOVE, 
           color=YRSINUS), data = data)+
  geom_point()+
  geom_smooth(method="lm", se=FALSE) + 
  facet_wrap(~YRSINUS + HINOTCOVE) +
  ggtitle("Health Insurance by Years lived in the United States")



ggplot(data,
       aes(x= HIMCAREE, y= HINOTCOVE)) + geom_point() + labs(x = "Medicare Status", y = "Health Insurance Status")+
  geom_violin(draw_quantiles   = c(.25, .5, .75))+
  geom_jitter(aes(y= HINOTCOVE,
                  x= HIMCAREE),
              color="Purple",
              height = 0,
              width=0.5, alpha=100)+
  
  ggtitle("Health Insurance By Medicare Status")

xlab("")






sjPlot::plot_xtab(data$EMPSTAT,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)






sjPlot::plot_xtab(data$CITIZEN,data$HINOTCOVE,margin="row",bar.pos="stack",
                  coord.flip=TRUE)













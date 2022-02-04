#1. input data
library(haven)
data <- read.csv("Mindfulness_Stress.csv")
head(data) #check dataset if things are okay

#2. describe data
library(psych) #read the using package
describe(data) 

#4. analysis and model
library(lavaan)
# model <- 'PS + Decen + SS ~ M
#           PS + SS + Dist ~ Decen
#           Dist ~ PS
#           Dist ~ SS
#           '
model1<-'Decen ~ M
        PS + SS + Dist ~ Decen + M
        Dist ~ PS + SS
'
fit1<-sem(model1,data=data) 
summary(fit1)
summary(fit1, standardized=T, fit.measures=T, rsq=T)


model2<-'Decen ~ M
        PS + SS  ~ Decen + M
        Dist ~ PS + SS
'
fit2<-sem(model2,data=data) 
summary(fit2)
summary(fit2, standardized=T, fit.measures=T, rsq=T)


model3<-'Decen ~ M
        PS  ~ Decen + M
        SS  ~ Decen
        Dist ~ PS + SS
'
fit3<-sem(model3,data=data) 
summary(fit3)
summary(fit3, standardized=T, fit.measures=T, rsq=T)

# Model3 works. then I need all parameters
model4<- 'Decen ~ a*M
        PS  ~ b*Decen + c*M
        SS  ~ d*Decen
        Dist ~ e*PS + f*SS
        
         PSindirect := a*b
         PSdirect :=c
         PStotal := c+(a*b)
 '
fit4<-sem(model4,data=data) 
summary(fit4)
summary(fit4, standardized=T, fit.measures=T, rsq=T)

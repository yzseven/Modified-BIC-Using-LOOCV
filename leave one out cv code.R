library(simstudy)
library(dplyr)
library(caret)


#### leave one out cross validation prediction error using trick

loocv=function(fit){
  
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
  
}

#### Define Dataset

def <- defData(varname = "x1", dist="uniform",formula = "10;20")
## x1 is from unifrom distribution 
def <- defData(def, varname = "y1", formula = 3, variance = 1)
## y is from N(3,1)

#### sample size = 40
md1<-c()

for(i in 1:1000){
  
  dt <- genData(40, def)
  dt <- dt%>%select(y1,x1)##generate dataset n=40
  
  fit1 <- lm(y1 ~ 1, data = dt)
  fit2 <- lm(y1 ~ x1, data = dt)
  
  a<-c(loocv(fit1),loocv(fit2))
  
md1<-c(md1,which.min(a))##select the one with small prediction error
}

table(md1)/1000## empirical probability


#### sample size = 100
md2<-c()

for(i in 1:1000){
  
  dt <- genData(100, def)
  dt<-dt%>%select(y1,x1)
  
  fit1 <- lm(y1 ~ 1, data = dt)
  fit2 <- lm(y1 ~ x1, data = dt)
  
  a<-c(loocv(fit1),loocv(fit2))
  
  md2<-c(md2,which.min(a))
  
}

table(md2)/1000


#### sample size = 500
md3<-c()

for(i in 1:1000){
  
  dt <- genData(500, def)
  dt<-dt%>%select(y1,x1)
  
  fit1 <- lm(y1 ~ 1, data = dt)
  fit2 <- lm(y1 ~ x1, data = dt)
  
  a<-c(loocv(fit1),loocv(fit2))
  
  md3<-c(md3,which.min(a))
  
}

table(md3)/1000

#### sample size = 1000
md4<-c()

for(i in 1:1000){
  
  dt <- genData(1000, def)
  dt <- dt%>%select(y1,x1)
  
  fit1 <- lm(y1 ~ 1, data = dt)
  fit2 <- lm(y1 ~ x1, data = dt)
  
  a<-c(loocv(fit1),loocv(fit2))
  
  md4<-c(md4,which.min(a))
  
}

table(md4)/1000





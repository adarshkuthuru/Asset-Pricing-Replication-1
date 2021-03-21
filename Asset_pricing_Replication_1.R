library(rvest)
library(tidyverse)
library(RCurl)
library(zoo)
library(tseries)
library(stringr)
library(stringi)
library(lubridate)
library(data.table)
library(ggplot2)
library(vars) #VAR
library(expm)
library(Matrix) #for matrices
library(lattice) #for matrices
library(reshape2)
library(xtable) #prints LaTeX tables
library(dplyr)
library(sandwich) #HH std errors

#Extracting mcculloch and kwon term-structure data
require(Ecdat) #Package required to import mcculloch and kwon term-structure data from OSU website 

setwd("C:/Users/KUTHURU/Downloads/Laptop/Semester 3/Empirical Asset Pricing Pierluigi/Replication-1/")

data(Irates) #Imports mcculloch and kwon term-structure data from OSU website 
as.yearmon(time(Irates)) #to check for dates of each row

data1=data.frame(date=as.yearmon(time(Irates)),rate=as.matrix(Irates)) #converts ts dataset to dataframe and adds date

#write.csv(data1, file = "data.csv", row.names = F,col.names = T)

#import inflation, rf-rate, CRSP vw-index and div yields data
data2=read.csv("data2.csv")
data2$date=as.yearmon(format(data2$caldt),"%Y%m%d")

#merge inflation with interest rates
data <- merge(data1,data2,by="date")
write.csv(data, file = "final_data.csv", row.names = F,col.names = T)

#import final dataset and estimate relative bill rate
final=read.csv("final_data.csv")

#esimate one yr-moving avg rate
for(i in 13:nrow(final)){
  sum=0
  for(j in 1:12){
    sum=sum+final$rate.r1[i-j]
  }
  final$ma[i]=sum/12
} 

#relative bill rate
final$rb_rate_100=(final$rate.r1-final$ma)

#final dataset
#write.csv(final, file = "final_data.csv", row.names = F,col.names = T)


#final=read.csv("campbell_ammer.csv")
final$date=as.yearmon(format(final$date),"%Y%m%d")
final <- as.data.table(final)





## ============================================================================== 
##                            Campbell and Ammer Replication 
## ============================================================================== 


## ============================================================================== 
##                                  Create Table-2
##                                  VAR Estimation:
## ============================================================================== 

#subsample-1
intYear <- 1952
finYear <- 1986
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]

var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

#variance-covariance matrix
#vcv=cov(vec_state, NULL, use = "complete.obs", method = c("pearson", "kendall", "spearman"))

table2a=rbind(coef(summary(var_est$varresult$ex_stock_ret_100))[,1],coef(summary(var_est$varresult$ex_stock_ret_100))[,2],
             coef(summary(var_est$varresult$real_int_100))[,1],coef(summary(var_est$varresult$real_int_100))[,2],
             coef(summary(var_est$varresult$Chg_1month))[,1],coef(summary(var_est$varresult$Chg_1month))[,2],
             coef(summary(var_est$varresult$spreads))[,1],coef(summary(var_est$varresult$spreads))[,2],
             coef(summary(var_est$varresult$log_dp))[,1],coef(summary(var_est$varresult$log_dp))[,2],
             coef(summary(var_est$varresult$rb_rate_100))[,1],coef(summary(var_est$varresult$rb_rate_100))[,2])

rownames(table2a)=paste(c("$e_t+1$"," ","$r_t+1$", " ","$\\Delta y_{n,t+1}$"," ",
                    "$s_{n,t+1}$"," ", "$d_t+1-p_t+1$"," ", "$rb_t+1$"," "))

#rownames(table2a) <- gsub("^X\\.*","",rownames(table2a))

table2a=round(table2a, digits =3) #rounding off values


#add parentheses to p-values
for(k in 1:(nrow(table2a)/2)){    
  table2a[2*k,] <- paste0("(", format(unlist(table2a[2*k,])),")") 
}

#subsample-2
intYear <- 1952
finYear <- 1979
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]

var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

#variance-covariance matrix
#vcv=cov(vec_state, NULL, use = "complete.obs", method = c("pearson", "kendall", "spearman"))

table2b=rbind(coef(summary(var_est$varresult$ex_stock_ret_100))[,1],coef(summary(var_est$varresult$ex_stock_ret_100))[,2],
             coef(summary(var_est$varresult$real_int_100))[,1],coef(summary(var_est$varresult$real_int_100))[,2],
             coef(summary(var_est$varresult$Chg_1month))[,1],coef(summary(var_est$varresult$Chg_1month))[,2],
             coef(summary(var_est$varresult$spreads))[,1],coef(summary(var_est$varresult$spreads))[,2],
             coef(summary(var_est$varresult$log_dp))[,1],coef(summary(var_est$varresult$log_dp))[,2],
             coef(summary(var_est$varresult$rb_rate_100))[,1],coef(summary(var_est$varresult$rb_rate_100))[,2])

rownames(table2b)=c("$e_t+1$"," ","$r_t+1$", " ","$\\Delta y_{n,t+1}$"," ",
                    "$s_{n,t+1}$"," ", "$d_t+1-p_t+1$"," ", "$rb_t+1$"," ")

table2b=round(table2b, digits =3) #rounding off values


#add parentheses to p-values
for(k in 1:(nrow(table2b)/2)){    
  table2b[2*k,] <- paste0("(", format(unlist(table2b[2*k,])),")") 
}

#subsample-3
intYear <- 1952
finYear <- 1972
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]

var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

#variance-covariance matrix
#vcv=cov(vec_state, NULL, use = "complete.obs", method = c("pearson", "kendall", "spearman"))

table2c=rbind(coef(summary(var_est$varresult$ex_stock_ret_100))[,1],coef(summary(var_est$varresult$ex_stock_ret_100))[,2],
             coef(summary(var_est$varresult$real_int_100))[,1],coef(summary(var_est$varresult$real_int_100))[,2],
             coef(summary(var_est$varresult$Chg_1month))[,1],coef(summary(var_est$varresult$Chg_1month))[,2],
             coef(summary(var_est$varresult$spreads))[,1],coef(summary(var_est$varresult$spreads))[,2],
             coef(summary(var_est$varresult$log_dp))[,1],coef(summary(var_est$varresult$log_dp))[,2],
             coef(summary(var_est$varresult$rb_rate_100))[,1],coef(summary(var_est$varresult$rb_rate_100))[,2])

rownames(table2c)=c("$e_t+1$"," ","$r_t+1$", " ","$\\Delta y_{n,t+1}$"," ",
                    "$s_{n,t+1}$"," ", "$d_t+1-p_t+1$"," ", "$rb_t+1$"," ")

table2c=round(table2c, digits =3) #rounding off values


#add parentheses to p-values
for(k in 1:(nrow(table2c)/2)){    
  table2c[2*k,] <- paste0("(", format(unlist(table2c[2*k,])),")") 
}

#subsample-4
intYear <- 1973
finYear <- 1986
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]

var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

#variance-covariance matrix
#vcv=cov(vec_state, NULL, use = "complete.obs", method = c("pearson", "kendall", "spearman"))

table2d=rbind(coef(summary(var_est$varresult$ex_stock_ret_100))[,1],coef(summary(var_est$varresult$ex_stock_ret_100))[,2],
              coef(summary(var_est$varresult$real_int_100))[,1],coef(summary(var_est$varresult$real_int_100))[,2],
              coef(summary(var_est$varresult$Chg_1month))[,1],coef(summary(var_est$varresult$Chg_1month))[,2],
              coef(summary(var_est$varresult$spreads))[,1],coef(summary(var_est$varresult$spreads))[,2],
              coef(summary(var_est$varresult$log_dp))[,1],coef(summary(var_est$varresult$log_dp))[,2],
              coef(summary(var_est$varresult$rb_rate_100))[,1],coef(summary(var_est$varresult$rb_rate_100))[,2])

rownames(table2d)=c("$e_t+1$"," ","$r_t+1$", " ","$\\Delta y_{n,t+1}$"," ",
                    "$s_{n,t+1}$"," ", "$d_t+1-p_t+1$"," ", "$rb_t+1$"," ")

table2d=round(table2d, digits =3) #rounding off values


#add parentheses to p-values
for(k in 1:(nrow(table2d)/2)){    
  table2d[2*k,] <- paste0("(", format(unlist(table2d[2*k,])),")") 
}

#final table-2
table2=rbind(table2a,table2b,table2c, table2d)

#Rename column names in table2
colnames(table2)=c("$e_t$", "$r_t$", "$\\Delta y_{n,t}$","$s_{n,t}$", "$d_t-p_t$", "$rb_t$")

## ============================================================================== 
##                                  Create Table-3
##                              Variance Decomposition:
## ============================================================================== 

#subsample-1
intYear <- 1952
finYear <- 1986
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]
var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

rho <- 0.05
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:6, 1]})) #coeff matrix
lambda <- (rho * Gamma) %*% solve(diag(6) - rho * Gamma) #as per formula in appendix-B & %*% is for matrix multiplication


#Residuals=>News
u <- as.vector(resid(var_est)[, 1]) 

RP_news <- as.vector(c(1,0,0,0,0,0) %*% lambda %*% t(resid(var_est)))
Ret_news <- as.vector(c(0,1,0,0,0,0) %*% solve(diag(6) - rho * Gamma) %*% t(resid(var_est)))
Div_news <- as.vector(u+Ret_news+RP_news)
  
#Variance decomposition; terms have to add to 1
table3a=rbind(var(Div_news)/var(u),
-2*cov(Ret_news, Div_news)/var(u),
-2*cov(RP_news, Div_news)/var(u),
var(Ret_news)/var(u),
2*cov(Ret_news, RP_news)/var(u),
var(RP_news)/var(u))

#subsample-2
intYear <- 1952
finYear <- 1979
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]
var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

rho <- 0.05
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:6, 1]})) #coeff matrix
lambda <- (rho * Gamma) %*% solve(diag(6) - rho * Gamma) #as per formula in appendix-B & %*% is for matrix multiplication


#Residuals=>News
u <- as.vector(resid(var_est)[, 1]) 

RP_news <- as.vector(c(1,0,0,0,0,0) %*% lambda %*% t(resid(var_est)))
Ret_news <- as.vector(c(0,1,0,0,0,0) %*% solve(diag(6) - rho * Gamma) %*% t(resid(var_est)))
Div_news <- as.vector(u+Ret_news+RP_news)

#Variance decomposition; terms have to add to 1
table3b=rbind(var(Div_news)/var(u),
              -2*cov(Ret_news, Div_news)/var(u),
              -2*cov(RP_news, Div_news)/var(u),
              var(Ret_news)/var(u),
              2*cov(Ret_news, RP_news)/var(u),
              var(RP_news)/var(u))

#subsample-3
intYear <- 1952
finYear <- 1972
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]
var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

rho <- 0.05
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:6, 1]})) #coeff matrix
lambda <- (rho * Gamma) %*% solve(diag(6) - rho * Gamma) #as per formula in appendix-B & %*% is for matrix multiplication


#Residuals=>News
u <- as.vector(resid(var_est)[, 1]) 

RP_news <- as.vector(c(1,0,0,0,0,0) %*% lambda %*% t(resid(var_est)))
Ret_news <- as.vector(c(0,1,0,0,0,0) %*% solve(diag(6) - rho * Gamma) %*% t(resid(var_est)))
Div_news <- as.vector(u+Ret_news+RP_news)

#Variance decomposition; terms have to add to 1
table3c=rbind(var(Div_news)/var(u),
              -2*cov(Ret_news, Div_news)/var(u),
              -2*cov(RP_news, Div_news)/var(u),
              var(Ret_news)/var(u),
              2*cov(Ret_news, RP_news)/var(u),
              var(RP_news)/var(u))

#subsample-4
intYear <- 1973
finYear <- 1986
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]
var_est <- VAR(vec_state, p=1, type="none")
summary(var_est)

rho <- 0.05
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:6, 1]})) #coeff matrix
lambda <- (rho * Gamma) %*% solve(diag(6) - rho * Gamma) #as per formula in appendix-B & %*% is for matrix multiplication


#Residuals=>News
u <- as.vector(resid(var_est)[, 1]) 

RP_news <- as.vector(c(1,0,0,0,0,0) %*% lambda %*% t(resid(var_est)))
Ret_news <- as.vector(c(0,1,0,0,0,0) %*% solve(diag(6) - rho * Gamma) %*% t(resid(var_est)))
Div_news <- as.vector(u+Ret_news+RP_news)

#Variance decomposition; terms have to add to 1
table3d=rbind(var(Div_news)/var(u),
              -2*cov(Ret_news, Div_news)/var(u),
              -2*cov(RP_news, Div_news)/var(u),
              var(Ret_news)/var(u),
              2*cov(Ret_news, RP_news)/var(u),
              var(RP_news)/var(u))

#subsample-5
intYear <- 1952
finYear <- 1986
## State vector: [ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100]
vec_state <- final[year(final$date) >= intYear, ]
vec_state <- vec_state[year(vec_state$date) <= finYear, list(ex_stock_ret_100,real_int_100,Chg_1month,spreads,log_dp,rb_rate_100)]
var_est <- VAR(vec_state, p=3, type="none")
summary(var_est)

rho <- 0.05
Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:6, 1]})) #coeff matrix
lambda <- (rho * Gamma) %*% solve(diag(6) - rho * Gamma) #as per formula in appendix-B & %*% is for matrix multiplication


#Residuals=>News
u <- as.vector(resid(var_est)[, 1]) 

RP_news <- as.vector(c(1,0,0,0,0,0) %*% lambda %*% t(resid(var_est)))
Ret_news <- as.vector(c(0,1,0,0,0,0) %*% solve(diag(6) - rho * Gamma) %*% t(resid(var_est)))
Div_news <- as.vector(u+Ret_news+RP_news)

#Variance decomposition; terms have to add to 1
table3e=rbind(var(Div_news)/var(u),
              -2*cov(Ret_news, Div_news)/var(u),
              -2*cov(RP_news, Div_news)/var(u),
              var(Ret_news)/var(u),
              2*cov(Ret_news, RP_news)/var(u),
              var(RP_news)/var(u))

#Final table-3
table3=cbind(table3a,table3b,table3c,table3d,table3e)
table3=round(table3, digits =3) #rounding off values

#Rename column names in table3
#colnames(table3)=c('1','2','3','4','5')

#table2 <- as.data.table(table2)
#table3 <- as.data.table(table3)

#write csv files
#write.csv(table3, file = "CA_table3.csv", row.names = F,col.names = T)
#write.csv(table2, file = "CA_table2.csv", row.names = F,col.names = T)

#Print tables into LaTeX
print(xtable(table2), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x}) #table2 should be a matrix class #use include.rownames=F to remove row names
print(xtable(table3), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x})



## ============================================================================== 
##                            Campbell and Shiller (1991) Replication 
## ============================================================================== 


CSdata=read.csv("CSData.csv")
CSdata=subset(CSdata, select = -c(X5.month.rate)) #delete X5.month.rate column

## ============================================================================== 
##                                Create Table-1(a)
##                  Regression of change of bond yield on term spread:
## ============================================================================== 

nmonths=c(2,3,4,6,9,12,24,36,48,60,120)

#create dataset for dependent variables: change in bond yields
CSdata1=matrix(NA,nrow=nrow(CSdata)-1,ncol=ncol(CSdata)-3)
for(j in 4:14){
  for(i in 1:(nrow(CSdata)-1)){
    CSdata1[i,j-3]=CSdata[i+1,j]-CSdata[i,j]
  }
}

#create dataset for independent variables: yield spreads
CSdata2=matrix(NA,nrow=nrow(CSdata)-1,ncol=ncol(CSdata)-3)
for(j in 4:14){
  for(i in 1:(nrow(CSdata)-1)){
    CSdata2[i,j-3]=(1/(nmonths[j-3]-1))*(CSdata[i,j]-CSdata[i,3])
  }
}

CSdata3=cbind(CSdata1,CSdata2)
#rename columns
colnames(CSdata3)=c(paste0("r", 1:11),paste0("s", 1:11))


table1a=matrix(NA,nrow=1,ncol=1)
for(i in 1:11){
  res=lm(CSdata3[,i]~CSdata3[,i+11])
  HH=kernHAC(res, kernel = "Truncated", bw = 1,
          prewhite = FALSE, adjust = FALSE, sandwich = FALSE)
  table1a=rbind(table1a,coef(res)[2],HH[2,1])
}
table1a=na.omit(table1a)
table1a=round(table1a, digits =3) #rounding off values


#add parentheses to HH std errors
for(k in 1:(nrow(table1a)/2)){    
  table1a[2*k,] <- paste0("(", format(unlist(table1a[2*k,])),")") 
}

#rename column and row names
colnames(table1a)=c('m=1')
rownames(table1a)=c("2"," ","3", " ","4"," ", "6"," ", "9"," ", "12"," ",
                    "24"," ", "36"," ", "48"," ", "60"," ", "120"," ")

## ============================================================================== 
##                                Create Table-2
##          Regression of perfect-foresight spread on actual term spread:
## ============================================================================== 

nmonths=c(2,3,4,6,9,12,24,36,48,60,120)

#create dataset for dependent variables: perfect-foresight spread
CSdata4=matrix(NA,nrow=nrow(CSdata)-1,ncol=ncol(CSdata)-3)
for(j in 4:14){
  for(t in 1:(nrow(CSdata)-1)){
    n=nmonths[j-3]
    sum=0
    for(i in 1:n){
      c=(1-(i/n))*(CSdata[t+i,3]-CSdata[t,3])
      sum=sum+c
    }
    CSdata4[t,j-3]=sum
  }
}

#create dataset for independent variables: term spread
CSdata5=matrix(NA,nrow=nrow(CSdata)-1,ncol=ncol(CSdata)-3)
for(j in 4:14){
  for(t in 1:(nrow(CSdata)-1)){
    i=nmonths[j-3]
    CSdata5[t,j-3]=(0.5)*(CSdata[t+i,3]-CSdata[t,3])
  }
}

CSdata6=cbind(CSdata4,CSdata5)
#rename columns
colnames(CSdata6)=c(paste0("r", 1:11),paste0("s", 1:11))

table2_CS=matrix(NA,nrow=1,ncol=1)
for(i in 1:11){
  res=lm(CSdata6[,i]~CSdata6[,i+11])
  HH=kernHAC(res, kernel = "Truncated", bw = 1,
             prewhite = FALSE, adjust = FALSE, sandwich = FALSE)
  table2_CS=rbind(table2_CS,coef(res)[2],HH[2,1])
}
table2_CS=na.omit(table2_CS)
table2_CS=round(table2_CS, digits =3) #rounding off values


#add parentheses to HH std errors
for(k in 1:(nrow(table2_CS)/2)){    
  table2_CS[2*k,] <- paste0("(", format(unlist(table2_CS[2*k,])),")") 
}

#rename column and row names
colnames(table2_CS)=c('m=1')
rownames(table2_CS)=c("2"," ","3", " ","4"," ", "6"," ", "9"," ", "12"," ",
                    "24"," ", "36"," ", "48"," ", "60"," ", "120"," ")



## ============================================================================== 
##                                Create Table-3
##          Regression of perfect-foresight spread on actual term spread:
## ============================================================================== 

#create a variable change in yields
CSdata7=matrix(NA,nrow=nrow(CSdata),ncol=ncol(CSdata)-3)
for(j in 4:14){
  for(i in 2:(nrow(CSdata))){
    CSdata7[i,j-3]=CSdata[i,j]-CSdata[i-1,j]
  }
}
CSdata7=na.omit(CSdata7)

CSdata8=cbind(CSdata7,CSdata5)
#rename columns
colnames(CSdata8)=c(paste0("r", 1:11),paste0("s", 1:11))


#Estimate theoritical spreads for all n, (m=1)
## State vector: 
CSdata10=matrix(NA,nrow=nrow(CSdata),ncol=1)
for(j in 1:11){ 
  vec_state <-CSdata8[ ,c(j,11+j)]
  vec_state=na.omit(vec_state)
  var_est <- VAR(vec_state, p=1, type="none")
  summary(var_est)
  
  theta <- 0.95
  delta <- 0.95
  Gamma <- t(sapply(coef(var_est), FUN=function(df) {df[1:2, 1]})) #coeff matrix of 
  theo_spread <- as.vector(c(0,1) %*% solve(diag(2) - delta * Gamma) %*% t(resid(var_est)))
  
  CSdata9=matrix(theo_spread,nrow=nrow(CSdata),ncol=1) #convert list to matrix
  CSdata10=cbind(CSdata10,CSdata9)
}

CSdata10=CSdata10[,2:12]  #delete v1 column
CSdata10=CSdata10[1:421,]

CSdata11=cbind(CSdata10,CSdata5)
#rename columns
colnames(CSdata11)=c(paste0("t", 1:11),paste0("s", 1:11))


table3_CS=matrix(NA,nrow=1,ncol=1)
for(i in 1:11){
  res=cor(CSdata11[,i],CSdata11[,i+11],use="complete.obs")
  table3_CS=rbind(table3_CS,res)
}
table3_CS=na.omit(table3_CS)
table3_CS=round(table3_CS, digits =3) #rounding off values


#rename column and row names
colnames(table3_CS)=c('m=1')
rownames(table3_CS)=c("2","3", "4", "6", "9", "12",
                      "24","36", "48","60", "120")


#Print tables into LaTeX
print(xtable(table1a), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x}) #table2 should be a matrix class #use include.rownames=F to remove row names
print(xtable(table2_CS), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x})
print(xtable(table3_CS), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x})



## ============================================================================== 
##                           Fama and Bliss (1987) Replication 
## ============================================================================== 


FBdata=read.csv("FamaBliss1.csv") #yield data
FBdata=na.omit(FBdata)
FBdata$Date=as.Date(format(FBdata$Date),"%m/%d/%Y") #convert character to date
FBdata$Year=year(FBdata$Date)
FBdata$Month=month(FBdata$Date)
FBdata <- FBdata[order(FBdata$Year,FBdata$Month),] #sorting

FBdata1=read.csv("FamaBliss2.csv") #price data
FBdata1=na.omit(FBdata1)
FBdata1$Date=as.Date(format(FBdata1$Date),"%m/%d/%Y") #convert character to date
FBdata1$Year=year(FBdata1$Date)
FBdata1$Month=month(FBdata1$Date)
FBdata1 <- FBdata1[order(FBdata1$Year,FBdata1$Month),] #sorting

#merge the above datasets
FBdata2=cbind(FBdata,FBdata1)

## ============================================================================== 
##                                Create Table-1
##                          Term Premium Regressions
## ============================================================================== 

nmonths=c(2,3,4,6,9,12,24,36,48,60,120)

#create dataset for dependent variables: 1 year ret
FBdata3=matrix(NA,nrow=nrow(FBdata1)-12,ncol=5)
for(j in 1:5){
  for(i in 1:(nrow(FBdata1)-12)){
    FBdata3[i,j]=((FBdata1[i+12,j+1]-FBdata1[i,j+1])/(FBdata1[i,j+1]))*100
  }
}
colnames(FBdata3)=c('h1','h2','h3','h4','h5')

#create dataset for independent variables: 1 year forward rate
FBdata4=matrix(NA,nrow=nrow(FBdata)-12,ncol=4)
for(j in 1:4){
  for(i in 1:(nrow(FBdata)-12)){
    FBdata4[i,j]=FBdata[i,j+2]-FBdata[i,j+1]
  }
}
colnames(FBdata4)=c('f2','f3','f4','f5')

#merge the above datasets
FBdata5=cbind(FBdata3,FBdata4,FBdata[1:252,])

#create dataset for regression
FBdata6=matrix(NA,nrow=nrow(FBdata5),ncol=8)
for(j in 1:4){
  for(i in 1:(nrow(FBdata5))){
    FBdata6[i,j]=FBdata5[i,j+1]-FBdata5[i,11]
    FBdata6[i,j+4]=FBdata5[i,j+5]-FBdata5[i,11]
  }
}
colnames(FBdata6)=c('h2','h3','h4','h5','f2','f3','f4','f5')


table1_FB=matrix(NA,nrow=1,ncol=10)
for(i in 1:4){
  res=lm(FBdata6[,i]~FBdata6[,i+4])
  
  #lag residuals
  lag1=lag(res$residuals,n=12)
  lag2=lag(res$residuals,n=24)
  lag3=lag(res$residuals,n=36)
  lag4=lag(res$residuals,n=48)
  lag5=lag(res$residuals,n=60)
  
  
  table1_FB=rbind(table1_FB,cbind(coef(res)[1],coef(summary(res))[, 2][1], coef(res)[2],
                  coef(summary(res))[, 2][2], summary(res)$r.squared,
                  cor(res$residuals,lag1,use="complete.obs"),cor(res$residuals,lag2,use="complete.obs"),
                  cor(res$residuals,lag3,use="complete.obs"),cor(res$residuals,lag4,use="complete.obs"),
                  cor(res$residuals,lag5,use="complete.obs")))
}
table1_FB=na.omit(table1_FB)
table1_FB=round(table1_FB, digits =3) #rounding off values

## ============================================================================== 
##                                Create Table-3
##                   REGRESSION FORECASTS OF THE CHANGE IN THE SPOT RATE
## ============================================================================== 

#create dataset for dependent variables: change in spot rate
FBdata7=matrix(NA,nrow=nrow(FBdata5),ncol=8)
for(j in 1:4){
  for(i in 1:(nrow(FBdata5))){
    FBdata7[i,j]=FBdata5[i+(12)*j,11]-FBdata5[i,11]
    FBdata7[i,j+4]=FBdata5[i,j+5]-FBdata5[i,11]
  }
}
colnames(FBdata7)=c('r1','r2','h3','h4','f2','f3','f4','f5')

table3_FB=matrix(NA,nrow=1,ncol=10)
for(i in 1:4){
  res=lm(FBdata7[,i]~FBdata7[,i+4])
  
  #lag residuals
  lag1=lag(res$residuals,n=12)
  lag2=lag(res$residuals,n=24)
  lag3=lag(res$residuals,n=36)
  lag4=lag(res$residuals,n=48)
  lag5=lag(res$residuals,n=60)
  
  
  table3_FB=rbind(table3_FB,cbind(coef(res)[1],coef(summary(res))[, 2][1], coef(res)[2],
                                  coef(summary(res))[, 2][2], summary(res)$r.squared,
                                  cor(res$residuals,lag1,use="complete.obs"),cor(res$residuals,lag2,use="complete.obs"),
                                  cor(res$residuals,lag3,use="complete.obs"),cor(res$residuals,lag4,use="complete.obs"),
                                  cor(res$residuals,lag5,use="complete.obs")))
}
table3_FB=na.omit(table3_FB)
table3_FB=round(table3_FB, digits =3) #rounding off values



#Print tables into LaTeX
print(xtable(table1_FB), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x}) #table1 should be a matrix class #use include.rownames=F to remove row names

print(xtable(table3_FB), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x})

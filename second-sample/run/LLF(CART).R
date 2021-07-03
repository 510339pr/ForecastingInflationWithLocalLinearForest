source("functions/func-LLF(CART).R")
library(HDeconometrics)
library(randomForest)
library(grf)
library(glmnet)

Y=dados
dum=rep(0,nrow(Y))
dum[which.min(Y[,1])]=1
Y=cbind(Y,dum=dum)

nprev=200 #amount of predictions made in the the first sub sample => take one extra inorder to initialize lambda 

set.seed(123)
sample2.llf1c=llf.rolling.window(Y,nprev,1,1) # 1 voor CPI
sample2.llf1p=llf.rolling.window(Y,nprev,2,1) # 2 voor PCE
sample2.llf2c=llf.rolling.window(Y,nprev,1,2)
sample2.llf2p=llf.rolling.window(Y,nprev,2,2)
sample2.llf3c=llf.rolling.window(Y,nprev,1,3)
sample2.llf3p=llf.rolling.window(Y,nprev,2,3)
sample2.llf4c=llf.rolling.window(Y,nprev,1,4)
sample2.llf4p=llf.rolling.window(Y,nprev,2,4)
sample2.llf5c=llf.rolling.window(Y,nprev,1,5)
sample2.llf5p=llf.rolling.window(Y,nprev,2,5)
sample2.llf6c=llf.rolling.window(Y,nprev,1,6)
sample2.llf6p=llf.rolling.window(Y,nprev,2,6)
sample2.llf7c=llf.rolling.window(Y,nprev,1,7)
sample2.llf7p=llf.rolling.window(Y,nprev,2,7)
sample2.llf8c=llf.rolling.window(Y,nprev,1,8)
sample2.llf8p=llf.rolling.window(Y,nprev,2,8)
sample2.llf9c=llf.rolling.window(Y,nprev,1,9)
sample2.llf9p=llf.rolling.window(Y,nprev,2,9)
sample2.llf10c=llf.rolling.window(Y,nprev,1,10)
sample2.llf10p=llf.rolling.window(Y,nprev,2,10) 
sample2.llf11c=llf.rolling.window(Y,nprev,1,11)
sample2.llf11p=llf.rolling.window(Y,nprev,2,11)
sample2.llf12c=llf.rolling.window(Y,nprev,1,12)
sample2.llf12p=llf.rolling.window(Y,nprev,2,12)

# # print evaluation metrics to excel document # # 
a = matrix(0,3,12)
model = "llf"
sample = "sample2."

# pce
for (i in 1:12){
  a[1,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics"), collapse = "")))[1]
  a[2,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics"), collapse = "")))[2]
  a[3,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics"), collapse = "")))[3]
}
# cpi 
for (i in 1:12){
  a[1,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics"), collapse = "")))[1]
  a[2,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics"), collapse = "")))[2]
  a[3,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics"), collapse = "")))[3]
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)


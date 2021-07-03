source("functions/func-LLF(CART).R")
library(HDeconometrics)
library(randomForest)
library(grf)
library(glmnet)
library(ggplot2)

Y=dados

# amount of predictions made in the the first sub sample => take 20 extra inorder to initialize lambda 
nprev=152 

# # determine forecasts # #
set.seed(123)
sample1.llf1c=llf.rolling.window(Y,nprev,1,1) # 1 voor CPI
sample1.llf1p=llf.rolling.window(Y,nprev,2,1) # 2 voor PCE
sample1.llf2c=llf.rolling.window(Y,nprev,1,2)
sample1.llf2p=llf.rolling.window(Y,nprev,2,2)
sample1.llf3c=llf.rolling.window(Y,nprev,1,3)
sample1.llf3p=llf.rolling.window(Y,nprev,2,3)
sample1.llf4c=llf.rolling.window(Y,nprev,1,4)
sample1.llf4p=llf.rolling.window(Y,nprev,2,4)
sample1.llf5c=llf.rolling.window(Y,nprev,1,5)
sample1.llf5p=llf.rolling.window(Y,nprev,2,5)
sample1.llf6c=llf.rolling.window(Y,nprev,1,6)
sample1.llf6p=llf.rolling.window(Y,nprev,2,6)
sample1.llf7c=llf.rolling.window(Y,nprev,1,7)
sample1.llf7p=llf.rolling.window(Y,nprev,2,7)
sample1.llf8c=llf.rolling.window(Y,nprev,1,8)
sample1.llf8p=llf.rolling.window(Y,nprev,2,8)
sample1.llf9c=llf.rolling.window(Y,nprev,1,9)
sample1.llf9p=llf.rolling.window(Y,nprev,2,9)
sample1.llf10c=llf.rolling.window(Y,nprev,1,10)
sample1.llf10p=llf.rolling.window(Y,nprev,2,10)
sample1.llf11c=llf.rolling.window(Y,nprev,1,11)
sample1.llf11p=llf.rolling.window(Y,nprev,2,11)
sample1.llf12c=llf.rolling.window(Y,nprev,1,12)
sample1.llf12p=llf.rolling.window(Y,nprev,2,12)

# # print evaluation metrics to excel document # # 
a = matrix(0,3,12)
model = "llf"
sample = "sample1."

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









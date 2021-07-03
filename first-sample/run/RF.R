source("functions/func-RF.R")
library(HDeconometrics)
library(randomForest)
library(grf)
library(glmnet)

Y=dados

# amount of predictions made in the the first sub sample => take 20 extra inorder to initialize lambda 
nprev=152 

# # determine forecasts # # 
set.seed(123)
sample1.rf1c=rf.rolling.window(Y,nprev,1,1) # 1 voor CPI
sample1.rf1p=rf.rolling.window(Y,nprev,2,1) # 2 voor PCE
sample1.rf2c=rf.rolling.window(Y,nprev,1,2) 
sample1.rf2p=rf.rolling.window(Y,nprev,2,2)
sample1.rf3c=rf.rolling.window(Y,nprev,1,3)
sample1.rf3p=rf.rolling.window(Y,nprev,2,3)
sample1.rf4c=rf.rolling.window(Y,nprev,1,4)
sample1.rf4p=rf.rolling.window(Y,nprev,2,4)
sample1.rf5c=rf.rolling.window(Y,nprev,1,5)
sample1.rf5p=rf.rolling.window(Y,nprev,2,5)
sample1.rf6c=rf.rolling.window(Y,nprev,1,6)
sample1.rf6p=rf.rolling.window(Y,nprev,2,6)
sample1.rf7c=rf.rolling.window(Y,nprev,1,7)
sample1.rf7p=rf.rolling.window(Y,nprev,2,7)
sample1.rf8c=rf.rolling.window(Y,nprev,1,8)
sample1.rf8p=rf.rolling.window(Y,nprev,2,8)
sample1.rf9c=rf.rolling.window(Y,nprev,1,9)
sample1.rf9p=rf.rolling.window(Y,nprev,2,9)
sample1.rf10c=rf.rolling.window(Y,nprev,1,10)
sample1.rf10p=rf.rolling.window(Y,nprev,2,10)
sample1.rf11c=rf.rolling.window(Y,nprev,1,11)
sample1.rf11p=rf.rolling.window(Y,nprev,2,11)
sample1.rf12c=rf.rolling.window(Y,nprev,1,12)
sample1.rf12p=rf.rolling.window(Y,nprev,2,12)


# # print evaluation metrics to excel document # # 
a = matrix(0,3,12)
model = "rf"
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




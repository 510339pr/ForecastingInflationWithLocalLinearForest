source("functions/func-LLF(LL).R")
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
sample1.llf1c_ll_split=llf.rolling.window(Y,nprev,1,1) # 1 voor CPI
sample1.llf1p_ll_split=llf.rolling.window(Y,nprev,2,1) # 2 voor PCE
sample1.llf2c_ll_split=llf.rolling.window(Y,nprev,1,2)
sample1.llf2p_ll_split=llf.rolling.window(Y,nprev,2,2)
sample1.llf3c_ll_split=llf.rolling.window(Y,nprev,1,3)
sample1.llf3p_ll_split=llf.rolling.window(Y,nprev,2,3)
sample1.llf4c_ll_split=llf.rolling.window(Y,nprev,1,4)
sample1.llf4p_ll_split=llf.rolling.window(Y,nprev,2,4)
sample1.llf5c_ll_split=llf.rolling.window(Y,nprev,1,5)
sample1.llf5p_ll_split=llf.rolling.window(Y,nprev,2,5)
sample1.llf6c_ll_split=llf.rolling.window(Y,nprev,1,6)
sample1.llf6p_ll_split=llf.rolling.window(Y,nprev,2,6)
sample1.llf7c_ll_split=llf.rolling.window(Y,nprev,1,7)
sample1.llf7p_ll_split=llf.rolling.window(Y,nprev,2,7)
sample1.llf8c_ll_split=llf.rolling.window(Y,nprev,1,8)
sample1.llf8p_ll_split=llf.rolling.window(Y,nprev,2,8)
sample1.llf9c_ll_split=llf.rolling.window(Y,nprev,1,9)
sample1.llf9p_ll_split=llf.rolling.window(Y,nprev,2,9)
sample1.llf10c_ll_split=llf.rolling.window(Y,nprev,1,10)
sample1.llf10p_ll_split=llf.rolling.window(Y,nprev,2,10)
sample1.llf11c_ll_split=llf.rolling.window(Y,nprev,1,11)
sample1.llf11p_ll_split=llf.rolling.window(Y,nprev,2,11)
sample1.llf12c_ll_split=llf.rolling.window(Y,nprev,1,12)
sample1.llf12p_ll_split=llf.rolling.window(Y,nprev,2,12)

# # print evaluation metrics to excel document # # 
a = matrix(0,3,12)
model = "llf"
sample = "sample1."

# pce
for (i in 1:12){
  a[1,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics", "_ll_split"), collapse = "")))[1]
  a[2,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics", "_ll_split"), collapse = "")))[2]
  a[3,i]=eval(parse(text=paste(c(sample,model,i,"p", "$error.metrics", "_ll_split"), collapse = "")))[3]
}
# cpi 
for (i in 1:12){
  a[1,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics", "_ll_split"), collapse = "")))[1]
  a[2,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics", "_ll_split"), collapse = "")))[2]
  a[3,i]=eval(parse(text=paste(c(sample,model,i,"c", "$error.metrics", "_ll_split"), collapse = "")))[3]
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)









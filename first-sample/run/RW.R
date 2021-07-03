require(zoo)

# run random walk 

rw.pred = function(Y,nprev, indice = 1, lag_value = 1){
  
  #predictions and real value
  test = cbind(Y[,indice],lag(zoo(Y[,indice]),-lag_value))
  
  #compute errors
  error = tail(test[,1]-test[,2],nprev)
  rmse = sqrt(mean(error^2))
  mae = mean(abs(error))
  mad = median(abs(error - median(error))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)

  return(list("error.metrics"=error.metrics, "errors"=error))

}

nprev = 132
set.seed(123)
sample1.rw1c=rw.pred(Y,nprev,1,1) # 1 voor CPI
sample1.rw1p=rw.pred(Y,nprev,2,1) # 2 voor PCE
sample1.rw2c=rw.pred(Y,nprev,1,2)
sample1.rw2p=rw.pred(Y,nprev,2,2)
sample1.rw3c=rw.pred(Y,nprev,1,3)
sample1.rw3p=rw.pred(Y,nprev,2,3)
sample1.rw4c=rw.pred(Y,nprev,1,4)
sample1.rw4p=rw.pred(Y,nprev,2,4)
sample1.rw5c=rw.pred(Y,nprev,1,5)
sample1.rw5p=rw.pred(Y,nprev,2,5)
sample1.rw6c=rw.pred(Y,nprev,1,6)
sample1.rw6p=rw.pred(Y,nprev,2,6)
sample1.rw7c=rw.pred(Y,nprev,1,7)
sample1.rw7p=rw.pred(Y,nprev,2,7)
sample1.rw8c=rw.pred(Y,nprev,1,8)
sample1.rw8p=rw.pred(Y,nprev,2,8)
sample1.rw9c=rw.pred(Y,nprev,1,9)
sample1.rw9p=rw.pred(Y,nprev,2,9)
sample1.rw10c=rw.pred(Y,nprev,1,10)
sample1.rw10p=rw.pred(Y,nprev,2,10)
sample1.rw11c=rw.pred(Y,nprev,1,11)
sample1.rw11p=rw.pred(Y,nprev,2,11)
sample1.rw12c=rw.pred(Y,nprev,1,12)
sample1.rw12p=rw.pred(Y,nprev,2,12)


# # print evaluation metrics to excel document # # 
a = matrix(0,3,12)
model = "rw"
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

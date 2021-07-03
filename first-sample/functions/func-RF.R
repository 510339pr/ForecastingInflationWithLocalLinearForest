runrf=function(Y,indice,lag){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  model=randomForest(X,y,importance=TRUE)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}


rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  start.time <- Sys.time()
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    lasso=runrf(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  
  errors = tail(real,nprev)-save.pred
  
  rmse=sqrt(mean((errors)^2))
  mae=mean(abs(errors))
  mad = median(abs(errors - median(errors))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  return(list("pred"=save.pred,"error.metrics"=error.metrics,"save.importance"=save.importance, "errors"=errors, "time.taken"=time.taken, "real"=tail(real,nprev)))
}


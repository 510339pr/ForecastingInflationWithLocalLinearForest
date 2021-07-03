runrf=function(Y,indice,lag, y.test,vip_old,obs_nr){
  
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
  
  #start variable importance stuff DALEX
  
  predict_rf = function(model,X.newdata){
    return(predict(model,X.newdata))
  }
  
  # # aggregate vip
  set.seed(123)
  if(obs_nr >= 1){
    
    explainer_rf <- DALEX::explain(
      model,
      data = X,
      y = y,
      predict_function = predict_rf,
      label = "randomForest RF"
    )
    
    vip_new = model_parts(explainer = explainer_rf, loss_function = loss_root_mean_square, B = 1)
    
    if (obs_nr > 1 ){
      for (i in vip_new$variable){
        
        index_new = which(vip_new$variable==i)
        index_old = which(vip_old$variable==i)
        vip_old$dropout_loss[index_old] = vip_old$dropout_loss[index_old] + vip_new$dropout_loss[index_new]
        
      }
    } 
    else {
      vip_old = vip_new
    }
  }
  # end variable importance stuff 
 
  
  return(list("model"=model,"pred"=pred ,"vip"=vip_old))
}


rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  start.time <- Sys.time()
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  
  vip_aggregated = matrix(0,1,1)
  
  for(i in nprev:1){
    obs_nr = 1+nprev-i
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    y.test = Y[(nrow(Y)-i+1),indice]
    lasso=runrf(Y.window,indice,lag,y.test,vip_aggregated,obs_nr)
    save.pred[(1+nprev-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+nprev-i),"\n")
    
    vip_aggregated = lasso$vip
    
  }
  
  real=Y[,indice]
  
  errors = tail(real,nprev)-save.pred
  
  rmse=sqrt(mean((errors)^2))
  mae=mean(abs(errors))
  mad = median(abs(errors - median(errors))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  return(list("pred"=save.pred,"error.metrics"=error.metrics,"save.importance"=save.importance, "errors"=errors, "time.taken"=time.taken, "real"=tail(real,nprev), "vip_aggregated"=vip_aggregated))
}


runllf=function(Y,indice,lag,y.test,total_pred_error_lambda, best_lambda, splits_freqs_old, vip_old, obs_nr){
  
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))] 
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]
    Y.real=tail(aux,1)[1:ncol(X)]
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  X.test = t(matrix((X.out)))
  
  # local linear correction variable selection
  type = "lasso"
  alpha = 1
  
  model=ic.glmnet(X,y,alpha = alpha)
  coef=model$coef
  if(type=="adalasso"){
    penalty=(abs(coef[-1])+1/sqrt(length(y)))^(-1)
    model=ic.glmnet(X,y,penalty.factor = penalty,alpha=alpha)
  }
  selected=which(model$coef[-1]!=0)
  if(length(selected)<2){
    selected=1:2
  }
  
  # train the model => split with or without Local Linear Split
  model=ll_regression_forest(X,y,enable.ll.split = F, ll.split.variables = selected, ll.split.weight.penalty = T, ll.split.lambda = 1) 
  
  # save split frequencies
  max.depth <- 5
  splits_freqs = split_frequencies(model, max.depth = max.depth)
  splits_freqs = data.frame(splits_freqs)
  splits_freqs_new = splits_freqs + splits_freqs_old
  
  # lambda tuning + prediction 
  lambda = c(0,0.001,0.010,0.1,1)
  pred_matrix = matrix(NA,length(lambda),1)
  pred_error_matrix = matrix(NA,length(lambda),1)
  
  for (i in 1:length(lambda)){
    pred_matrix[i,1] = predict(model,X.test,linear.correction.variables = selected, ll.lambda=lambda[i])[1,1]
    pred_error_matrix[i,1] = abs(y.test - pred_matrix[i,1])
  }
  
  total_pred_error_lambda[,1] =  total_pred_error_lambda[,1] + pred_error_matrix[,1] 
  pred = pred_matrix[best_lambda,1] 
  
  
  # variable importance stuff
  predict_grf = function(model,X.newdata){
    return(predict(model,X.newdata,linear.correction.variables = selected,best_lambda)[,1])
  }
  
  # start variable importance stuff DALEX
  
  # # aggregate vip
  set.seed(123)
  if(obs_nr >= 20){
    
    explainer_rf_grf <- DALEX::explain(
      model,
      data = X,
      y = y,
      predict_function = predict_grf,
      label = "grf RF"
    )
    
    vip_new = model_parts(explainer = explainer_rf_grf, loss_function = loss_root_mean_square, B = 1)
    
    if (obs_nr > 20){
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
  
  return(list("model"=model,"pred"=pred, "total_pred_error_lambdas"=total_pred_error_lambda, "pred_error_matrix"= pred_error_matrix, "split_freqs" = splits_freqs_new, "vip"=vip_old))
}

llf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  start.time <- Sys.time()
  
  # initialization 
  amount_lambdas = 5
  total_pred_error_lambdas = matrix(0,amount_lambdas,1) 
  all_lambda_errors = matrix(0,amount_lambdas,nprev)
  moving_window_lambda = 20
  best_lambda = 1
  
  # matrix for splitting 
  freqs = matrix(0,5,508)
  freqs = data.frame(freqs)
  
  vip_aggregated = matrix(0,1,1)
  
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    obs_nr = 1+nprev-i
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    y.test.value = Y[(nrow(Y)-i+1),indice] 
    lasso=runllf(Y.window,indice,lag,y.test.value,total_pred_error_lambdas,best_lambda,freqs,vip_aggregated,obs_nr)
    save.pred[(1+nprev-i),] = matrix(lasso$pred)
    total_pred_error_lambdas = lasso$total_pred_error_lambdas
    all_lambda_errors[,nprev-i+1] = lasso$pred_error_matrix
    
    # determine the amount of splits 
    if (nprev-i>moving_window_lambda){
      freqs = lasso$split_freqs
    }else{
      freqs = matrix(0,5,508)
      freqs = data.frame(freqs)
    }
    
    # determine best lambda 
    error_lambda_sum = c(amount_lambdas)
    if(nprev-i+1>moving_window_lambda){
      error_lambda_sum = rowSums(all_lambda_errors[,(nprev-i+1-moving_window_lambda+1):(nprev-i+1)])#(nprev-i+1-moving_window_lambda):(nprev-i+1)
    }
    best_lambda = which.min(error_lambda_sum)
    print(error_lambda_sum)
    print(best_lambda)
    
    vip_aggregated = lasso$vip
    
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real = Y[,indice]
  real = matrix(real)
  
  errors = (tail(real,(nprev-moving_window_lambda))-save.pred[-(1:moving_window_lambda),1]) 
  
  rmse=sqrt(mean(errors^2)) 
  mae=mean(abs(errors))
  mad = median(abs(errors - median(errors))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  return(list("pred"=save.pred[-(1:moving_window_lambda),1],"error.metrics"=error.metrics, "errors"=errors, "time.taken" = time.taken, "real" = tail(real,(nprev-moving_window_lambda)), "freqs" = freqs, "vip_aggregated"=vip_aggregated))
}


library("ForecastComb")

# building moving window
# it trains linear regression with 30 observations, after which 1 step ahead "forecast" is made 

combined.rolling.pred = function(pred_rf, pred_llf,real,moving.window,npred){
  
  npred = npred 
  moving_window = moving.window
  
  save_pred = matrix(0,npred-moving_window,1)
  
  for (i in 1:(npred-moving_window)){
    
    first_obs = i
    last_obs = i + moving_window -1
    
    # training 
    train_rf = matrix(0,first_obs:last_obs,1)
    train_llf = matrix(0,first_obs:last_obs,1)
    train_rf = pred_rf[first_obs:last_obs,1]
    train_llf = pred_llf[first_obs:last_obs]
    train_matrix_models = matrix(0,moving_window,2)
    train_matrix_models[,1] = train_rf
    train_matrix_models[,2] = train_llf
    
    # test 
    test_rf = pred_rf[last_obs+1,1]
    test_llf = pred_llf[last_obs+1]
    test_matrix_models = matrix(0,1,2)
    test_matrix_models[,1] = test_rf
    test_matrix_models[,2] = test_llf
    
    # real 
    real_train = real[first_obs:last_obs,1]
    set.seed(123)
    # compute prediction
    all_data = foreccomb(real_train,train_matrix_models, newpreds = test_matrix_models)
    test = auto_combine(all_data, criterion = 'RMSE', param_list = NULL)
    save_pred[i,1] = test$Forecasts_Test
    
    print(test$Method)
    
    cat("iteration",i,"\n")
    
  }
  
  errors = tail(real,npred-moving.window) - save_pred 
  
  rmse=sqrt(mean(errors^2)) 
  mae=mean(abs(errors))
  mad = median(abs(errors - median(errors))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)
  
  return(list("pred"=save_pred, "errors" = errors, "error.metrics" = error.metrics))
  
}

# run for sample 1 and 2 for CPI as well as PCE

inflation.metric = "c"
npred = 180
sample = "sample2."

for (i in 1:12){
  
  # # # # select models # # # #
  
  m_sample_rf = eval(parse(text=paste(c(sample,"rf",i,inflation.metric), collapse = "")))
  m_sample_llf = eval(parse(text=paste(c(sample,"llf",i,inflation.metric), collapse = "")))
  
  # # # # # predictions # # # # #
  
  pred_values_rf = matrix(0,length(m_sample_rf$errors),1)
  pred_values_rf[1:npred,1] = m_sample_rf$pred
  
  pred_values_llf = matrix(0,length(m_sample_rf$errors),1)
  pred_values_llf[1:npred,1] = m_sample_llf$pred
  
  
  # # # # # real # # # # #
  
  real_values = matrix(0,length(m_sample_rf$errors),1)
  real_values[1:npred,1] = m_sample_llf$real
  
  # run combined forecast predictions
  model = "cbf"
  name = paste(sample,model,i,inflation.metric, sep = "")
  set.seed(123)
  tmp = combined.rolling.pred(pred_values_rf,pred_values_llf, real_values,30,npred)
  
  assign(name, tmp)
  
}


# # # print combined forecasts to excel document

a = matrix(0,6,12)

# pce
for (i in 1:12){
  #pce
  a[1,i]=eval(parse(text=paste(c(sample,"cbf",i,"p", "$error.metrics"), collapse = "")))[1]
  a[2,i]=eval(parse(text=paste(c(sample,"cbf",i,"p", "$error.metrics"), collapse = "")))[2]
  a[3,i]=eval(parse(text=paste(c(sample,"cbf",i,"p", "$error.metrics"), collapse = "")))[3]
  
  #cpi
  a[4,i]=eval(parse(text=paste(c(sample,"cbf",i,"c", "$error.metrics"), collapse = "")))[1]
  a[5,i]=eval(parse(text=paste(c(sample,"cbf",i,"c", "$error.metrics"), collapse = "")))[2]
  a[6,i]=eval(parse(text=paste(c(sample,"cbf",i,"c", "$error.metrics"), collapse = "")))[3]
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)

# # # get new error estimates LLF & RF

combined_error_metrics = function(errors){
  
  rmse=sqrt(mean(errors^2)) 
  mae=mean(abs(errors))
  mad = median(abs(errors - median(errors))) 
  error.metrics=c("rmse"=rmse,"mae"=mae, "mad"=mad)
  return(error.metrics)
  
}

a = matrix(0,6,12)
inflation.metric = "c"
model = "llf"

for (i in 1:12){
  
  m_sample = eval(parse(text=paste(c("sample2.",model,i,inflation.metric), collapse = "")))

  error.metrics = combined_error_metrics(tail(m_sample$errors,npred-30))
  a[1,i]=error.metrics[1]
  a[2,i]=error.metrics[2]
  a[3,i]=error.metrics[3]
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)


# # # test difference significance and write results to excel document # # # => do twice for sq and abs

set.seed(123)
a = matrix(0,2,12)

for (i in 1:12){
  
  testMatrix = matrix(0,length(sample2.cbf1c$errors),2)
  testMatrix[,1] =  eval(parse(text=paste(c(sample,"cbf",i,"c", "$errors"), collapse = ""))) ** 2
  testMatrix[,2] =   tail(eval(parse(text=paste(c(sample, "llf",i,"c", "$errors"), collapse = ""))),npred-30) ** 2 
  mcs = MCSprocedure(testMatrix)
  a[1,i] = mcs@Info$mcs_pvalue
  
  
  testMatrix = matrix(0,length(sample2.cbf1c$errors),2)
  testMatrix[,1] = abs(  eval(parse(text=paste(c(sample,"cbf",i,"c", "$errors"), collapse = ""))) ) 
  testMatrix[,2] = abs(  tail(eval(parse(text=paste(c(sample, "llf",i,"c", "$errors"), collapse = ""))),npred-30) )
  mcs = MCSprocedure(testMatrix)
  a[2,i] = mcs@Info$mcs_pvalue
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)







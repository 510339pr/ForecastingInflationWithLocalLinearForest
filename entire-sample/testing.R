# # # # # # # # test significance difference between LLF(CART) & RF, summary # # # # # # # # 
set.seed(123)
a = matrix(0,2,12)

inflation.metric = "c"

for (i in 1:12){
  
  # # # get errors each model  
  
  m_sample_1_rf = eval(parse(text=paste(c("sample1.","rf",i,inflation.metric), collapse = "")))
  m_sample_2_rf = eval(parse(text=paste(c("sample2.","rf",i,inflation.metric), collapse = "")))
  
  m_sample_1_llf = eval(parse(text=paste(c("sample1.","llf",i,inflation.metric), collapse = "")))
  m_sample_2_llf = eval(parse(text=paste(c("sample2.","llf",i,inflation.metric), collapse = "")))
  
  error_values_rf = matrix(0,length(m_sample_1_rf$errors) + length(m_sample_2_rf$errors),1)
  error_values_rf[1:132,1] = m_sample_1_rf$errors
  error_values_rf[133:312,1] = m_sample_2_rf$errors
  
  error_values_llf = matrix(0,length(m_sample_1_rf$errors) + length(m_sample_2_rf$errors),1)
  error_values_llf[1:132,1] = m_sample_1_llf$errors
  error_values_llf[133:312,1] = m_sample_2_llf$errors
  
  
  # # # # # real # # # # #
  
  testMatrix = matrix(0,length(error_values_llf),2)
  testMatrix[,1] = (error_values_rf)**2 
  testMatrix[,2] = (error_values_llf)**2
  mcs = MCSprocedure(testMatrix)
  a[1,i] = mcs@Info$mcs_pvalue
  
  testMatrix = matrix(0,length(error_values_llf),2)
  testMatrix[,1] = abs(error_values_rf) 
  testMatrix[,2] = abs(error_values_llf)
  mcs = MCSprocedure(testMatrix)
  a[2,i] = mcs@Info$mcs_pvalue
  
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)


# # # # # # test significance difference between LLF(CART) & RF sample 1 # # # # # 
inflation.metric = "c"

for (i in 1:12){
  
  # # # get errors each model  
  
  m_sample_1_rf = eval(parse(text=paste(c("sample1.","rf",i,inflation.metric), collapse = "")))
 
  
  m_sample_1_llf = eval(parse(text=paste(c("sample1.","llf",i,inflation.metric), collapse = "")))

  
  # # # # # real # # # # #
  
  testMatrix = matrix(0,length(m_sample_1_rf$errors),2)
  testMatrix[,1] = (m_sample_1_rf$errors)**2 
  testMatrix[,2] = (m_sample_1_llf$errors)**2
  mcs = MCSprocedure(testMatrix)
  a[1,i] = mcs@Info$mcs_pvalue
  
  testMatrix = matrix(0,length(m_sample_1_rf$errors),2)
  testMatrix[,1] = abs(m_sample_1_rf$errors)
  testMatrix[,2] = abs(m_sample_1_llf$errors)
  mcs = MCSprocedure(testMatrix)
  a[2,i] = mcs@Info$mcs_pvalue
  
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)

# # # # # # test significance difference between LLF(CART) & RF sample 2 # # # # # 
inflation.metric = "c"

for (i in 1:12){
  
  # # # get errors each model  
  
  m_sample_2_rf = eval(parse(text=paste(c("sample2.","rf",i,inflation.metric), collapse = "")))
  
  
  m_sample_2_llf = eval(parse(text=paste(c("sample2.","llf",i,inflation.metric), collapse = "")))
  
  
  # # # # # real # # # # #
  
  testMatrix = matrix(0,length(m_sample_2_llf$errors),2)
  testMatrix[,1] = (m_sample_2_rf$errors)**2 
  testMatrix[,2] = (m_sample_2_llf$errors)**2
  mcs = MCSprocedure(testMatrix)
  a[1,i] = mcs@Info$mcs_pvalue
  
  testMatrix = matrix(0,length(m_sample_2_llf$errors),2)
  testMatrix[,1] = abs(m_sample_2_rf$errors)
  testMatrix[,2] = abs(m_sample_2_llf$errors)
  mcs = MCSprocedure(testMatrix)
  a[2,i] = mcs@Info$mcs_pvalue
  
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)


# # # # # # test significance difference between LLF(LR) & RF # # # # # 
inflation.metric = "c"


for (i in 1:12){
  
  # # # get errors each model  
  
  m_sample_1_rf = eval(parse(text=paste(c("sample1.","rf",i,inflation.metric), collapse = "")))
  m_sample_2_rf = eval(parse(text=paste(c("sample2.","rf",i,inflation.metric), collapse = "")))
  
  m_sample_1_llf = eval(parse(text=paste(c("sample1.","llf",i,inflation.metric,"_ll_split"), collapse = "")))
  m_sample_2_llf = eval(parse(text=paste(c("sample2.","llf",i,inflation.metric,"_ll_split"), collapse = "")))
  
  error_values_rf = matrix(0,length(m_sample_1_rf$errors) + length(m_sample_2_rf$errors),1)
  error_values_rf[1:132,1] = m_sample_1_rf$errors
  error_values_rf[133:312,1] = m_sample_2_rf$errors
  
  error_values_llf = matrix(0,length(m_sample_1_rf$errors) + length(m_sample_2_rf$errors),1)
  error_values_llf[1:132,1] = m_sample_1_llf$errors
  error_values_llf[133:312,1] = m_sample_2_llf$errors
  
  
  # # # # # real # # # # #
  
  testMatrix = matrix(0,length(error_values_llf),2)
  testMatrix[,1] = (error_values_rf)**2 
  testMatrix[,2] = (error_values_llf)**2
  mcs = MCSprocedure(testMatrix)
  a[1,i] = mcs@Info$mcs_pvalue
  
  testMatrix = matrix(0,length(error_values_llf),2)
  testMatrix[,1] = abs(error_values_rf) 
  testMatrix[,2] = abs(error_values_llf)
  mcs = MCSprocedure(testMatrix)
  a[2,i] = mcs@Info$mcs_pvalue
  
  
}

write.table(a,"print.csv",sep=",",row.names = FALSE, col.names = FALSE)

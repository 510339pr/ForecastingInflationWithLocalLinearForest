options(max.print=2000)
# combine vip 
vip_window_1 = sample1.llf9c_ll_split$vip_aggregated
vip_window_2 = sample2.llf9c_ll_split$vip_aggregated

# adjust vip window 2 (removing the dummy)
test_df = vip_window_2$variable 
test_c = c(1:510) 
remove_variables = list(123, 246, 369, 492) 

for (i in 1:510){
  if(nchar(test_df[i])<=3){
    test_c[i] = strtoi(test_df[i])
  }
}
for(i in remove_variables){
  test_c = test_c[-which(test_c==i)]
}
removed = list(123,245,367,489)
for(r in removed){
  for(i in 1:506){
    if(test_c[i]>r){
      test_c[i] =  test_c[i] - 1 
    }
  }
}
for(i in remove_variables){
  index = which(vip_window_2$variable==toString(i))
  vip_window_2 = vip_window_2[-index, ]
}
vip_window_2$variable[2:505] = test_c[2:505]

testing = vip_window_1

# aggregate the results 
for (i in vip_window_1$variable){
  index_1 = which(vip_window_1$variable==i)
  index_2 = which(vip_window_2$variable==i)
  testing$dropout_loss[index_1] = vip_window_1$dropout_loss[index_1] + vip_window_2$dropout_loss[index_2]
}

LLF_vip = testing

# make boxplot 
vip_df = LLF_vip
vip_df = vip_df[order(LLF_vip$dropout_loss),] 
amount_variables = 496:506


# testing more 
test_df_2 = vip_df$dropout_loss - vip_df$dropout_loss[1] 
amount_variables = 496:505

vip_df$variable[amount_variables]
var_nr = c(1:10)
counter = 1
for(i in amount_variables){
  var_nr[counter] = strtoi(vip_df$variable[i])
  counter = counter + 1
}

test_list = list(1:9)
counter = 0
for(i in var_nr){
  counter = counter + 1
  if(i<=126){
    test_list[counter] = paste(colnames(dados)[i], "(1)", sep=" ")
    print(paste(colnames(dados)[i], "(1)", sep=" "))
  }
  else if(i>126 && i<=252){
    test_list[counter] = paste(colnames(dados)[i-126], "(2)", sep=" ")
    print(paste(colnames(dados)[i-126], "(2)", sep=" "))
  }
  else if(i>252 && i<=378){
    test_list[counter] = paste(colnames(dados)[i-252], "(3)", sep=" ")
    print(paste(colnames(dados)[i-252], "(3)", sep=" "))
  }
  else{
    test_list[counter] = paste(colnames(dados)[i-378], "(4)", sep=" ")
    print(paste(colnames(dados)[i-378], "(4)", sep=" "))
  }
}

grid(nx=NA, ny=NULL)
par(mar = c(4, 5, 0, 2) + 0.1)
barplot(test_df_2[amount_variables],
        # style graph
        main="", horiz=TRUE, names.arg=test_list, cex.names=0.5, las = 1, col = rgb(41, 171, 136, maxColorValue = 255), font.axis = 2
        ,xlim=c(0,0.035), cex.lab = 1, xlab = "RMSE loss compared to full model"
)




# # # get split frequencies data # # # 

# this function searches for variables that have cumulative more than x amount of splits over the 
# entire moving window
split_variables = function(d,split_cut_off,split_dept){
  counter = 0
  for (i in 1:dim(d)[2]){
    column = d[1:split_dept,i]
    if(sum(column)>split_cut_off){
      counter = counter + 1
    }
  }
  counter
  test = matrix(0,max.depth, counter)
  test_df = data.frame(test)
  save_variables = matrix(0,1,counter)
  
  counter_2 = 0
  for (i in 1:dim(d)[2]){
    column = d[1:split_dept,i]
    if(sum(column)>split_cut_off){
      counter_2 = counter_2 + 1
      test_df[,counter_2] = d[,i]
      save_variables[,counter_2] = i
    }
  }
  return(save_variables)
}

# find variables more than x amount of cumulative splits when Local Linear split is used
ll_split_variables = split_variables(sample1.llf9c_ll_split$freqs + sample2.llf9c_ll_split$freqs,20000,3)
# find variables more than x amount of cumulative splits when CART is used
norm_split_variables = split_variables(sample1.llf9c$freqs + sample2.llf9c$freqs,20000,3)

# # add variables that CART splits upon but Local Linear split avoids
for(i in norm_split_variables){
  check = F
  for(j in ll_split_variables){
    if (i == j){
      check = T
    } 
  }
  
  if(check == F){
    print(i)
  }
}

# get variable names 
colnames(dados)[110]
colnames(dados)[62]
colnames(dados)[2]
colnames(dados)[102]

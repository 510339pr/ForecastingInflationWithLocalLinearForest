# ForecastingInflationWithLocalLinearForest

##  first-sample: 
    contains data and code for the first subsample 
###	functions: contains the functions code that compute
		func-LLF(CART): code to run LLF with CART splitting algorithm
 		func-LLF(LL): code to run LLF with Local Linear splitting algorithm
		func-RF: code to run RF
		func-"model"_WITH_VARIMP: computes predictions of models with variable importance measurement 
###	run: contains the code that executes the functions, 
		If you want to run LLF or RF with variable importance then change the file name in the R code. 
###	first_sample_data: R workspace with data loaded 

## second-sample: 
follows the same structure as the first subsample only with second sample data

## entire-sample:
	(first make a workspace that contains the results of the first and second subsample in order the following code)
	aggregateVIP: aggregates variable importance measurements over the 2 subsamples
	combForecasts: combines forecasts with OLS
	graphs: builts the graphs that are given in the paper
	Local_linear_signals: checks what variable is often split upon
	simulationStudy: contains the code to replicate the simulation study
	testing: contains code that preforms the testing 
  
## references
  The code of https://github.com/gabrielrvsc/ForecastingInflation is used in building this repository.
  

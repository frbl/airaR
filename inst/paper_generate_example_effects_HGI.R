library('aira')
library('xtable')
source('inst/generate_test_functions.R')

var_model_100849 <- function() {
  name = '100849'
  model <- testdata_var_model_100849()
  aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = model,
                   orthogonalize= TRUE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  lag <<- model$p
  negative_variables <- c('na_deactivation', 'na_activation', 'stress')
  aira$determine_best_node_from_all(negative_variables = negative_variables)
}

# Check if any of the models give errors
p100849 <-var_model_100849()
print(p100849)

#' @export
convert_var_to_positive <- function(var_model, negative_variables) {
  # create a matrix of ones
  #new_coefs = (Bcoef(var_model) | TRUE) * 1
  # new_coefs
  # negative_variables = c('e', 'U')

  # remove the .l1 and l2 etc terms
  res <- sapply(strsplit(dimnames(Bcoef(var_model))[[2]], '\\.'), '[[', 1)

  # If the var is a negative variable, convert it to -1, ortherwise, convert it to 1
  res <- (((res %in% negative_variables) * -2) + 1)
  for(var_name in dimnames(var_model$y)[[2]]){
    var_model$varresult[[var_name]]$coefficients <- var_model$varresult[[var_name]]$coefficients  * res
  }
  var_model
}

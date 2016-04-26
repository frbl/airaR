#' Converts a var model to a positive notation, returns the updated var model
#'
#' @param var_model the var model that should be converted to positive
#' @param negative_variables the variables that are negative, and should be positivised
#' @param direction the direction to make positive (either outgoing, incomming or both (default))
#' @return the positivised var-model
#' @export
convert_var_to_positive <- function(var_model, negative_variables, direction = 'both') {
  # create a matrix of ones
  #new_coefs = (Bcoef(var_model) | TRUE) * 1
  # new_coefs
  # negative_variables = c('e', 'U')

  # remove the .l1 and l2 etc terms
  print('hoi')
  res <- sapply(strsplit(dimnames(Bcoef(var_model))[[2]], '\\.'), '[[', 1)

  # If the var is a negative variable, convert it to -1, ortherwise, convert it to 1
  res <- (((res %in% negative_variables) * -2) + 1)


  print(res)
  if (direction == 'outgoing') {
    for(var_name in dimnames(var_model$y)[[2]]){
      var_model$varresult[[var_name]]$coefficients <- var_model$varresult[[var_name]]$coefficients  * res
    }
  } else if (direction == 'incomming') {
    for(var_name in negative_variables){
      var_model$varresult[[var_name]]$coefficients <- var_model$varresult[[var_name]]$coefficients  * -1
    }
  } else { # both directions
    for(var_name in dimnames(var_model$y)[[2]]){
      if (var_name %in% negative_variables) {
        var_model$varresult[[var_name]]$coefficients <- var_model$varresult[[var_name]]$coefficients  * -1
      } else {
        var_model$varresult[[var_name]]$coefficients <- var_model$varresult[[var_name]]$coefficients  * res
      }
    }
  }
  var_model
}

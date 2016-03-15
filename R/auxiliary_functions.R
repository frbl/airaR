#' Creates a square matrix of size names, with a default value and named rows/columns
#'
#' @param names the names to use for the column names
#' @param default_value the value to set in each field of the matrix (default 0)
#' @return a length(names) by length(names) matrix
#' @export
create_result_matrix = function(names, default_value = 0) {
  K <- length(names)
  result_matrix <- matrix(rep(default_value, K*K), nrow=K, ncol=K)
  colnames(result_matrix) <- names
  rownames(result_matrix) <- names
  result_matrix
}

#' This is terrible practice. Irf requires the exogen matrix to be available, which is not available anymore.
#' Here we recreate the matrix by padding the datamat with zeros.
#'
#' @param model the model for which to set the exogen matrix in the global space
#' @export
set_exo <- function(model) {
  # TODO: Fix this!
  pattern <- paste(dimnames(model$y)[[2]], collapse='|')

  pattern <- paste('const', pattern, sep='|')
  endogen <- grepl(pattern, names(model$datamat), perl=TRUE)
  exogedata <<- model$datamat[names(model$datamat)[!endogen]]
  for ( i in 1:model$p) {
    exogedata <<- rbind(rep(0, length(!endogen)), exogedata)
  }
}

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

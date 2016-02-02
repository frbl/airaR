context('convert_var_to_positive')

testdata_var_model <- function() {
  data(Canada)
  data_set <- Canada
  vars::VAR(data_set, p=2, type='const')
}

testdata_negative_variables <- c('U', 'rw')

test_that('converts a var model to its positive representation', {
  original_dataset <- (testdata_var_model())

  result <- convert_var_to_positive(testdata_var_model(), testdata_negative_variables)
  # Something should have changed in the dataset
  differences <- (abs(Bcoef(result) - Bcoef(original_dataset)) > 0.001)
  expect_true(any(differences))

  true_columns <- c("U.l1", "U.l2", "rw.l1", "rw.l2")
  false_columns <- c("e.l1", "e.l2", "prod.l1", "prod.l2", "const")

  # Test if we tested all variables
  number_of_vars <- length(dimnames(Bcoef(original_dataset))[[2]])
  expect_equal(length(c(true_columns, false_columns)), number_of_vars)

  for(falsey in false_columns) expect_false(any(differences[,falsey]))
  for(truthy in true_columns) expect_true(all(differences[,truthy]))
})

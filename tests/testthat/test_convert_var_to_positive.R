context('convert_var_to_positive')

testdata_var_model <- function() {
  data(Canada, package='vars')
  data_set <- Canada
  vars::VAR(data_set, p=2, type='const')
}

testdata_negative_variables <- c('U', 'rw')

test_that('converts a var model to its positive representation', {
  test_that('for outgoing edges', {
    original_dataset <- (testdata_var_model())

    result <- convert_var_to_positive(testdata_var_model(), testdata_negative_variables, direction='outgoing')
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

  test_that('for incomming edges', {
    original_dataset <- (testdata_var_model())

    result <- convert_var_to_positive(testdata_var_model(), testdata_negative_variables, direction='incomming')
    # Something should have changed in the dataset
    differences <- (abs(Bcoef(result) - Bcoef(original_dataset)) > 0.001)
    expect_true(any(differences))

    changed_rows <- testdata_negative_variables
    not_changed_rows <- c("e", "prod")

    # Test if we tested all variables
    number_of_vars <- testdata_var_model()$K
    expect_equal(length(c(changed_rows, not_changed_rows)), number_of_vars)

    for(truthy in changed_rows) expect_true(all(differences[truthy,]))
    for(falsey in not_changed_rows) expect_false(all(differences[falsey,]))
  })

  test_that('for both incomming and outgoing edges', {
    original_dataset <- (testdata_var_model())

    result <- convert_var_to_positive(testdata_var_model(), testdata_negative_variables, direction='both')

    # Something should have changed in the dataset
    differences <- (abs(Bcoef(result) - Bcoef(original_dataset)) > 0.001)
    expect_true(any(differences))

    changed_columns <- c("U.l1", "U.l2", "rw.l1", "rw.l2")
    similar_columns <- c("e.l1", "e.l2", "prod.l1", "prod.l2", "const")
    changed_rows <- c('U', 'rw')
    not_changed_rows <- c("e", "prod")

    count <- 0
    # All columns in the not negative rows, and positive columns should remain the same
    for (r in not_changed_rows) {
      for (c in similar_columns) {
        count <- count +1
        expect_false(differences[r,c])
      }
    }

    # All columns in the negative rows, and positive and negative columns should change
    for (r in changed_rows) {
      for (c in c(changed_columns, similar_columns)) {
        count <- count +1
        browser()
        expect_true(differences[r,c])
      }
    }

    # negative columns in the positive rows should change
    for (r in not_changed_rows) {
      for (c in changed_columns) {
        count <- count +1
        expect_true(differences[r,c])
      }
    }

    # Test if we tested all variables
    expect_equal(count, prod(dim(Bcoef(original_dataset))))
  })
})

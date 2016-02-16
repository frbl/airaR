context('aira')

.set_exo <- function(model) {
  # This is terrible practice. Irf requires the exogen matrix to be available, which is not available anymore.
  # Here we recreate the matrix by padding the datamat with zeros.
  # TODO: Fix this!
  pattern <- paste(dimnames(model$y)[[2]], collapse='|')

  pattern <- paste('const', pattern, sep='|')
  endogen <- grepl(pattern, names(model$datamat), perl=TRUE)
  exogedata <<- model$datamat[names(model$datamat)[!endogen]]
  for ( i in 1:model$p) {
    exogedata <<- rbind(rep(0, length(!endogen)), exogedata)
  }
}

testdata_var_model <- function() {
  data_set <- autovar::read_spss("inst/pp1_nieuw_compleet.sav", to.data.frame=TRUE)
  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exogedata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
  #assign("endodata", "endodata", envir = .GlobalEnv)

  vars::VAR(endodata, p=2, type='const', exogen=exogedata)
  #assign("y", "y", envir = .GlobalEnv)
  #assign("exogedata", "exogedata", envir = .GlobalEnv)
  #v$datamat
  #vars::irf(v, boot=10)

}

test_that('determine_best_node_from_all', {
  test_that('it returns the total effect of a variable on the other variables', {
    .set_exo(testdata_var_model())
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_best_node_from_all()

    expect_equal(tot$SomBewegUur, -1.859704, tolerance=1e-5)
    expect_equal(tot$SomPHQ, 0.1706285, tolerance=1e-5)
  })
  test_that('it can use bootstrapping to return the total significant effect of a variable on the other variables', {
    aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_best_node_from_all()

    # According to the Rosmalen paper, in this model we would expect a significant NEGATIVE effect
    # from sombeweguur on the som phq variable. The effect is rather large, i.e. < -.15 (i.e. order 1)

    # Because of the low number of iterations, the results might vary
    expect_lt(tot$SomBewegUur, -0.14)
    expect_gt(tot$SomBewegUur, -0.22)

    # Furthermore, they show that for order one, no significant effect exist from the somphq variables
    expect_equal(tot$SomPHQ, 0, tolerance=1e-5)
  })
})

test_that('determine_effect_network', {
  test_that('Without autocorrelation', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    result <- aira$determine_effect_network()
    expect_equal(dim(result), c(2,2))

    # No autocorrelation
    expect_equal(result['SomBewegUur', 'SomBewegUur'], 0)
    expect_equal(result['SomPHQ', 'SomPHQ'], 0)

    expect_equal(result['SomBewegUur', 'SomPHQ'], -1.859704, tolerance=1e-5)
    expect_equal(result['SomPHQ', 'SomBewegUur'], 0.1706285, tolerance=1e-5)
  })
})

test_that('determine_percentage_effect', {
  test_that('with orthogonalization', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_percentage_effect("SomBewegUur", 10)

    # The effect on the variable itself should not be included
    expect_equal(tot$SomBewegUur, NULL)
    expect_equal(tot$SomPHQ,0.3580667, tolerance=1e-4)
  })

  test_that('without orthogonalization', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)
    result <- aira$determine_percentage_effect("SomBewegUur", 10)

    # The effect on the variable itself should not be included
    expect_equal(result$SomBewegUur, NULL)
    expect_equal(result$SomPHQ,0.585768, tolerance=1e-5)

    result <- aira$determine_percentage_effect("SomPHQ", 10)

    # The effect on the variable itself should not be included
    expect_equal(result$SomPHQ, NULL)
    expect_equal(result$SomBewegUur, -0.2187225, tolerance=1e-5)
  })
})

## HERE WE REPLECATE THE ROSMALEN EXPERIMENT
test_that('determine_length_of_effect', {
  variable_to_shock = 'SomBewegUur'
  variable_to_respond = 'SomPHQ'
  aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE, reverse_order=FALSE)
  .set_exo(testdata_var_model())
  result <- aira$determine_length_of_effect(variable_name = variable_to_shock,
                                            response = variable_to_respond,
                                            measurement_interval = 24*60)

  # We expect the effect to be between two and three days
  expected_bottom = 2 * 24 * 60
  expected_top = 3 * 24 * 60
  #print(result/(24*60))
  expect_more_than(result, expected_bottom)
  expect_less_than(result, expected_top)
})

test_that('.calculate_irf', {
  skip("Not yet tested")
})

test_that('get_all_variable_names', {
  aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)
  result <- aira$get_all_variable_names()
  expected <- dimnames(testdata_var_model()$y)[[2]]
  expect_equal(result, expected)
})

test_that('.get_variable_name', {
  aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)

  for (i in 1:aira$var_model$K) {
    result <- aira$.get_variable_name(i)
    expected <- dimnames(testdata_var_model()$y)[[2]][i]
    expect_equal(result, expected)
  }
})

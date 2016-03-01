context('aira')
fast = FALSE
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

  var.2c <- vars::VAR(endodata, p=2, type='const', exogen=exogedata)
  resmat <- autovar::new_restriction_matrix(var.2c)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l2', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l1', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l2', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'UitbijterPHQ', 0, resmat)
  resmat <- autovar::format_restriction_matrix(var.2c, resmat)
  var.2c <- vars::restrict(var.2c, method = 'manual', resmat = resmat)
  var.2c$exogen <- exogedata
  var.2c
}

testdata_multiple_variables <- function() {
  data(Canada, package='vars')
  data_set <- Canada
  var.2c <- vars::VAR(data_set, p=2, type='both')
  var.2c
}

test_that('determine_best_node_from_all', {
  test_that('it returns the total effect of a variable on the other variables', {
    test_that('Rosmalen', {
      .set_exo(testdata_var_model())
      aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
      tot <- aira$determine_best_node_from_all()

      expect_equal(tot$SomBewegUur, -2.32155, tolerance=1e-5)
      expect_equal(tot$SomPHQ, 0, tolerance=1e-5)
    })
    test_that('Canada', {
      aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_multiple_variables(), orthogonalize= TRUE)
      tot <- aira$determine_best_node_from_all()

      # Values have been copy pasted. just to check if the DF is complete
      expect_true('e' %in% names(tot))
      expect_true('prod' %in% names(tot))
      expect_true('U' %in% names(tot))
      expect_true('rw' %in% names(tot))

      expect_true(class(tot$e) == 'numeric')
      expect_true(class(tot$prod) == 'numeric')
      expect_true(class(tot$U) == 'numeric')
      expect_true(class(tot$rw) == 'numeric')

      expect_equal(tot$e, -1.674678, tolerance=1e-5)
      expect_equal(tot$rw, -2.166623, tolerance=1e-5)
      expect_equal(tot$prod, -0.9059553, tolerance=1e-5)
      expect_equal(tot$U, 3.49601, tolerance=1e-5)
    })

  })


  test_that('it returns the total effect of a variable on the other variables, also without orthogonalization', {
    .set_exo(testdata_var_model())
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)
    tot <- aira$determine_best_node_from_all()

    # Result from aira-web
    expect_equal(tot$SomBewegUur, -1.047593888, tolerance=1e-5)
    expect_equal(tot$SomPHQ, 0, tolerance=1e-5)
  })

  test_that('it can use bootstrapping to return the total significant effect of a variable on the other variables', {
    test_that('Rosmalen', {
      if(fast) skip('Takes too long')
      aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
      tot <- aira$determine_best_node_from_all()

      # According to the Rosmalen paper, in this model we would expect a significant NEGATIVE effect
      # from sombeweguur on the som phq variable. The effect is rather large, i.e. < -.15 (i.e. order 1)

      # Because of the low number of iterations, the results might vary
      # with 1000 bootstraps the result is more or less stable, and around .4, but this takes a lot of time
      expect_lt(tot$SomBewegUur, -0.03)
      expect_gt(tot$SomBewegUur, -0.10)

      # Furthermore, they show that for order one, no significant effect exist from the somphq variables
      expect_equal(tot$SomPHQ, 0, tolerance=1e-5)
    })

    test_that('Canada', {
      aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_multiple_variables(), orthogonalize= TRUE)
      tot <- aira$determine_best_node_from_all()
      # Values have been copy pasted. just to check if the DF is complete
      expect_true('e' %in% names(tot))
      expect_true('prod' %in% names(tot))
      expect_true('U' %in% names(tot))
      expect_true('rw' %in% names(tot))

      expect_true(class(tot$e) == 'numeric')
      expect_true(class(tot$prod) == 'numeric')
      expect_true(class(tot$U) == 'numeric')
      expect_true(class(tot$rw) == 'numeric')
    })
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

    expect_equal(result['SomBewegUur', 'SomPHQ'], -2.32155, tolerance=1e-5)
    expect_equal(result['SomPHQ', 'SomBewegUur'], 0, tolerance=1e-5)
  })
})

test_that('determine_percentage_effect', {
  test_that('with orthogonalization', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_percentage_effect("SomBewegUur", 10)

    # The effect on the variable itself should not be included
    expect_equal(tot$SomBewegUur, NULL)

    # The effect of SOMPHQ is 0 (or at least extremely small), therefore it would take an infinite amount of
    # somphq to make sombeweguur larger
    expect_equal(tot$SomPHQ, Inf, tolerance=1e-4)

    tot <- aira$determine_percentage_effect("SomPHQ", 10)

    # The effect on the variable itself should not be included
    expect_equal(tot$SomPHQ, NULL)

    # The effect of SomBewegUur on SomPhq is not null, so this can have an effect
    expect_equal(tot$SomBewegUur, -.0705, tolerance=1e-4)
  })

  test_that('without orthogonalization', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)
    result <- aira$determine_percentage_effect("SomBewegUur", 10)

    # The effect on the variable itself should not be included
    expect_equal(result$SomBewegUur, NULL)
    expect_equal(result$SomPHQ, Inf)

    result <- aira$determine_percentage_effect("SomPHQ", 10)

    # The effect on the variable itself should not be included
    expect_equal(result$SomPHQ, NULL)
    # Result checked with website
    expect_equal(result$SomBewegUur, -0.1562397, tolerance=1e-5)
  })

  test_that('with a higher percentage', {
    percentage = 20
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= FALSE)
    result <- aira$determine_percentage_effect("SomBewegUur", percentage)

    # The effect on the variable itself should not be included
    expect_equal(result$SomBewegUur, NULL)
    expect_equal(result$SomPHQ, Inf)

    result <- aira$determine_percentage_effect("SomPHQ", percentage)

    # The effect on the variable itself should not be included
    expect_equal(result$SomPHQ, NULL)
    # Result checked with website
    expected = -0.1562397*2
    expect_equal(result$SomBewegUur, expected, tolerance=1e-5)
  })
})

## HERE WE REPLECATE THE ROSMALEN EXPERIMENT
test_that('determine_length_of_effect', {
  if(fast) skip('Takes too long')
  variable_to_shock = 'SomBewegUur'
  variable_to_respond = 'SomPHQ'
  aira <- Aira$new(bootstrap_iterations = 2000, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE, reverse_order=FALSE)
  .set_exo(testdata_var_model())
  result <- aira$determine_length_of_effect(variable_name = variable_to_shock,
                                            response = variable_to_respond,
                                            measurement_interval = 24*60)

  # We expect the effect to be between 1.5 and three days
  expected_bottom = 1.5 * 24 * 60
  expected_top = 3 * 24 * 60
  print(result/(24*60))
  expect_more_than(result, expected_bottom)
  expect_less_than(result, expected_top)
})

test_that('.calculate_irf', {
  test_that('caching', {
    test_that('it returns a same cached object the second time', {
      if(fast) skip('Takes too long')
      aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE, reverse_order=FALSE)
      .set_exo(testdata_var_model())
      result1 <- aira$.calculate_irf(variable_name = 'SomBewegUur')
      result2 <- aira$.calculate_irf(variable_name = 'SomBewegUur')
      expect_equal(result1, result2)
    })

    test_that('it returns faster the second time because of caching', {
      if(fast) skip('Takes too long')
      aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE, reverse_order=FALSE)
      .set_exo(testdata_var_model())
      start.time <- Sys.time()
      result1 <- aira$.calculate_irf(variable_name = 'SomBewegUur')
      end.time <- Sys.time()
      duration_pre_caching <- end.time - start.time

      start.time <- Sys.time()
      result2 <- aira$.calculate_irf(variable_name = 'SomBewegUur')
      end.time <- Sys.time()

      duration_post_caching <- end.time - start.time
      expect_less_than(100 * duration_post_caching, duration_pre_caching)
    })
  })
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

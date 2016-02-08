context('aira')

testdata_var_model <- function() {
  data_set <- autovar::read_spss("inst/pp1_nieuw_compleet.sav", to.data.frame=TRUE)
  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exogedata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
  vars::VAR(endodata, p=2, type='const', exogen=exogedata)
}

test_that('determine_best_node_from_all', {
  test_that('it returns the total effect of a variable on the other variables', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_best_node_from_all()

    expect_equal(tot$SomBewegUur, -1.859704, tolerance=1e-5)
    expect_equal(tot$SomPHQ, 0.1706285, tolerance=1e-5)
  })
  test_that('it can use bootstrapping to return the total significant effect of a variable on the other variables', {
    aira <- Aira$new(bootstrap_iterations = 100, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_best_node_from_all()

    # According to the Rosmalen paper, in this model we would expect a significant NEGATIVE effect
    # from sombeweguur on the som phq variable. The effect is rather large, i.e. < -.15 (i.e. order 1)
    expect_lt(tot$SomBewegUur, -0.15)

    # Furthermore, they show that for order one, no significant effect exist from the somphq variables
    expect_equal(tot$SomPHQ, 0, tolerance=1e-5)
  })
})

test_that('determine_percentage_effect', {
  test_that('with orthogonalization', {
    aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
    tot <- aira$determine_percentage_effect("SomBewegUur", 10)

    # The effect on the variable itself should not be included
    expect_equal(tot$SomBewegUur, NULL)
    expect_equal(tot$SomPHQ,0.3580667, tolerance=1e-5)
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

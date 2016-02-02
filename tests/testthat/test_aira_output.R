context('aira_output')

testdata_aira_model <- function() {
  data_set <- autovar::read_spss("../../pp1 nieuw compleet.sav", to.data.frame=TRUE)
  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exodata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
  var_model <- vars::VAR(endodata, exogen=exodata, p=2, type='const')
  Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = var_model, orthogonalize= TRUE)
}

test_that('print_overview_percentage_effect', {
  aira_output <- AiraOutput$new(aira = testdata_aira_model())
  result <- aira_output$print_overview_percentage_effect(10)
  expected = "Effect achieved:\n"
  expected = paste(expected, "You could increase your SomBewegUur with 10% by increasing your SomPHQ with 36%\n", sep ="")
  expected = paste(expected, "You could increase your SomPHQ with 10% by decreasing your SomBewegUur with 9%\n", sep ="")
  expect_equal(result, expected)
})

test_that('print_percentage_effect', {
  aira_output <- AiraOutput$new(aira = testdata_aira_model())
  result <- aira_output$print_percentage_effect('SomBewegUur', 20)
  expected = "Effect achieved:\n"
  expected = paste(expected, "You could increase your SomBewegUur with 20% by increasing your SomPHQ with 72%\n", sep ="")
  expect_equal(result, expected)
})

context('aira_output')

testdata_aira_model <- function() {
  data_set <- autovar::read_spss("../../pp1 nieuw compleet.sav", to.data.frame=TRUE)
  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exodata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
  vars::VAR(endodata, exogen=exodata, p=2, type='const')
  Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = testdata_var_model(), orthogonalize= TRUE)
}

test_that('printPercentageEffect')

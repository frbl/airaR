library('aira')
source('inst/generate_test_functions.R')

# pp1 does not have LN
alternative_var_model_pp1 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "inst/pp1_nieuw_compleet.sav", sep=""), to.data.frame=TRUE)
  data_set[,'SomPHQ'] <- max(data_set[,'SomPHQ']) - data_set[,'SomPHQ']

  endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
  exogedata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]

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


alternative_testdata_var_model_100551 <- function(bust_cache=TRUE) {
  if(!exists("var_100551") || bust_cache) {
    if(!exists("file_100551") || bust_cache) {
      file_100551 <<- loadData('100551')
    }
    file_100551[,'onrust'] <- max(file_100551[,'onrust']) - file_100551[,'onrust']
    file <- file_100551
    var_100551 <<- calculateVar(file)
  }
  var.2c <- var_100551[[1]]$varest
  var.2c
}

alt_model <- alternative_var_model_pp1()
normal_model <- testdata_var_model_pp1()

#alt_model <- alternative_testdata_var_model_100551()
#normal_model <- testdata_var_model_100551(TRUE)

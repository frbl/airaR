base_dir <- 'tests/testthat/'
library('Amelia')
testdata_var_model_pp1 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "inst/pp1_nieuw_compleet.sav", sep=""), to.data.frame=TRUE)
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

testdata_var_model_pp2 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "inst/pp2_nieuw_compleet_64dagen.sav", sep=""), to.data.frame=TRUE)
  endodata <- data_set[,c('lnSomBewegUur', 'lnSomPHQ')]
  exogedata <- data_set[,c('UitbijterBeweg'), drop=FALSE]
  #assign("endodata", "endodata", envir = .GlobalEnv)

  var.2c <- vars::VAR(endodata, p=1, type='const', exogen=exogedata)
  resmat <- autovar::new_restriction_matrix(var.2c)
  # USing constraints set on line 73 of winnende model pp2.txt
  resmat <- autovar::update_restriction_matrix(var.2c, 'lnSomBewegUur', 'lnSomPHQ.l1', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'lnSomPHQ', 'UitbijterBeweg', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'lnSomPHQ', 'lnSomBewegUur.l1', 0, resmat)
  resmat <- autovar::format_restriction_matrix(var.2c, resmat)
  var.2c <- vars::restrict(var.2c, method = 'manual', resmat = resmat)
  var.2c$exogen <- exogedata
  var.2c
}

testdata_var_model_pp4 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "inst/pp4_nieuw_compleet_met_140min.sav", sep=""), to.data.frame=TRUE)
  endodata <- data_set[,c('SomPHQ', 'SomBewegUur')]
  exogedata <- data_set[,c('Work', 'UitbijterPHQ', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')]
  #assign("endodata", "endodata", envir = .GlobalEnv)

  var.2c <- vars::VAR(endodata, p=2, type='const', exogen=exogedata)
  resmat <- autovar::new_restriction_matrix(var.2c)
  # Using constraints set on line 29 of winnende model pp4.txt
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Sunday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Friday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Thursday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Monday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Tuesday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Wednesday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBeweg', 'Wednesday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'SomBewegUur.l1', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l1', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'SomBewegUur.l2', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l2', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'UitbijterPHQ', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'Work', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'Thursday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'Wednesday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomBewegUur', 'Monday', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'SomPHQ', 'SomPHQ.l1', 0, resmat)

  resmat <- autovar::format_restriction_matrix(var.2c, resmat)
  var.2c <- vars::restrict(var.2c, method = 'manual', resmat = resmat)
  var.2c$exogen <- exogedata
  var.2c
}

testdata_var_model_pp5 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "inst/pp5_nieuw_compleet.sav", sep=""), to.data.frame=TRUE)
  endodata <- data_set[,c('lnSomBewegUur', 'lnSomPHQ')]
  exogedata <- data_set[,c('Uitbijter_27'), drop=FALSE]

  var.2c <- vars::VAR(endodata, p=1, type='const', exogen=exogedata)
  resmat <- autovar::new_restriction_matrix(var.2c)
  # Using constraints set on line 29 of winnende model pp5.txt
  resmat <- autovar::update_restriction_matrix(var.2c, 'lnSomBewegUur', 'lnSomBewegUur.l1', 0, resmat)
  resmat <- autovar::update_restriction_matrix(var.2c, 'lnSomPHQ', 'lnSomBewegUur.l1', 0, resmat)

  resmat <- autovar::format_restriction_matrix(var.2c, resmat)
  var.2c <- vars::restrict(var.2c, method = 'manual', resmat = resmat)
  var.2c$exogen <- exogedata
  var.2c
}


###############################################
#               HGI TEST FUNCTIONS            #
###############################################
loadData <- function(file) {
  data <- read.csv(paste(base_dir, "inst/csv/",file,".csv", sep=""), stringsAsFactors = FALSE)
  included_columns <- c('onrust', 'ontspanning', 'beweging')

  # Remove unused columns
  data <- data[,(names(data) %in% included_columns)]
  autovar::impute_dataframe(data, measurements_per_day = 3, repetitions = 150)
}

calculateVar <- function(data) {
  models <- autovarCore::autovar(
    data,
    selected_column_names = names(data),
    measurements_per_day = 3,
    criterion='BIC',
    test_names = c("portmanteau", "portmanteau_squared","skewness"),
    imputation_iterations = 1
  )
  if (models[[1]]$bucket < 0.01 ) print('Model not very valid')
  models
}


testdata_var_model_100551 <- function(bust_cache=FALSE) {
  if(!exists("var_100551") || bust_cache) {
    if(!exists("file_100551") || bust_cache) {
      file_100551 <<- loadData('100551')
    }
    file <- file_100551
    var_100551 <<- calculateVar(file)
  }
  var.2c <- var_100551[[1]]$varest
  var.2c
}

testdata_var_model_100849 <- function(bust_cache=FALSE) {
  if(!exists("var_100849") || bust_cache) {
    if(!exists("file_100849") || bust_cache) {
      file_100849 <<- loadData('100849')
    }
    file <- file_100849
    var_100849 <<- calculateVar(file)
  }
  var.2c <- var_100849[[1]]$varest
  var.2c
}

testdata_var_model_112098 <- function(bust_cache=FALSE) {
  if(!exists("var_112098") || bust_cache) {
    if(!exists("file_112098") || bust_cache) {
      file_112098 <<- loadData('112098')
    }
    file <- file_112098
    var_112098 <<- calculateVar(file)
  }
  var.2c <- var_112098[[1]]$varest
  var.2c
}

testdata_var_model_110478 <- function(bust_cache=FALSE) {
  if(!exists("var_110478") || bust_cache) {
    if(!exists("file_110478") || bust_cache) {
      file_110478 <<- loadData('110478')
    }
    file <- file_110478
    var_110478 <<- calculateVar(file)
  }
  var.2c <- var_110478[[1]]$varest
  var.2c
}

testdata_var_model_100713 <- function(bust_cache=FALSE) {
  if(!exists("var_100713") || bust_cache) {
    if(!exists("file_100713") || bust_cache) {
      file_100713 <<- loadData('100713')
    }
    file <- file_100713
    var_100713 <<- calculateVar(file)
  }
  var.2c <- var_100713[[1]]$varest
  var.2c
}

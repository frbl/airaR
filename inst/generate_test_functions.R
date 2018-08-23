base_dir <- '~/vault/Datasets/Rosmalen_aira/'
library('Amelia')
testdata_var_model_pp1 <- function() {
  data_set <- autovar::read_spss(paste(base_dir, "pp1_nieuw_compleet.sav", sep=""), to.data.frame=TRUE)
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
  data_set <- autovar::read_spss(paste(base_dir, "pp2_nieuw_compleet_64dagen.sav", sep=""), to.data.frame=TRUE)
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
  data_set <- autovar::read_spss(paste(base_dir, "pp4_nieuw_compleet_met_140min.sav", sep=""), to.data.frame=TRUE)
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
  data_set <- autovar::read_spss(paste(base_dir, "pp5_nieuw_compleet.sav", sep=""), to.data.frame=TRUE)
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

translate_model <- function(model) {
  set_exo(model)
  model_names <- names(model$varresult)
  model_names <- replace(model_names, model_names=='SomBewegUur', 'Activity')
  model_names <- replace(model_names, model_names=='SomPHQ', 'Depression')
  model_names <- replace(model_names, model_names=='lnSomBewegUur', 'Activity')
  model_names <- replace(model_names, model_names=='lnSomPHQ', 'Depression')
  names(model$varresult) <- model_names

  names(model$varresult$Activity$coefficients) <- sub(".*SomBewegUur", 'Activity', names(model$varresult$Activity$coefficients))
  names(model$varresult$Activity$coefficients) <- sub(".*SomPHQ", 'Depression', names(model$varresult$Activity$coefficients))

  names(model$varresult$Depression$coefficients) <- sub(".*SomBewegUur", 'Activity', names(model$varresult$Depression$coefficients))
  names(model$varresult$Depression$coefficients) <- sub(".*SomPHQ", 'Depression', names(model$varresult$Depression$coefficients))

  colnames(model$y) <- sub(".*SomPHQ", 'Depression', colnames(model$y))
  colnames(model$y) <- sub(".*SomBewegUur", 'Activity', colnames(model$y))
  model
}

###############################################
#               HGI TEST FUNCTIONS            #
###############################################
loadData <- function(file) {
  filename <- paste(base_dir, "csv/",file,".csv", sep="")
  message(filename)
  data <- read.csv(filename, stringsAsFactors = FALSE)
  included_columns <- c('somberheid', 'tekortschieten', 'ontspanning')
  included_columns <- c('piekeren', 'onrust', 'opgewektheid', 'concentratie', 'eigenwaarde')

  # Remove unused columns
  data <- data[,included_columns]
  autovar::impute_dataframe(data, measurements_per_day = 3, repetitions = 150)
}

calculateVar <- function(data) {
  models <- autovarCore::autovar(
    data,
    selected_column_names = names(data),
    measurements_per_day = 3,
    criterion='BIC',
    test_names = c("portmanteau", "portmanteau_squared","skewness"),
    imputation_iterations = 150
  )
  if (models[[1]]$bucket < 0.01 ) message('Model not very valid')
  models
}

testdata_var_model <- function(id, bust_cache=FALSE) {
  model_name <- paste('var',id,sep='_')
  file_name <- paste('file',id,sep='_')

  if(!exists(model_name) || bust_cache) {
    if(!exists(file_name) || bust_cache) {
      cur_data <- loadData(id)
      assign(file_name, cur_data, envir=globalenv())
    }
    file <- get(file_name)
    var_model <- calculateVar(file)
    assign(model_name, var_model, envir=globalenv())
  }
  get(model_name)[[1]]$varest
}

# Person 1 Female, 1983, Dutch, 28.71 yearsold
testdata_var_model_100849 <- function(bust_cache=FALSE) {
  testdata_var_model(100849, bust_cache = bust_cache)
}

# Person 2: Female, 1952, Dutch, 61.73 yearsold
testdata_var_model_100551 <- function(bust_cache=FALSE) {
  testdata_var_model(100551, bust_cache = bust_cache)
}

# Person: 3: Female, 1989 (12), Dutch,  24.80 yearsold
testdata_var_model_112098 <- function(bust_cache=FALSE) {
  testdata_var_model(112098, bust_cache = bust_cache)
}

# Person 4: Male, 1958, Dutch, 56.39 yearsold
testdata_var_model_110478 <- function(bust_cache=FALSE) {
  testdata_var_model(110478, bust_cache = bust_cache)
}

# person5 Female, 1989 01 , Dutch, 25.82 yearsold
testdata_var_model_100713 <- function(bust_cache=FALSE) {
  testdata_var_model(100713, bust_cache = bust_cache)
}

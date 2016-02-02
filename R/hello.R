# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  source("R/aira.R")

  variable_name <- 'U'

  dataset = read.spss("pp1 nieuw compleet.sav", to.data.frame=TRUE)
  endodata = dataset[,c('SomBewegUur', 'SomPHQ')]
  exodata = dataset[,c('UitbijterPHQ','UitbijterBeweg')]
  var.2c = VAR(endodata, exogen=exodata, p=2, type='const')
  resmat <- new_restriction_matrix(var.2c)
  resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l2', 0, resmat)
  resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l1', 0, resmat)
  resmat <- update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l2', 0, resmat)
  resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'UitbijterPHQ', 0, resmat)
  resmat <- format_restriction_matrix(var.2c, resmat)

  var.2c <- restrict(var.2c, method = 'manual', resmat = resmat)
  var.2c$exogen <- exodata
  print(Bcoef(var.2c))


  aira <- Aira$new(bootstrap_iterations = 10, steps= 10, var_model = var.2c)
  tot <- aira$determine_best_node_from_all()
  tot <- aira$determine_percentage_effect("SomBewegUur", 10)
  print(tot)
}

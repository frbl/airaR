library('aira')
source('inst/generate_test_functions.R')

horizon <<- 10
width <- (360 * 1.45) /72
height <- 360 /72



generate_plots <- function(model, name) {
  loc = paste('inst/output/irf/', name, '_a_d.pdf', sep='')
  pdf(loc, width=width, height=height)
  plot(vars::irf(model, impulse='Activity', response='Depression', runs=200))
  dev.off()
  system(paste('pdfcrop',loc))
  new_loc = paste('inst/output/irf/', name, '_a_d-crop.pdf', sep='')
  system(paste('mv',new_loc, loc))

  loc = paste('inst/output/irf/', name, '_d_a.pdf', sep='')
  pdf(loc, width=width, height=height)
  plot(vars::irf(model, impulse='Depression', response='Activity', runs=200))
  dev.off()
  system(paste('pdfcrop',loc))
  new_loc = paste('inst/output/irf/', name, '_d_a-crop.pdf', sep='')
  system(paste('mv',new_loc, loc))
}

var_model_pp1 <- function() {
  var.2c <- testdata_var_model_pp1()
  var.2c <- translate_model(var.2c)
  generate_plots(var.2c, 'pp1')
}

var_model_pp2 <- function() {
  var.2c <- testdata_var_model_pp2()
  var.2c <- translate_model(var.2c)
  generate_plots(var.2c, 'pp2')
}
var_model_pp4 <- function() {
  var.2c <- testdata_var_model_pp4()
  var.2c <- translate_model(var.2c)
  generate_plots(var.2c, 'pp4')
}
var_model_pp5 <- function() {
  var.2c <- testdata_var_model_pp5()
  var.2c <- translate_model(var.2c)
  generate_plots(var.2c, 'pp5')
}

set.seed(12345)
var_model_pp1()
var_model_pp2()
var_model_pp4()
var_model_pp5()

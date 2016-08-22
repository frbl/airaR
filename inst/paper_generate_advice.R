source('inst/generate_test_functions.R')
run_aira <- function(variable_to_improve, percentage, model, name) {
  aira <- Aira$new(bootstrap_iterations = bootstrap_iterations, horizon= 10, var_model = model,
                   orthogonalize= TRUE, reverse_order=FALSE)
  set_exo(model)
  aira_output <- AiraOutput$new(aira = aira)
  result <- aira$determine_percentage_effect(variable_to_improve, percentage)
  print(result)
  result <- lapply(result, `[[`, 1)
  print(result)
  output <- aira_output$print_percentage_effect(variable_to_improve = variable_to_improve,
                                                percentage_to_improve = percentage, print_newlines = FALSE,
                                                print_title = FALSE)
  print(output)
  result
}

var_model_pp1 <- function() {
  var.2c <- testdata_var_model_pp1()
  variable1 = 'SomBewegUur'
  variable2 = 'SomPHQ'
  name = 'PP1'
  a <- run_aira(variable1, percentage = 10, var.2c, name)
  b <- run_aira(variable2, percentage = -10, var.2c, name)
  c(a,b)
}


var_model_pp2 <- function() {
  var.2c <- testdata_var_model_pp2()
  variable1 = 'lnSomBewegUur'
  variable2 = 'lnSomPHQ'
  name = 'PP2'
  a <- run_aira(variable1, percentage = 10, var.2c, name)
  b <- run_aira(variable2, percentage = -10, var.2c, name)
  c(a,b)
}

var_model_pp4 <- function() {
  var.2c <- testdata_var_model_pp4()

  variable1 = 'SomBewegUur'
  variable2 = 'SomPHQ'
  name = 'PP4'
  a <- run_aira(variable1, percentage = 10, var.2c, name)
  b <- run_aira(variable2, percentage = -10, var.2c, name)
  c(a,b)
}

var_model_pp5 <- function() {
  var.2c <- testdata_var_model_pp5()

  variable1 = 'lnSomBewegUur'
  variable2 = 'lnSomPHQ'
  name = 'PP5'
  a <- run_aira(variable1, percentage = 10, var.2c, name)
  b <- run_aira(variable2, percentage = -10, var.2c, name)
  c(a,b)
}

# Check if any of the models give errors
pp1<-var_model_pp1()
pp2<-var_model_pp2()
pp4<-var_model_pp4()
pp5<-var_model_pp5()

pp1<-unlist(pp1)
pp2<-unlist(pp2)
pp4<-unlist(pp4)
pp5<-unlist(pp5)


row_names <- c('pp1','pp2','pp4','pp5')
col_names <- c('A>D','D>A')

our_results <- matrix(c(pp1,pp2,pp4,pp5),ncol=length(pp1))
our_results <- our_results
our_results <- round(our_results, 2)
our_results <- matrix(gsub(Inf, "No effect", our_results),ncol=length(pp1))
our_results <- matrix(gsub("([0-9])$", "\\1\\\\%", our_results, perl=TRUE),ncol=length(pp1))
rownames(our_results) <- row_names
colnames(our_results) <- col_names
our_results

x <- data.frame(
                mine_som_beweg_uur = our_results
                )

names(x) <- c("Activity to improve Depression", "Depression to improve Activity (\\textsc{aira})")
table <- xtable(x, label="tab:effects",
                caption='\\citeauthor{RefWorks:4}~\\cite{RefWorks:4}.', digits = 1)
print(table,
      file='inst/output/tab_effects.tex',
      sanitize.text.function=function(str)gsub(" "," ",str,fixed=TRUE),
      floating=TRUE,
      booktabs=TRUE)


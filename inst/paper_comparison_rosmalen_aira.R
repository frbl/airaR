library('aira')
library('xtable')
source('inst/generate_test_functions.R')

horizon <<- 10
run_aira <- function(variable_to_shock, variable_to_respond, model, name) {
  aira <- Aira$new(bootstrap_iterations = bootstrap_iterations, horizon= horizon, var_model = model,
                   orthogonalize= TRUE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  filename <- paste("inst/output/", name, "_", variable_to_shock,"_",variable_to_respond, ".pdf", sep="")
  pdf(file=filename, width=7, height=5)
  result <- aira$determine_length_of_effect(variable_name = variable_to_shock,
                                            response = variable_to_respond,
                                            measurement_interval = 24*60,
                                            first_effect_only = FALSE, plot_results=TRUE)
  title(name)
  dev.off()
  # Crop the PDF
  system(paste('pdfcrop', filename, filename, sep=" "))

  # We expect the effect to be between 1.5 and three days
  print(paste(name, ':', result, 'minutes (', variable_to_shock, 'on', variable_to_respond,')'))
  result
}

var_model_pp1 <- function() {
  var.2c <- testdata_var_model_pp1()
  var.2c

  variable1 = 'SomBewegUur'
  variable2 = 'SomPHQ'
  name = 'PP1'
  a <- run_aira(variable1, variable2, var.2c, name)
  b <- run_aira(variable2, variable1, var.2c, name)
  c(a,b)
}



var_model_pp2 <- function() {
  var.2c <- testdata_var_model_pp2()
  variable1 = 'lnSomBewegUur'
  variable2 = 'lnSomPHQ'
  name = 'PP2'
  a <- run_aira(variable1, variable2, var.2c, name)

  b <- run_aira(variable2, variable1, var.2c, name)
  c(a,b)
}

var_model_pp4 <- function() {
  var.2c <- testdata_var_model_pp4()

  variable1 = 'SomBewegUur'
  variable2 = 'SomPHQ'
  name = 'PP4'
  a <- run_aira(variable1, variable2, var.2c, name)
  b <- run_aira(variable2, variable1, var.2c, name)
  c(a,b)
}

var_model_pp5 <- function() {
  var.2c <- testdata_var_model_pp5()


  variable1 = 'lnSomBewegUur'
  variable2 = 'lnSomPHQ'
  name = 'PP5'
  a <- run_aira(variable1, variable2, var.2c, name)
  b <- run_aira(variable2, variable1, var.2c, name)
  c(a,b)
}

# Check if any of the models give errors
pp1<-var_model_pp1()
pp2<-var_model_pp2()
pp4<-var_model_pp4()
pp5<-var_model_pp5()

row_names <- c('pp1','pp2','pp4','pp5')
col_names <- c('A>D','D>A')

our_results <- matrix(c(pp1,pp2,pp4,pp5),ncol=length(pp1), byrow=TRUE)
our_results_days <- our_results / (24 * 60)
our_results <- matrix(paste(round(our_results, 1), ' minutes (', round(our_results_days, 1), ' days)', sep=""), ncol=length(pp1))

rownames(our_results) <- row_names
colnames(our_results) <- col_names
# A > D, D > A
their_results_order_1 <- matrix(c(5,0,
                                  3,0,
                                  2,6, # The first 2 is 2 * 1 (1 at lag 0, 1 at lag 2)
                                  0,1), ncol=length(pp1), byrow = TRUE)


#their_results_order_2 <- matrix(c(3,1, # the 3 is 1 + 2 (1 at lag 1, 2 at lag 3,4)
#                          00,2,
#                          00,7,
#                          00,1), ncol=length(pp1), byrow = TRUE)

rownames(their_results_order_1) <- row_names
rownames(their_results_order_2) <- row_names
colnames(their_results_order_1) <- col_names
colnames(their_results_order_2) <- col_names

# Print results in Latex table
x <- data.frame(
  mine_som_beweg_uur = our_results,
  their_som_beweg_uur_order1 = their_results_order_1
  #their_som_beweg_uur_order2 = their_results_order_2
)

x


column_names <- c("A $\\rightarrow$ D (aira)", "D $\\rightarrow$ A (aira)", "A $\\rightarrow$ D (Rosmalen 1)", "D $\\rightarrow$ A (Rosmalen 1)", "A $\\rightarrow$ D (Rosmalen 2)", "D $\\rightarrow$ A (Rosmalen 2)")
names(x) <- column_names[1:4]

print(x)

table <- xtable(x, label="tab:comparison",
                caption='Comparison between the outcomes of AIRA and results from the study by \\citeauthor{RefWorks:4}~\\cite{RefWorks:4}.', digits = 1)
print(table,
      file='inst/output/tab_comparison.tex',
      sanitize.text.function=function(str)gsub(" "," ",str,fixed=TRUE),
      floating=TRUE,
      booktabs=TRUE, floating.environment = 'table*')


library('aira')
library('xtable')
source('inst/generate_test_functions.R')
set.seed(12345)
test_model <- function(model, negative_variables) {
  model <- convert_var_to_positive(var_model = model, negative_variables = negative_variables)
  aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = model,
                   orthogonalize= FALSE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  aira$determine_best_node_from_all()
}

negative_variables <- c('onrust')
bust <- FALSE
# Check if any of the models give errors
model <- testdata_var_model_100849(bust)
lag <- model$p
p100849 <- test_model(model, negative_variables)

model <- testdata_var_model_100551(bust)
lag <- model$p
p100551 <- test_model(model, negative_variables)

model <- testdata_var_model_112098(bust)
lag <- model$p
p112098 <- test_model(model, negative_variables)

model <- testdata_var_model_110478(bust)
lag <- model$p
p110478 <- test_model(model, negative_variables)

model <- testdata_var_model_100713(bust)
lag <- model$p
p100713 <- test_model(model, negative_variables)

print(p100849)
print(p100551)
print(p112098)
print(p110478)
print(p100713)

x <- rbind(
  t(p100849),
  t(p100551),
  t(p112098),
  t(p110478),
  t(p100713)
)

colnames(x) <- c("Feeling less nervous", "Relaxation", "Activity")
rownames(x)<- paste('Person',1:dim(x)[1])
data.frame(x)

table <- xtable(x, label="tab:effects_in_aira",
                caption='Effects of Feeling less nervous, relaxation and activity on well-being, in terms of standard deviations.', digits = 3)
print(table,
      file='inst/output/tab_effects_in_aira.tex',
      sanitize.text.function=function(str)gsub(" "," ",str,fixed=TRUE),
      floating=TRUE,
      booktabs=TRUE, floating.environment = 'table')

print(x)

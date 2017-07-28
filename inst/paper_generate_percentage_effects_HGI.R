library('aira')
library('xtable')
source('inst/generate_test_functions.R')
set.seed(12345)
test_model <- function(model, negative_variables) {
  aira <- Aira$new(bootstrap_iterations = bootstrap_iterations, horizon= 10, var_model = model,
                   orthogonalize= FALSE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  improve_somberheid <- aira$determine_percentage_effect('somberheid', -10)
  improve_relaxation <- aira$determine_percentage_effect('ontspanning', 10)
  improve_tekortschieten <- aira$determine_percentage_effect('tekortschieten', -10)
  data.frame(
             improve_somberheid = improve_somberheid,
             improve_relaxation = improve_relaxation,
             improve_tekortschieten = improve_tekortschieten
             )
}

negative_variables <- c('somberheid', 'tekortschieten')
# Check if any of the models give errors
model <- testdata_var_model_100849(bust)
#plot(vars::irf(model, boot=FALSE, ortho=FALSE))
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

decrease_somberheid <- data.frame(
                              'decrease.feeling.gloomy.by.changing.relaxation' = c(p100849$improve_somberheid.ontspanning.percentage, p100551$improve_somberheid.ontspanning.percentage, p112098$improve_somberheid.ontspanning.percentage, p110478$improve_somberheid.ontspanning.percentage, p100713$improve_somberheid.ontspanning.percentage),
                              'decrease.feeling.gloomy.by.changing.feeling.inadequate' = c(p100849$improve_somberheid.tekortschieten.percentage, p100551$improve_somberheid.tekortschieten.percentage, p112098$improve_somberheid.tekortschieten.percentage, p110478$improve_somberheid.tekortschieten.percentage, p100713$improve_somberheid.tekortschieten.percentage)
                              )

increase_relaxation <- data.frame(
                                'increase.relaxation.by.changing.feeling.gloomy' = c(p100849$improve_relaxation.somberheid.percentage, p100551$improve_relaxation.somberheid.percentage, p112098$improve_relaxation.somberheid.percentage, p110478$improve_relaxation.somberheid.percentage, p100713$improve_relaxation.somberheid.percentage),
                                'increase.relaxation.by.changing.feeling.inadequate' = c(p100849$improve_relaxation.tekortschieten.percentage, p100551$improve_relaxation.tekortschieten.percentage, p112098$improve_relaxation.tekortschieten.percentage, p110478$improve_relaxation.tekortschieten.percentage, p100713$improve_relaxation.tekortschieten.percentage)
                                )

increase_inadequate <- data.frame(
                                  'decrease.feeling.inadequate.by.changing.feeling.gloomy' = c(p100849$improve_tekortschieten.somberheid.percentage, p100551$improve_tekortschieten.somberheid.percentage, p112098$improve_tekortschieten.somberheid.percentage, p110478$improve_tekortschieten.somberheid.percentage, p100713$improve_tekortschieten.somberheid.percentage),
                                  'decrease.feeling.inadequate.by.changing.relaxation' = c(p100849$improve_tekortschieten.ontspanning.percentage, p100551$improve_tekortschieten.ontspanning.percentage, p112098$improve_tekortschieten.ontspanning.percentage, p110478$improve_tekortschieten.ontspanning.percentage, p100713$improve_tekortschieten.ontspanning.percentage)
                                  )

x <- rbind(
           t(decrease_somberheid),
           t(increase_relaxation),
           t(increase_inadequate)
           )
x <- t(x)

rownames(x)<- paste('Person',1:dim(x)[1])
column_names <- dimnames(x)[[2]]
column_names <- gsub('.OB.', ' (', column_names)
column_names <- gsub('.CB.', ')', column_names)
column_names <- gsub('uparrow', '$\\\\uparrow$', column_names)
column_names <- gsub('downarrow', '$\\\\downarrow$', column_names)
column_names <- gsub('\\.', ' ', column_names)
column_names
colnames(x) <- column_names
#

for(person in dimnames(x)[[1]]) {
  for(change_variable in dimnames(x)[[2]]) {
    value <- x[person,change_variable]
    if (value != Inf)
      print(paste(person,'can',change_variable,'with',value))
  }
}

# Remove effects > 1000%
x[abs(x) > 1000] <- Inf
x[x != Inf] <- paste(round(x[x != Inf],2), '\\%', sep='')
x[x == Inf] <- '$\\infty$'

#
table <- xtable(x, label="tab:percentage_effects_in_aira",
                caption='Effects of Feeling less gloom, inadequate and relaxation on well-being', digits = 3)
print(table,
      file='inst/output/tab_percentage_effects_in_aira.tex',
      sanitize.text.function=function(str)gsub(" "," ",str,fixed=TRUE),
      floating=TRUE,
      booktabs=TRUE, floating.environment = 'table*')


print(x)
sink('inst/output/item_percentage_effects_in_aira.tex')
cat("\\begin{itemize}")
for (r in 1:nrow(x)) {
  for (c in 1:ncol(x)) {
    if(x[r,c] == "$\\infty$") next
    res = paste("\\item", rownames(x)[r], 'can', colnames(x)[c],'by', x[r,c])
    cat(res)
  }
}
cat("\\end{itemize}")
sink()

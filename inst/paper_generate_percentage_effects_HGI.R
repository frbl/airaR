library('aira')
library('xtable')
source('inst/generate_test_functions.R')
set.seed(12345)
test_model <- function(model, negative_variables) {
  aira <- Aira$new(bootstrap_iterations = 200, horizon= 10, var_model = model,
                   orthogonalize= FALSE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  improve_onrust <- aira$determine_percentage_effect('onrust', -10)
  improve_activity <- aira$determine_percentage_effect('beweging', 10)
  improve_ontspanning <- aira$determine_percentage_effect('ontspanning', 10)
  data.frame(
    improve_onrust = improve_onrust,
    improve_activity = improve_activity,
    improve_ontspanning = improve_ontspanning
  )
}

negative_variables <- c('onrust')
bust <- FALSE
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


decrease_onrust <- data.frame(
  'Activity.OB.downarrow.onrust.CB.' = c(p100849$improve_onrust.beweging, p100551$improve_onrust.beweging, p112098$improve_onrust.beweging, p110478$improve_onrust.beweging, p100713$improve_onrust.beweging),
  'Relaxation.OB.downarrow.onrust.CB.' = c(p100849$improve_onrust.ontspanning, p100551$improve_onrust.ontspanning, p112098$improve_onrust.ontspanning, p110478$improve_onrust.ontspanning, p100713$improve_onrust.ontspanning)
  )

increase_activity <- data.frame(
  'Onrust.OB.uparrow.activity.CB.' = c(p100849$improve_activity.onrust, p100551$improve_activity.onrust, p112098$improve_activity.onrust, p110478$improve_activity.onrust, p100713$improve_activity.onrust),
  'Relaxation.OB.uparrow.activity.CB.' = c(p100849$improve_activity.ontspanning, p100551$improve_activity.ontspanning, p112098$improve_activity.ontspanning, p110478$improve_activity.ontspanning, p100713$improve_activity.ontspanning)
)

increase_relaxation <- data.frame(
  'Onrust.OB.uparrow.relaxation.CB.' = c(p100849$improve_ontspanning.onrust, p100551$improve_ontspanning.onrust, p112098$improve_ontspanning.onrust, p110478$improve_ontspanning.onrust, p100713$improve_ontspanning.onrust),
  'Activity.OB.uparrow.relaxation.CB.' = c(p100849$improve_ontspanning.beweging, p100551$improve_ontspanning.beweging, p112098$improve_ontspanning.beweging, p110478$improve_ontspanning.beweging, p100713$improve_ontspanning.beweging)
)

x <- rbind(
   t(decrease_onrust),
   t(increase_activity),
   t(increase_relaxation)
)
x <- t(x)

x[x != Inf] <- paste(round(x[x != Inf],2), '\\%', sep='')
x[x == Inf] <- '$\\infty$'
x
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


#
table <- xtable(x, label="tab:effects_in_aira",
                 caption='Effects of Feeling less down, relaxation and activity on well-being', digits = 3)
print(table,
       file='inst/output/tab_percentage_effects_in_aira.tex',
       sanitize.text.function=function(str)gsub(" "," ",str,fixed=TRUE),
       floating=TRUE,
       booktabs=TRUE, floating.environment = 'table*')


print(x)


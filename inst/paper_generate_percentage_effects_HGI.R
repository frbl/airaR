library('aira')
library('xtable')
source('inst/generate_test_functions.R')
set.seed(12345)
bust = FALSE
test_model <- function(model, negative_variables) {
  aira <- Aira$new(bootstrap_iterations = bootstrap_iterations, horizon= 10, var_model = model,
                   orthogonalize= FALSE, reverse_order=FALSE) # Reverse order is order 2
  set_exo(model)
  #improve_somberheid <- aira$determine_percentage_effect('somberheid', -10)
  #improve_relaxation <- aira$determine_percentage_effect('ontspanning', 10)
  #improve_tekortschieten <- aira$determine_percentage_effect('tekortschieten', -10)
  data.frame(
    improve_piekeren     = aira$determine_percentage_effect('piekeren', -10),
    improve_onrust       = aira$determine_percentage_effect('onrust', -10),
    improve_opgewektheid = aira$determine_percentage_effect('opgewektheid', 10),
    improve_concentratie = aira$determine_percentage_effect('concentratie', 10),
    improve_eigenwaarde  = aira$determine_percentage_effect('eigenwaarde', 10)
  )
}

negative_variables <- c('somberheid', 'tekortschieten')
negative_variables <- c('piekeren', 'onrust')

# Check if any of the models give errors
#plot(vars::irf(model, boot=FALSE, ortho=FALSE))
# for i in `ls`; do a=`cat $i | grep -v NA | wc -l| awk '{print $1}'`; if [[ $a -ge 78 ]]; then; echo $i; fi; done
more_than_85 <- c(100009, 100023, 100070, 100122, 100298, 100398, 100540, 100551, 100566, 100713, 100849, 101003, 101553, 101619, 101703, 101725, 101908, 101912, 101938, 102167, 102434, 102444, 103773, 104142, 104255, 104583, 104591, 104648, 104703, 104789, 105233, 105266, 105590, 105774, 105882, 105890, 105927, 106001, 106098, 106281, 106319, 106423, 106794, 106973, 107039, 107153, 107543, 107596, 107735, 107979, 107993, 108084, 108211, 108531, 108750, 108978, 109747, 109751, 109824, 109911, 110160, 110285, 110329, 110344, 110360, 110454, 110460, 110478, 110544, 110637, 110645, 110676, 110799, 111013, 111157, 111177, 111191, 111231, 111264, 111268, 111289, 111297, 111299, 111306, 111310, 111323, 111335, 111350, 111369, 111378, 111395, 111410, 111413, 111459, 111488, 111492, 111495, 111528, 111529, 111543, 111570, 111583, 111592, 111632, 111737, 111738, 111742, 111755, 111779, 111787, 111845, 111849, 111884, 111904, 111952, 111958, 111977, 111983, 111984, 112001, 112024, 112028, 112031, 112032, 112037, 112046, 112098, 112099, 112153, 112172, 112186, 112217, 112302) 

people <- c(100849, 100551, 112098, 110478, 100713)
result <- list()
for (person in more_than_85) {
  cur_name <- paste('p', person, sep='')
  if(!exists(cur_name) || bust) {
    model <- testdata_var_model(person, bust)
    lag <- model$p
    assign(cur_name, test_model(model, negative_variables), envir = globalenv())
  }
  the_test_model <- get(cur_name)

  the_test_model[abs(the_test_model) > 1000] <- Inf
  #the_test_model[the_test_model != Inf] <- paste(round(the_test_model[the_test_model != Inf],2), '\\%', sep='')

  the_test_model <- t(the_test_model)
  res <- list()
  res[cur_name] = list(the_test_model[the_test_model != Inf, ])
  result <- append(result, res)
}

print(result)

browser()


#model <- testdata_var_model_100551(bust)
#lag <- model$p
#p100551 <- test_model(model, negative_variables)

#model <- testdata_var_model_112098(bust)
#lag <- model$p
#p112098 <- test_model(model, negative_variables)

#model <- testdata_var_model_110478(bust)
#lag <- model$p
#p110478 <- test_model(model, negative_variables)

#model <- testdata_var_model_100713(bust)
#lag <- model$p
#p100713 <- test_model(model, negative_variables)

print(p100849)
print(p100551)
print(p112098)
print(p110478)
print(p100713)

browser()



decrease_somberheid <- data.frame(
                              'decrease.feeling.gloomy.by.changing.relaxation' = c(p100849$improve_somberheid.ontspanning.percentage,
                                                                                   p100551$improve_somberheid.ontspanning.percentage,
                                                                                   p112098$improve_somberheid.ontspanning.percentage,
                                                                                   p110478$improve_somberheid.ontspanning.percentage,
                                                                                   p100713$improve_somberheid.ontspanning.percentage),
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

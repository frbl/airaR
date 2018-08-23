devtools::load_all('../../vars')
devtools::load_all('.')
source('inst/generate_test_functions.R')
library('ggplot2')
library('reshape2')
library('magrittr')
person <- 112217
simulation_data <- list()
pointer <- paste('p',112217,sep='')

data <- loadData(person)
data <- data[ , !(names(data) %in% c('X', 'date'))]
data <- data[,  c('piekeren', 'onrust', 'opgewektheid', 'concentratie', 'eigenwaarde')]
#data <- scale(data, scale=FALSE) %>% data.frame

#model_aut <- testdata_var_model(person, bust_cache=TRUE)
model <- calculateVar(data)[[1]]$varest

# The exogeneous columns start from the 'const' column
exos = colnames(model$dat)[which(colnames(model$dat) == 'const'):ncol(model$dat)]
exos

n.ahead = 3 

#' generate_meas_dum
#'
#' Function that repeats a pattern (or subset of the pattern) for n.ahead steps
generate_meas_dum <- function (pattern, n.ahead) {
  meas_dum = c()
  for (i in seq(n.ahead)) {
    idx = ((i - 1) %% length(pattern)) + 1
    val = pattern[idx]
    meas_dum = c(meas_dum, val)
  }
  meas_dum
}

#' variable_over_time_plot
#'
#' Function that takes data to plot, but then extends that data with the provided predicted data.
#' @param orig_data data.frame with the full data over time, containing the names in predicted_data
#' @param predicted_data a small data.frame with the newly predicted variables
variable_over_time_plot <- function (orig_data, predicted_data) {

  used_number_of_measurements <- nrow(orig_data)
  used_number_of_predictions <- nrow(predicted_data)

  for (i in seq(used_number_of_predictions)) {
    orig_data <- rbind(orig_data, rep(NA, ncol(predicted_data)))
  }

  for (i in seq(used_number_of_measurements)) {
    predicted_data <- rbind(rep(NA, ncol(predicted_data)), predicted_data)
  }

  orig_data[,'time'] <- seq(1, used_number_of_measurements + used_number_of_predictions)
  predicted_data[,'time'] <- seq(1, used_number_of_measurements + used_number_of_predictions)

  melted_orig_data <- melt(orig_data[,c('time', names(predicted_data))], id='time')
  melted_pred_data <- melt(predicted_data[,c('time', names(predicted_data))], id='time')

  g <- ggplot(data = melted_pred_data, aes(x=time, y=value, col=variable)) +
    facet_grid(facets = variable~., scales = "free_y")+
    geom_line(linetype="solid") +
    geom_line(linetype="solid", data=melted_orig_data, aes(x=time, y=value, col=-variable), colour='black') +
    theme_bw() +
    theme(legend.position="none")

  plotme(g)
}



# Generate the dummy variables for the future measurements
meas_1 = c(1,0,0)
meas_2 = c(0,1,0)
index = (max(model$dat$index)+1):(max(model$dat$index)+n.ahead)
index2 = index^2
outl = rep(0, n.ahead)
exos

day_3 = c(rep(1,3), rep(0,18))
day_4 = c(rep(0,3), day_3)[1:length(day_3)]
day_5 = c(rep(0,3), day_4)[1:length(day_4)]
day_6 = c(rep(0,3), day_5)[1:length(day_5)]
day_1 = c(rep(0,6), day_6)[1:length(day_6)]
day_2 = c(rep(0,3), day_1)[1:length(day_1)]

dumvar = data.frame(dailymeas_1 = generate_meas_dum(meas_1, n.ahead),
                    dailymeas_2 = generate_meas_dum(meas_2, n.ahead),
                    index = index,
                    index2 = index^2
                    )

exos

if ('day_1' %in% exos) {
  day_dummies <- data.frame(day_1 = generate_meas_dum(day_1, n.ahead),
                            day_2 = generate_meas_dum(day_2, n.ahead),
                            day_3 = generate_meas_dum(day_3, n.ahead),
                            day_4 = generate_meas_dum(day_4, n.ahead),
                            day_5 = generate_meas_dum(day_5, n.ahead),
                            day_6 = generate_meas_dum(day_6, n.ahead))
  dumvar <- cbind(dumvar, day_dummies)
}

## Add outlier dummies
outlnames <- exos[grepl( "outlier" , exos )]
outliers <- c()
for (outlname in outlnames ) {
  outliers <- cbind(outliers, outl)
  rownames(outliers) <- NULL
}
colnames(outliers) <- outlnames
dumvar <- cbind(dumvar, outliers)
dumvar

# +1 for the const col
stopifnot((ncol(dumvar) + 1) == length(exos))



aira_mod <- Aira( bootstrap_iterations = 100,
                 horizon = 10,
                 var_model = model,
                 orthogonalize = FALSE)

aira_mod$determine_percentage_effect('onrust', -10)







run_simulation = function(model, n.ahead, dumvar, outvar, intervention = NULL) {
  devtools::load_all('../../vars')
  # Create a matrix with all the endogeneous variables and the lagged versions therof
  Zy <- as.matrix(model$datamat[, 1:(model$K * (model$p + 1))])

  if(is.list(intervention)) { Zy <- intervention$intervention(Zy) }

  # Predict three steps ahead based on this data
  predictions <- predict(model, n.ahead = n.ahead, dumvar = data.matrix(dumvar), Zy=Zy, intervention = intervention)$fcst
  # Retrieve the actual point estimates of the forcasts
  pred <- as.data.frame(lapply(predictions, function(x) x[,'upper']))
  actual_data <- model$dat[, (names(model$dat) %in% names(pred))]
  message(100 * ((mean(pred[, outvar]) - mean(actual_data[, outvar])) / mean(actual_data[, outvar])))
  variable_over_time_plot(actual_data, pred)
  out <- pred[outvar] %>% unlist 
  message(mean(out) * 3 * 0.1 * sd(out))
  out %>% mean
}

# Now, when not intervening, this is the value for onrust.
no_intervention <- run_simulation(model = model, n.ahead = n.ahead, dumvar=dumvar, outvar = 'onrust')

what = -1.04/8
where = 'concentratie'

the_intervention <- list(where = where,
                         what = what,
                         intervention = function(Zy) {
  Zy[, where]

  #(mean(Zy[,'concentratie']) * - (0.885/4) )

  #Zy[,'concentratie'] <- Zy[,'concentratie'] +   (mean(Zy[,'concentratie']) * - (0.885/4) )
  #Zy[,'concentratie'] <- Zy[,'concentratie'] -   4* sd(Zy[,'concentratie'])

  #(mean(Zy[,'concentratie']) * - (0.885/4) )

  #Zy[,'concentratie'] <- Zy[,'concentratie']  * -(.74)
  Zy[, where] <- Zy[, where] + mean(Zy[, where]) * what
  #Zy[,'concentratie'] <- Zy[,'concentratie'] -   sd(Zy[,'concentratie'])
  return(Zy)
})

intervention <- run_simulation(model = model, n.ahead = n.ahead, dumvar=dumvar, outvar = 'onrust', intervention = the_intervention)

(no_intervention - 0.25) - no_intervention / no_intervention
intervention


needed_difference <- mean(var_model$y[,variable_to_improve]) *
                    length_of_effect *
                    (percentage / 100) *
                    sd(var_model$y[,current_variable_name])

100 * (intervention - no_intervention) / no_intervention 

#min_dat <- min(mat_data)
#max_dat <- max(mat_data)

#Sigma <- cov(mat_data)
#mu <- colMeans(mat_data)

#sim_data <- MASS::mvrnorm(n = 1000, mu, Sigma)
#sim_data[sim_data < min_dat] <- min_dat
#sim_data[sim_data > max_dat] <- max_dat


#the_test_model[abs(the_test_model) > 1000] <- Inf
##the_test_model[the_test_model != Inf] <- paste(round(the_test_model[the_test_model != Inf],2), '\\%', sep='')

#the_test_model <- t(the_test_model)
#res <- list()
#res[cur_name] = list(the_test_model[the_test_model != Inf, ])
#result <- append(result, res)

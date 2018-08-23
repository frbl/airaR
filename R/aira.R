#' The aira main class.
#'
#' @field bootstrap_iterations the number of bootstrap iterations to do for determining the significance of the effects
#' @field horizon the number of steps to look in the future
#' @field var_model the var model to perform the calculations on
#' @field orthogonalize use orthogonalized IRF
#' @importFrom methods setRefClass
#' @export Aira
#' @exportClass Aira
Aira <- setRefClass('Aira',
  fields = c(
    "bootstrap_iterations",
    "horizon",
    "var_model",
    "orthogonalize",
    "irf_cache",
    "vars_functions",
    "name"
  ),
  methods = list(
    initialize = function(bootstrap_iterations, horizon, var_model, orthogonalize, name="", reverse_order=FALSE) {
      #rm(list=ls(pos='.GlobalEnv',all=TRUE),pos='.GlobalEnv')
      irf_cache <<- list()
      vars_functions <<- VarsFunctions$new(bootstrap_iterations = bootstrap_iterations,
                                           horizon = horizon,
                                           var_model = var_model,
                                           orthogonalize = orthogonalize,
                                           reverse_order = reverse_order
                                           )
      callSuper(bootstrap_iterations= bootstrap_iterations, horizon = horizon,
                var_model = var_model, orthogonalize = orthogonalize, name = name)
    },

    determine_best_node_from_all = function(negative_variables = c()) {
      "Returns the total effect a variable has on all other variables in the network.
      If bootstrap iterations provided to aira is 0, we will not run any bootstrapping.
      If bootstrap iterations >0, we will only consider the significant effects in the response. If negative_variables
      are provided, we will convert those variables to positive ones (i.e., depression will become -1 * depression)
      @param negative_variables the variables to invert to positibe variables"
      total <- list()
      for (variable in 1:var_model$K) {
        variable_name <- .get_variable_name(variable)
        total[[variable_name]] <- 0
        for (response_name in 1:var_model$K) {
          if (variable == response_name) next
          response_name <- .get_variable_name(response_name)

          # If the current response variable is a negative variable, we'd like to invert the result
          multiplier <- ifelse(response_name %in% negative_variables, -1, 1)

          irf <- multiplier * .calculate_irf(variable_name=variable_name, response=response_name)
          total[[variable_name]] <- total[[variable_name]] + irf
        }
      }
      total
    },

    determine_effect_network = function(include_autoregressive_effects = FALSE) {
      "Returns the summed effect each node has on the other nodes node
       @param include_autoregressive_effects if enabled, autoregressive effects are used (default FALSE). Not yet supported!"
      K <- var_model$K
      effect_matrix <- matrix(0, K, K)
      colnames(effect_matrix) <- get_all_variable_names()
      rownames(effect_matrix) <- get_all_variable_names()

      for (variable in 1:var_model$K) {
        variable_name <- .get_variable_name(variable)
        for (response_name in 1:var_model$K) {
          if (variable == response_name & !include_autoregressive_effects) next # Don't consider autocorrelation
          response_name <- .get_variable_name(response_name)
          result <- .calculate_irf(variable_name = variable_name, response = response_name)
          effect_matrix[variable_name, response_name] <- result
        }
      }
      effect_matrix
    },

    determine_percentage_effect = function(variable_to_improve, percentage) {
      "Returns the percentage for each variable in the network (other then the provided variable)
      to be changed in order to change the variable_to_improve with the given percentage.
      @param variable_to_improve the name of the variable in the network which we'd like to improve
      @param percentage the percentage with which we'd like to improve the variable to improve"
      total <- list()
      for (variable in 1:var_model$K) {
        current_variable_name <- .get_variable_name(variable)

        # We won't intervene on the current variable
        if (current_variable_name == variable_to_improve) next

        # Calculate the cumulative IRF effect of the current variable on the variable we wish to change.
        effect <- .calculate_irf(current_variable_name, variable_to_improve)

        # IF there is absolutely no effect, we just skip and move on to the next variable.
        if (abs(effect) < 0.0001) {
          total[[current_variable_name]] <- list(percentage = Inf, length_of_effect = Inf)
          next
        }

        length_of_effect <- determine_length_of_effect(current_variable_name, variable_to_improve, 1, first_effect_only=FALSE, plot_results=FALSE)
        length_of_effect <- ceiling(length_of_effect$effective_horizon)

        needed_difference <- mean(var_model$y[,variable_to_improve]) * length_of_effect * (percentage / 100) * sd(var_model$y[,current_variable_name])
        message(paste('Needed difference:', needed_difference, ', effect: ', effect, ' SD of var to use:',  sd(var_model$y[,current_variable_name])))

        denominator <- mean(var_model$y[,current_variable_name]) * effect * sd(var_model$y[,variable_to_improve])

        message(paste('Numerator', needed_difference))
        message(paste('Denominator', denominator))

        needed_difference <- needed_difference / denominator

        total[[current_variable_name]] <- list(percentage = needed_difference * 100, length_of_effect = length_of_effect)
      }
      total
    },

    determine_length_of_effect = function(variable_name, response, measurement_interval, first_effect_only=FALSE, plot_results=FALSE) {
      "Returns the time in minues a variable is estimated to have an effect on another variable.
      @param variable_name the name of the variable to receive the shock
      @param response the name of the variable to respond to the shock
      @param measurement interval the time in minutes between two measurements"

      # TODO: The effect is currently not cached.
      if(bootstrap_iterations <= 0) {
        result <- vars_functions$irf(from=variable_name, to=response)
        lower <- result$irf[[variable_name]]
        upper <- lower
      } else {
        print('Determining with bootstrap')
        result <- vars_functions$bootstrapped_irf(from=variable_name, to=response)
        lower <- result$Lower[[variable_name]]
        upper <- result$Upper[[variable_name]]
      }

      if(plot_results) plot(result)

      # In order to be able to support normal irf (not only bootstrapped irf) we should incorporate rounding errors.
      threshold <- 1e-4
      low <- (lower > threshold)
      high <- (upper < -threshold)
      begin <- 1
      end <- begin
      effect_started <- FALSE
      effective_horizon <- 0
      exact_length <- 0
      for (i in 1:horizon) {
        if (low[i] | high[i]) {
          # If the beginning of the effect is not on the first measurement, we should interpolate.
          if (i > 1 & !effect_started) {
            if (low[i]){
              begin = i - ((lower[i] - threshold) / (lower[i] - lower[i - 1]))
            } else if(high[i]) { # high[i] == true
              begin = i - ((upper[i] + threshold) / (upper[i] - upper[i - 1]))
            }
          }
          effect_started = TRUE
        } else {
          if (effect_started) {
            if (low[i-1]){
              end = i - 1 + (1 - ((lower[i] - threshold) / (lower[i] - lower[i - 1])))
            } else if(high[i-1]) { # high[i-1] == true
              end = i - 1 + (1 - ((upper[i] + threshold) / (upper[i] - upper[i - 1])))
            }
            effective_horizon <- end
            effect_started <- FALSE
            exact_length <- exact_length + (end - begin)
            if(first_effect_only) break
          }
        }
      }
      # The effect has never stopped?
      if(effect_started) {
        exact_length <- exact_length + ((horizon - 1) - begin)
        effective_horizon <- horizon
      }

      data.frame(
        length_in_minutes = exact_length * measurement_interval,
        length_of_effect = exact_length,
        effective_horizon = effective_horizon
      )
    },
    get_all_variable_names = function() {
      "returns all variables in the var model"
      dimnames(var_model$y)[[2]]
    },

    .get_variable_name = function(id) {
      "Returns a variable name based on its id"
      get_all_variable_names()[[id]]
    },

    .calculate_irf = function(variable_name, response = NULL, plot_results = FALSE){
      "Calculates IRF and returns the total effect"
      resulting_score <- 0
      result <- ''
      key <- paste(variable_name, response, sep="|")
      # If we have processed this call before, return it from the cache
      if((key %in% names(irf_cache)) & !plot_results) return(irf_cache[[key]])
      if (bootstrap_iterations > 0) {
        ## TERRIBLE;
        set_exo(var_model)

        result <- vars_functions$bootstrapped_irf(from=variable_name, to=response)
        low  <- (result$Lower[[variable_name]] * (result$Lower[[variable_name]] > 0))
        high <- (result$Upper[[variable_name]] * (result$Upper[[variable_name]] < 0))
        sign_effects <- (low + high)[, !dimnames(result$Lower[[variable_name]])[[2]] %in% variable_name]
        resulting_score <- sum(sign_effects)
      } else {
        result <- vars_functions$irf(from=variable_name, to=response)
        resulting_score <- result$irf[[variable_name]][, !dimnames(result$irf[[variable_name]])[[2]] %in% variable_name, drop=FALSE]
        resulting_score <- as.numeric(colSums(resulting_score))
      }
      if (plot_results) plot(result)

      irf_cache[[key]] <<- resulting_score
      resulting_score
    }
  )
)


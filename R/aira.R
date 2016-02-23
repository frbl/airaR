#rm(list=ls(pos='.GlobalEnv',all=TRUE),pos='.GlobalEnv')
#' The aira main class.
#'
#' @field bootstrap_iterations the number of bootstrap iterations to do for determining the significance of the effects
#' @field horizon the number of steps to look in the future
#' @field var_model the var model to perform the calculations on
#' @field orthogonalize use orthogonalized IRF
#' @importFrom vars VAR irf Bcoef Phi
#' @importFrom methods setRefClass
#' @export Aira
#' @exportClass Aira
Aira <- setRefClass('Aira',
  fields = c(
    "bootstrap_iterations",
    "horizon",
    "var_model",
    "orthogonalize",
    "irf_cache"
  ),
  methods = list(
    initialize = function(bootstrap_iterations, horizon, var_model, orthogonalize, reverse_order=FALSE) {
      # In order to allow for a reverse ordering in variables, we have to override a function in the vars package.
      # This is done here. Note the fact that the reverse order var is stored in a global variable. This is due to
      # The fact that we are not calling the function directly, and thus cannot pass an argument to it.
      assign('reverse_order', reverse_order, envir= .GlobalEnv)
      override_function("Psi.varest","vars",myPsi.varest)
      irf_cache <<- list()
      callSuper(bootstrap_iterations= bootstrap_iterations, horizon = horizon,
                var_model = var_model, orthogonalize = orthogonalize)
    },

    determine_best_node_from_all = function() {
      "Returns the total effect a variable has on all other variables in the network.
      If bootstrap iterations provided to aira is 0, we will not run any bootstrapping.
      If bootstrap iterations >0, we will only consider the significant effects in the response"
      total <- list()
      for (variable in 1:var_model$K) {
        variable_name <- .get_variable_name(variable)
        total[variable_name] <- .calculate_irf(variable_name)
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
        for (response_name in 1:var_model$K) {
          if (variable == response_name & !include_autoregressive_effects) next # Don't consider autocorrelation
          variable_name <- .get_variable_name(variable)
          response_name <- .get_variable_name(response_name)
          result <- .calculate_irf(variable_name = variable_name, response = response_name)
          effect_matrix[variable_name, response_name] <- result
        }
      }
      effect_matrix
    },

    determine_percentage_effect  = function(variable_to_improve, percentage) {
      "Returns the percentage for each variable in the network (other then the provided variable)
      to be changed in order to change the variable_to_improve with the given percentage.
      @param variable_to_improve the name of the variable in the network which we'd like to improve
      @param percentage the percentage with which we'd like to improve the variable to improve"
      total <- list()
      for (variable in 1:var_model$K) {
        variable_name <- .get_variable_name(variable)
        if (variable_name == variable_to_improve)
          next

        effect <- .calculate_irf(variable_name, variable_to_improve)
        if (abs(effect) < 0.0001) {
          total[variable_name] <- Inf
          next
        }

        needed_difference <- mean(var_model$y[,variable_to_improve]) * (percentage / 100)
        needed_difference <- needed_difference / effect
        needed_difference <- needed_difference / mean(var_model$y[,variable_name])

        total[variable_name] <- needed_difference
      }
      total
    },

    determine_length_of_effect = function(variable_name, response, measurement_interval) {
      "Returns the time in minues a variable is estimated to have an effect on another variable.
      @param variable_to_shock the name of the variable to receive the shock
      @param variable_to_respond the name of the variable to respond to the shock
      @param measurement interval the time in minutes between two measurements"

      if(bootstrap_iterations <= 0) stop('Bootstrapping is needed for this function.')

      result <- vars::irf(var_model, impulse=variable_name,
                          response = response, n.ahead = horizon, cumulative= FALSE,
                          runs = bootstrap_iterations, boot=TRUE, ortho = orthogonalize)



      lower <- result$Lower[[variable_name]]
      upper <- result$Upper[[variable_name]]

      low <- (lower > 0)
      high <- (upper < 0)
      begin <- 1
      end <- -1
      prev <- FALSE

      for (i in 1:horizon) {
        if (low[i] | high[i]) {
          # If the beginning of the effect is not on the first measurement, we should interpolate.
          if (i > 1 & !prev) {
            if (low[i]){
              begin = i - ((lower[i] - 0) / (lower[i] - lower[i-1]))
            } else {
              begin = i - ((upper[i] - 0) / (upper[i] - upper[i-1]))
            }
          }
          prev = TRUE
        } else {
          if (prev) {
            if (low[i-1]){
              end = i + ((lower[i] - 0) / (lower[i] - lower[i-1]))
            } else {
              end = i + ((upper[i] - 0) / (upper[i] - upper[i-1]))
            }
            prev = FALSE
            break
          }
        }
      }

      (end - begin) * measurement_interval
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
      key <- paste(variable_name, response, "|")

      # If we have processed this call before, return it from the cache
      if((key %in% names(irf_cache)) & !plot_results) return(irf_cache[[key]])

      if (bootstrap_iterations > 0) {
        result <- vars::irf(var_model, impulse=variable_name,
                 response = response, n.ahead = horizon, cumulative= FALSE,
                 boot=TRUE, runs = bootstrap_iterations, ortho = orthogonalize)

        low <- (result$Lower[[variable_name]] * (result$Lower[[variable_name]] > 0))
        high <- (result$Upper[[variable_name]] * (result$Upper[[variable_name]] < 0))
        sign_effects <- (low + high)[, !dimnames(result$Lower[[variable_name]])[[2]] %in% variable_name]
        resulting_score <- sum(sign_effects)
      } else {
        result <- vars::irf(var_model, impulse=variable_name, response = response, n.ahead = horizon, cumulative= FALSE, ortho = orthogonalize, boot= FALSE)
        resulting_score <- result$irf[[variable_name]][, !dimnames(result$irf[[variable_name]])[[2]] %in% variable_name, drop=FALSE]
        resulting_score <- as.numeric(colSums(resulting_score))
      }
      if (plot_results) plot(result)

      irf_cache[key] <<- resulting_score
      resulting_score
    }
  )
)


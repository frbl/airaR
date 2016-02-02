#' The aira output class.
#'
#' @field aira an instance of aira
AiraOutput <- setRefClass(
  'AiraOutput',
  fields = c("aira"),
  methods = list(
    print_overview_percentage_effect = function(percentage_to_improve = 10) {
      "Prints the percentage increases for all variables in the model.
      @param percentage_to_improve the percentage to improve the variables with (default 10%)"
      result <- ""
      for (variable_to_improve in aira$get_all_variable_names()) {
        percentage_effects <- aira$determine_percentage_effect(variable_to_improve = variable_to_improve, percentage_to_improve)
        if (length(percentage_effects) > 0) {
          if(result == "") result <- paste(result, 'Effect achieved:\n', sep= "")
          result <- .determine_effects(percentage_effects=percentage_effects, result = result,
                                       variable_to_improve = variable_to_improve,
                                       percentage_to_improve = percentage_to_improve)
        }
      }
      result
    },
    print_percentage_effect = function(variable_to_improve, percentage_to_improve = 10) {
      "Prints the percentage increases needed for a specific variable.
      @param variable_to_improve the actuabl variable to improve
      @param percentage_to_improve the percentage to improve the variable with (default 10%)"
      result <- ""
      percentage_effects <- aira$determine_percentage_effect(variable_to_improve = variable_to_improve, percentage_to_improve)
      if (length(percentage_effects) > 0) {
        result <- paste(result, 'Effect achieved:\n', sep= "")
        result <- .determine_effects(percentage_effects=percentage_effects, result = result,
                                     variable_to_improve = variable_to_improve,
                                     percentage_to_improve = percentage_to_improve)
      }
      result
    },
    export_model_to_json = function() {

    },
    .determine_effects = function(percentage_effects, result, variable_to_improve, percentage_to_improve) {
      for (name in names(percentage_effects)) {
        effect <- round(percentage_effects[[name]] * 100)
        direction <- ifelse(sign(effect) == 1, "increasing", "decreasing")
        result <- paste(result, "You could increase your ", variable_to_improve, " with ", percentage_to_improve, '% ', sep ="")
        result <- paste(result, "by ", direction, " your ", name, " with ", abs(effect), '%\n', sep ="")
      }
      result
    }
  )
)

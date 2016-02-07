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
    export_model = function() {
      "Exports the effects of all variables in the network"
      network <- .generate_network()
      scores <- aira$determine_best_node_from_all()

      val <- c()
      for(i in 1:nrow(network$nodes)) {
        node <- network$nodes[i,]
        val <- c(val, scores[[node$name]])
      }
      network$nodes$val <- val
      network
    },
    export_model_to_json = function() {
      "Exports the effects of all variables in the network in a JSON structure, that can be interpreted by the AIRA JS library"
      jsonlite::toJSON(export_model())
    },
    .determine_effects = function(percentage_effects, result, variable_to_improve, percentage_to_improve) {
      for (name in names(percentage_effects)) {
        effect <- round(percentage_effects[[name]] * 100)
        direction <- ifelse(sign(effect) == 1, "increasing", "decreasing")
        result <- paste(result, "You could increase your ", variable_to_improve, " with ", percentage_to_improve, '% ', sep ="")
        result <- paste(result, "by ", direction, " your ", name, " with ", abs(effect), '%\n', sep ="")
      }
      result
    },
    .generate_network = function() {
      varres <- aira$var_model$varresult

      var_names <- names(varres)

      # create a data.frame of significant connections, order it by coefficient strength
      linkstr <- NULL
      i <- 0

      nodedata <- NULL
      linkdata <- NULL
      rnames <- NULL
      nodecount <- 0
      nodedegree <- list()

      # Create node data
      for (varname in var_names) {
        if (varname %in% rnames) next
        nodedata <- rbind(nodedata,data.frame(index=nodecount,
                                              name=varname,
                                              key=varname,
                                              stringsAsFactors=FALSE))
        rnames <- c(rnames,varname)
        nodecount <- nodecount+1
      }

      # Create edge data
      for (equation in varres) {
        i <- i+1
        eqsum <- summary(equation)
        eqname <- var_names[i]
        for (fromnodename in var_names) {
          if (fromnodename == eqname) next

          from_name_in_equation <- TRUE
          any_pval_significant <- FALSE
          significant_lag <- 0

          for(lag in 1:aira$var_model$p){
            from_name_in_equation <- from_name_in_equation & (paste(fromnodename, ".l", lag, sep = "") %in% rownames(coef(eqsum)))
            p_val <- eqsum$coefficients[paste(fromnodename,'.l1',sep=""),4]
            any_pval_significant <- any_pval_significant | (p_val <= 0.05)

            # The first significant lag is considered the most important, and is used for plotting the coef
            if (significant_lag == 0 & p_val <= 0.05) significant_lag <- lag
          }
          if (!from_name_in_equation | !any_pval_significant) next

          nodedegree[[fromnodename]] <- ifelse(is.null(nodedegree[[fromnodename]]),0,nodedegree[[fromnodename]]) + 1
          nodedegree[[eqname]] <- ifelse(is.null(nodedegree[[eqname]]),0,nodedegree[[eqname]]) + 1

          coef <- eqsum$coefficients[paste(fromnodename,'.l', significant_lag, sep=""),1]

          linkstr <- rbind(linkstr,data.frame(source=fromnodename,
                                              target=eqname,
                                              coef=abs(coef),
                                              sign=sign(coef),
                                              stringsAsFactors=FALSE))

          tonode<- which(eqname == rnames) - 1
          fromnode <- which(fromnodename == rnames) - 1
          linkdata <- rbind(linkdata,data.frame(source=fromnode,
                                                target=tonode,
                                                distance=0.9,
                                                weight=toString(coef),
                                                stringsAsFactors=FALSE))
        }
      }
      list(links = linkdata,nodes = nodedata)
    }
  )
)

#' The aira output class.
#'
#' @field aira an instance of aira
#' @importFrom jsonlite toJSON
#' @importFrom jsonlite fromJSON
#' @importFrom methods setRefClass
#' @export AiraOutput
#' @exportClass AiraOutput
AiraOutput <- setRefClass(
  'AiraOutput',
  fields = c("aira"),
  methods = list(
    print_overview_percentage_effect = function(percentage_to_improve = 10) {
      "Prints the percentage increases for all variables in the model.
      @param percentage_to_improve the percentage to improve the variables with (default 10 percent)"
      result <- ""
      for (variable_to_improve in aira$get_all_variable_names()) {
        percentage_effects <- aira$determine_percentage_effect(variable_to_improve = variable_to_improve, percentage_to_improve)
        if (length(percentage_effects) > 0) {
          if(result == "") result <- paste(result, 'Effect achieved:\n', sep= "")
          result <- .determine_effects(percentage_effects = percentage_effects,
                                       result = result,
                                       variable_to_improve = variable_to_improve,
                                       percentage_to_improve = percentage_to_improve,
                                       print_newlines = TRUE)
        }
      }
      result
    },
    print_percentage_effect = function(variable_to_improve, percentage_to_improve = 10, print_newlines = FALSE, print_title=FALSE) {
      "Prints the percentage increases needed for a specific variable.
      @param variable_to_improve the actuabl variable to improve
      @param percentage_to_improve the percentage to improve the variable with (default 10 percent)"
      result <- ""
      percentage_effects <- aira$determine_percentage_effect(variable_to_improve = variable_to_improve, percentage_to_improve)
      if (length(percentage_effects) > 0) {
        if (print_title) {
          result <- paste(result, 'Effect achieved:', sep= "")
          separator <- ifelse(print_newlines, '\n', ' ')
          result <- paste(result, separator, sep = "")
        } else {
          result <- ""
        }

        result <- .determine_effects(percentage_effects=percentage_effects, result = result,
                                     variable_to_improve = variable_to_improve,
                                     percentage_to_improve = percentage_to_improve,
                                     print_newlines = print_newlines)
      }
      result
    },
    export_model = function(negative_variables = c()) {
      "Exports the effects of all variables in the network"
      network <- .generate_network()
      scores <- aira$determine_best_node_from_all(negative_variables)

      # Put the results in a list, so we can change the complete column of the dataframe in
      # one statement
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
      toJSON(export_model())
    },

    export_var_network = function(autoregressive = FALSE) {
      "Exports the coefficients of all variables in the network"
      network <- .generate_network(autoregressive)
      names <- dimnames(aira$var_model$y)[[2]]
      .convert_to_matrix(nodes = network$nodes, links = network$links, names=names)
    },

    .convert_to_matrix = function(nodes, links, names){
      "Convert the two matrices of links and lists to a single adjacency matrix"
      output_network <- create_result_matrix(names = names)
      if(is.null(links)) return(output_network)
      for (i in 1:nrow(links)) {
        link <- links[i,]
        if (is.na(link$source)) next
        source <- nodes[nodes$index == link$source, 'key']
        target <- nodes[nodes$index == link$target, 'key']
        weight <- as.numeric(link$weight)
        output_network[source, target] <- output_network[source, target] + weight
      }
      output_network
    },

    .determine_effects = function(percentage_effects, result, variable_to_improve, percentage_to_improve, print_newlines) {
      index <- 0
      for (name in names(percentage_effects)) {
        index <- index + 1
        percentage_effect = percentage_effects[[name]]$percentage
        if(abs(percentage_effect) == Inf) next
        effect <- round(percentage_effect)
        direction <- ifelse(sign(effect) == 1, "increasing", "decreasing")
        direction_improvement <- ifelse(sign(percentage_to_improve) == 1, "increase", "decrease")
        result <- paste(result, "You could ",direction_improvement," your ", variable_to_improve, " with ", abs(percentage_to_improve), '% ', sep ="")
        result <- paste(result, "by ", direction, " your ", name, " with ", abs(effect), '%', sep = "")
        if (print_newlines) {
          result <- paste(result, '\n', sep = "")
        } else {
          # Don't add a comma to the end.
          if (index == length(names)) {
            result <- paste(result, '.', sep = "")
          } else {
            result <- paste(result, ', ', sep = "")
          }
        }
      }
      result
    },
    .generate_network = function(autoregressive = FALSE) {
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
          if (!autoregressive && fromnodename == eqname) next
          any_pval_significant <- FALSE
          significant_lag <- -1

          for(lag in 1:aira$var_model$p){
            if (!paste(fromnodename, ".l", lag, sep = "") %in% rownames(coef(eqsum))) next

            p_val <- eqsum$coefficients[paste(fromnodename,'.l', lag, sep=""),4]
            any_pval_significant <- any_pval_significant | (p_val <= 0.05)

            # The first significant lag is considered the most important, and is used for plotting the coef
            if (significant_lag == -1 && p_val <= 0.05) significant_lag <- lag
          }
          if (!any_pval_significant) next

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

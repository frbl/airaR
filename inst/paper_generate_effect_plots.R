require(aira)
require(qgraph)
source('inst/generate_test_functions.R')
# Remove all plots
if(!is.null(dev.list()) && !is.na(dev.list()['pdf'])) dev.off(dev.list()["pdf"])
if(!is.null(dev.list()) && !is.na(dev.list()['RStudioGD'])) dev.off(dev.list()["RStudioGD"])

run_aira <- function(model, name) {
  aira <- Aira$new(bootstrap_iterations = bootstrap_iterations, horizon= 10, var_model = model,
                   orthogonalize= TRUE, reverse_order=FALSE)
  set_exo(model)
  aira_output <- AiraOutput$new(aira = aira)
  result <- aira$determine_best_node_from_all()
  network <- aira_output$export_model()
  names <- dimnames(model$y)[[2]]
  print(network)
  output <- aira_output$.convert_to_matrix(nodes = network$nodes, links = network$links, names=names)

  print(output)
  rownames(output) <- c('Activity', 'Depression')
  colnames(output) <- c('Activity', 'Depression')
  list(output = output, nodes = (network$nodes[,'val']*50))
}

# Check if any of the models give errors
pp1 <- run_aira(model = testdata_var_model_pp1(), 'pp1')
pp2 <- run_aira(model = testdata_var_model_pp2(), 'pp2')
pp4 <- run_aira(model = testdata_var_model_pp4(), 'pp4')
pp5 <- run_aira(model = testdata_var_model_pp5(), 'pp5')
names <- c('Activity', 'Depression')
plot(qgraph::qgraph(pp1$output, vsize=pp1$nodes, asize=10, edgeConnectPoints=200, label.norm='OOOOOOOO'))
plot(qgraph::qgraph(pp2$output, vsize=pp2$nodes, asize=10, edgeConnectPoints=200, label.norm='OOOOOOOO'))
plot(qgraph::qgraph(pp4$output, vsize=pp4$nodes, asize=10, edgeConnectPoints=200, label.norm='OOOOOOOO'))
plot(qgraph::qgraph(pp5$output, vsize=pp5$nodes, asize=10, edgeConnectPoints=200, label.norm='OOOOOOOO'))


write.csv(pp5$output)

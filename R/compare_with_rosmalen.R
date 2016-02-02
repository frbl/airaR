library('xtable')

variable_name <- 'U'

dataset = suppressWarnings(read.spss("pp1 nieuw compleet.sav", to.data.frame=TRUE))
endodata = dataset[,c('SomBewegUur', 'SomPHQ')]
exodata = dataset[,c('UitbijterPHQ','UitbijterBeweg')]
var.2c = VAR(endodata, exogen=exodata, p=2, type='const')
resmat <- new_restriction_matrix(var.2c)
resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l2', 0, resmat)
resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l1', 0, resmat)
resmat <- update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l2', 0, resmat)
resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'UitbijterPHQ', 0, resmat)
resmat <- format_restriction_matrix(var.2c, resmat)

var.2c <- restrict(var.2c, method = 'manual', resmat = resmat)
var.2c$exogen <- exodata
print(Bcoef(var.2c))

# Values calculated using our method
ORDER <- 0
order1_actdep_mine <- irf(var.2c, impulse="SomBewegUur", response="SomPHQ", ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf
ORDER <- 1
order2_actdep_mine <- (irf(var.2c, impulse="SomBewegUur", response="SomPHQ", ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)

ORDER <- 0
order1_depact_mine <- (irf(var.2c, impulse="SomPHQ", response="SomBewegUur",  ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)
ORDER <- 1
order2_depact_mine <- (irf(var.2c, impulse="SomPHQ", response="SomBewegUur",  ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)

# Values calculated using Rosmalen method (STATA)
order1_actdep <- c(-0.39, -0.82, -1.15, -1.46, -1.70, -1.90,-2.06, -2.19, -2.30, -2.38, -2.45)
order2_actdep <- c(0, -0.27, -0.50, -0.75, -0.95, -1.13, -1.28, -1.40, -1.50, -1.58, -1.65)
order1_depact <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
order2_depact <- c(-0.35, -0.51, -0.68, -0.80, -0.89, -0.97, -1.03, -1.08, -1.12, -1.15, -1.17)

print('Differences')
print(round(as.vector(order1_actdep_mine[[1]]) - order1_actdep,digits = 2))
print(round(as.vector(order2_actdep_mine[[1]]) - order2_actdep,digits = 2))
print(round(as.vector(order1_depact_mine[[1]]) - order1_depact,digits = 2))
print(round(as.vector(order2_depact_mine[[1]]) - order2_depact,digits = 2))

#aira <- Aira$new(bootstrap_iterations = 1000, steps = 90, var_model = var.2c)
#tot <- aira$determine_best_node_from_all()

print('Total effect of the nodes, for a time of 9 measurements')
print(tot)


for(variable_to_improve in dimnames(var.2c$y)[[2]]) {
  tot <- aira$determine_percentage_effect(variable_to_improve = variable_to_improve, 10)
  if(length(tot) > 0) {
    print('Effect achieved')
    for(name in names(tot)) {
      print(paste("You could increase your ", name, " with ", round(tot[[name]] * 100), '%', sep=""))
    }
  }
}

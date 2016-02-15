library('aira')
library('vars')
data_set <- autovar::read_spss("inst/pp1_nieuw_compleet.sav", to.data.frame=TRUE)
endodata <- data_set[,c('SomBewegUur', 'SomPHQ')]
exogedata <- data_set[,c('UitbijterPHQ','UitbijterBeweg')]
#assign("endodata", "endodata", envir = .GlobalEnv)

var.2c <- vars::VAR(endodata, p=2, type='const', exogen=exogedata)
negative_variables <- c('SomPHQ')

aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = var.2c, orthogonalize= TRUE)
tot <- aira$determine_best_node_from_all()
print(tot)

norm <- irf(var.2c, ortho = FALSE, boot= FALSE)
var.2c.pos <-convert_var_to_positive(var.2c, negative_variables = negative_variables)
pos <- irf(var.2c.pos, ortho = FALSE, boot = FALSE)

aira <- Aira$new(bootstrap_iterations = 0, horizon= 10, var_model = var.2c.pos, orthogonalize= TRUE)
tot <- aira$determine_best_node_from_all()
print(tot)

print(norm$irf$SomBewegUur)
print(pos$irf$SomBewegUur)
print(Bcoef(var.2c))
print(Bcoef(new))

# Shock Stress
# Shock Activity
# Check how to improve PA

#library('xtable')
#library('autovar',character.only=TRUE)

# compare_with_rosmalen <- function() {
#   variable_name <- 'U'
#   dataset = suppressWarnings(read.spss("inst/pp1_nieuw_compleet.sav", to.data.frame=TRUE))
#   endodata = dataset[,c('SomBewegUur', 'SomPHQ')]
#   exodata = dataset[,c('UitbijterPHQ','UitbijterBeweg')]
#   var.2c = VAR(endodata, exogen = exodata, p=2, type='const')
#   resmat <- new_restriction_matrix(var.2c)
#   resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l2', 0, resmat)
#   resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'SomPHQ.l1', 0, resmat)
#   resmat <- update_restriction_matrix(var.2c, 'SomPHQ', 'SomBewegUur.l2', 0, resmat)
#   resmat <- update_restriction_matrix(var.2c, 'SomBewegUur', 'UitbijterPHQ', 0, resmat)
#   resmat <- format_restriction_matrix(var.2c, resmat)
#
#   var.2c <- restrict(var.2c, method = 'manual', resmat = resmat)
#   var.2c$exogen <- exodata
#   print(Bcoef(var.2c))
#
#   # Values calculated using our method
#   ORDER <- 0
#   order1_actdep_mine <- irf(var.2c, impulse="SomBewegUur", response="SomPHQ", ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf
#   ORDER <- 1
#   order2_actdep_mine <- (irf(var.2c, impulse="SomBewegUur", response="SomPHQ", ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)
#
#   ORDER <- 0
#   order1_depact_mine <- (irf(var.2c, impulse="SomPHQ", response="SomBewegUur",  ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)
#   ORDER <- 1
#   order2_depact_mine <- (irf(var.2c, impulse="SomPHQ", response="SomBewegUur",  ortho=TRUE, cumulative = TRUE, boot=100, ci=.95)$irf)
#
#   # Values calculated using Rosmalen method (STATA)
#   order1_actdep <- c(-0.39, -0.82, -1.15, -1.46, -1.70, -1.90,-2.06, -2.19, -2.30, -2.38, -2.45)
#   order2_actdep <- c(0, -0.27, -0.50, -0.75, -0.95, -1.13, -1.28, -1.40, -1.50, -1.58, -1.65)
#   order1_depact <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
#   order2_depact <- c(-0.35, -0.51, -0.68, -0.80, -0.89, -0.97, -1.03, -1.08, -1.12, -1.15, -1.17)
#
#   print('Differences')
#   print(round(as.vector(order1_actdep_mine[[1]]) - order1_actdep,digits = 2))
#   print(round(as.vector(order2_actdep_mine[[1]]) - order2_actdep,digits = 2))
#   print(round(as.vector(order1_depact_mine[[1]]) - order1_depact,digits = 2))
#   print(round(as.vector(order2_depact_mine[[1]]) - order2_depact,digits = 2))
#
#   #aira <- Aira$new(bootstrap_iterations = 1000, steps = 90, var_model = var.2c)
#   #tot <- aira$determine_best_node_from_all()
#
#   print('Total effect of the nodes, for a time of 9 measurements')
# }



# x <- 0:10
#
# oirf <-  c(-.387845, -.433359, -.331886, -.302933, -.243305, -.20158, -.162092, -.130596, -.104354, -.083257, -.066234)
# lower <- c(-.793465, -.774665, -.603678, -.561734, -.465791, -.399364, -.332544, -.278331, -.231007, -.191862, -.159004)
# upper <- c(.017774, -.092052, -.060094, -.044132, -.020819, -.003796, .008361, .017138, .022299, .025348, .026536)
#
# test_data_a_d_1 <- data.frame(oirf = oirf,
#                               lower = lower,
#                               upper = upper,
#                               x = x)
#
# a_d_1 <- ggplot(test_data_a_d_1, aes(x)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
#   geom_line(aes(y = oirf, colour = "A->D (1)")) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   scale_y_continuous(limits = c(-.8, .05))
#
#
# plot(a_d_1)
# test_data_a_d_1
#
# oirf <-  c(0, 1.9e-18, 1.0e-17, 9.0e-18, 9.3e-18, 7.9e-18, 6.8e-18, 5.6e-18, 4.6e-18, 3.7e-18, 3.0e-18)
# lower <- c(0, -4.2e-17, -3.6e-17, -3.6e-17, -3.3e-17, -2.9e-17, -2.6e-17, -2.2e-17, -1.9e-17, -1.6e-17, -1.4e-17)
# upper <- c(0, 4.6e-17, 5.7e-17, 5.4e-17, 5.2e-17, 4.5e-17, 3.9e-17, 3.3e-17, 2.8e-17, 2.3e-17, 2.0e-17)
#
# test_data_d_a_1 <- data.frame(oirf = oirf,
#                               lower = lower,
#                               upper = upper,
#                               x =x)
#
# d_a_1 <- ggplot(test_data_d_a_1, aes(x)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
#   geom_line(aes(y = oirf, colour = "D->A (1)")) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   scale_y_continuous(limits = c(-0.8, 0.05))
#
#
# plot(d_a_1)
#
#
# oirf <-  c(0, -.271674, -.229268, -.243989, -.208100, -.180166, -.148775, -.122069, -.098736, -.07944, -.063565)
# lower <- c(0, -.536735, -.461581, -.477173, -.411662, -.363459, -.308084, -.261117, -.218597, -.182676, -.152061)
# upper <- c(0, -.006614, 0.003045, -.010804, -.004538, 0.003126, 0.010533, 0.016979, 0.021125, 0.023797, 0.024931)
#
# test_data_a_d_2 <- data.frame(oirf = oirf,
#                               lower = lower,
#                               upper = upper,
#                               x = x)
#
# a_d_2 <- ggplot(test_data_a_d_2, aes(x)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
#   geom_line(aes(y = oirf, colour = "A->D (2)")) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   scale_y_continuous(limits = c(-0.8, 0.05))
#
# plot(a_d_2)
#
# oirf <- c(-.352816, -.157569, -.166737, -.117503, -.098018, -.075869, -.060656, -.047811, -.03792, -.029994, -.023753)
# lower <- c(-.719282, -.322125, -.330862, -.23632, -.203247, -.161986, -.134852, -.110341, -.09138, -.075413, -.062529)
# upper <- c(.01365, .006987, -.002611, .001315, .00721, .010248, .013541, .014718, .015541, .015425, .015023)
#
# test_data_d_a_2 <- data.frame(oirf = oirf,
#                               lower = lower,
#                               upper = upper,
#                               x =x)
#
# d_a_2 <- ggplot(test_data_d_a_2, aes(x)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey70") +
#   geom_line(aes(y = oirf, colour = "D->A (2)")) +
#   geom_hline(yintercept = 0) +
#   geom_vline(xintercept = 0) +
#   scale_y_continuous(limits = c(-0.8, 0.05))
#
# plot(d_a_2)
#
# multiplot(a_d_1, d_a_1, a_d_2, d_a_2, cols=2)

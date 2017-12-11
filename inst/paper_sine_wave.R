#!/usr/bin/env Rscript
require(ggplot2)
require(reshape2)
output <- c()
multiplier = 100
discrete = FALSE
x <- 1:(10 * multiplier)
for (j in x) {
  output <- c(output, sin((j)/multiplier) * (max(x) - (j)))
}

lin_interp = function(x, y, length.out=10 * multiplier) {
  approx(x, y, xout=seq(min(x), max(x), length.out=length.out))
}

output <- output / max(output) * 2


x <- x / max(x/10)

y.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=output))
y.predict <- predict(y.loess, data.frame(x=x))

y.predict.positive <- y.predict
y.predict.positive[y.predict < 0] = 0

y.predict.negative <- y.predict
y.predict.negative[y.predict > 0] = 0
noise <- sin(seq(0.01,10,0.01))

x.discrete = x[x == floor(x)]
y.discrete.predict <- y.predict[which(x == floor(x))]

y.discrete.predict[1] <-1

noise.discrete <- lin_interp(x.discrete, noise[which(x == floor(x))])$y
y.discrete.predict <- lin_interp(x.discrete, y.discrete.predict)$y
y.discrete.predict.positive <- y.discrete.predict
y.discrete.predict.negative <- y.discrete.predict
y.discrete.predict.negative[y.discrete.predict > 0] = 0
y.discrete.predict.positive[y.discrete.predict < 0] = 0

x.discrete <- lin_interp(x.discrete, seq(length(x.discrete)))$x


offset = .5
test_data <<- data.frame(
                         x = x,
                         out = y.predict,
                         out_positive = y.predict.positive,
                         out_negative = y.predict.negative,
                         out_upper = y.predict + (offset - noise/2.5),
                         out_lower = y.predict - (offset)
                         )

test_data_discrete <<- data.frame(
                                  x= x.discrete,
                                  out = y.discrete.predict,
                                  out_positive = y.discrete.predict.positive,
                                  out_negative = y.discrete.predict.negative,
                                  out_upper = y.discrete.predict + (offset - noise.discrete/2.5),
                                  out_lower = y.discrete.predict - (offset)
                                  )

for(i in (1:2)) {
  label = 'continous'
  if (i == 2){
    label = 'discrete'
    test_data = test_data_discrete
  }

  plotje <- ggplot(data=test_data,aes(x=x, y=out), show_guide = FALSE) +
    geom_line(aes(y=out, colour='gray')) +
    geom_hline(yintercept=0, colour="gray")+
    geom_area(data = test_data, aes(y = out_positive), fill = 'forestgreen', alpha=0.8) +
    geom_area(data = test_data, aes(y = out_negative), fill = 'darkred',alpha=0.8)+
    theme(legend.background = element_rect(colour="white"))+
    theme(panel.background = element_rect(fill = 'transparent', size=0)) +
    theme(legend.key =       element_rect(fill = "white", colour = "white"))+
    labs(x = "Horizon (Time steps)", y = "Response (Yt - d)")+
    theme(text = element_text(size=14))+
    theme(legend.position = "none") +
    scale_x_continuous(breaks = round(seq(min(test_data$x), max(test_data$x), by = 1),1))+
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.y = element_blank()) +
    geom_line(aes(y=out_upper),colour='#000000', linetype=2, alpha=0.3) +
    geom_line(aes(y=out_lower),colour='#000000', linetype=2, alpha=0.3) +
    geom_ribbon(data = subset(test_data, out_upper < 0), aes(ymin = 0, ymax = out_upper), fill = '#000000', alpha=0.3) +
    geom_ribbon(data = subset(test_data, out_lower > 0), aes(ymin = out_lower, ymax = 0), fill = '#000000', alpha=0.3)+
    theme(panel.border = element_blank()) +
    theme(axis.line = element_blank())

  plot(plotje)
  ggsave(paste("inst/output/pos_neg_area_",label,'.pdf', sep=''), plotje, width=12.00, height=6.4, units="in", scale=.7)
}

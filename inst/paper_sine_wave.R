require(ggplot2)
require(reshape2)
output <- c()
multiplier = 100
x <- 1:(10 * multiplier)
for (j in x) {
  output <- c(output, sin((j)/multiplier) * (max(x) - (j)))
}

output <- output / max(output) * 2

noise <- sin(seq(0.01,10,0.01))
x <- x / max(x/10)

y.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=output))
y.predict <- predict(y.loess, data.frame(x=x))


offset = .5
test_data <<- data.frame(
                         x = x,
                         out = y.predict,
                         out_upper = y.predict + (offset - noise/2.5),
                         out_lower = y.predict - (offset)
                         )

plotje <- ggplot(data=test_data,aes(x=x, y=out), show_guide = FALSE) +
  geom_line(aes(y=out, colour='gray')) +
  geom_hline(yintercept=0, colour="gray")+
  geom_ribbon(data = subset(test_data, out > 0), aes(ymin = 0, ymax = out), fill = 'forestgreen', alpha=0.8) +
  geom_ribbon(data = subset(test_data, out <= 0), aes(ymin = out, ymax = 0), fill = 'darkred',alpha=0.8)+
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
ggsave("inst/output/pos_neg_area.pdf", plotje, width=12.00, height=6.4, units="in", scale=.7)

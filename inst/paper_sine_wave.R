require(ggplot2)
require(reshape2)
output <- c()
multiplier = 100
x <- 1:(10 * multiplier)
for (j in x) {
  output <- c(output, sin((j)/multiplier) * (max(x) - (j)))
}

output <- output / max(output) * 2

x <- x / max(x/10)

y.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=output))
y.predict <- predict(y.loess, data.frame(x=x))


test_data <<- data.frame(
  x = x,
  out = y.predict
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
  theme(legend.position = "none") +
  scale_x_continuous(breaks = round(seq(min(test_data$x), max(test_data$x), by = 1),1))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.y = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_blank())

plot(plotje)
ggsave("inst/output/pos_neg_area.pdf", plotje, width=12.00, height=6.4, units="in", scale=.7)

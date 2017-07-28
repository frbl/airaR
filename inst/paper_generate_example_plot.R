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

x = (1:2701)
irf_data <- read.csv2('inst/irf-data.csv', sep=',', dec='.')
eating_candy  <- irf_data$eating_candy
cheerfulness  <- irf_data$cheerfulness
agitation     <- irf_data$agitation
concentration <- irf_data$concentration
rumination    <- irf_data$rumination
self_worth    <- irf_data$self_worth

names(irf_data)

leating_candy.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=eating_candy))
leating_candy.predict <- predict(leating_candy.loess, data.frame(x=x))

lcheerfulness.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=cheerfulness))
lcheerfulness.predict <- predict(lcheerfulness.loess, data.frame(x=x))

lagitation.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=agitation))
lagitation.predict <- predict(lagitation.loess, data.frame(x=x))

lconcentration.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=concentration))
lconcentration.predict <- predict(lconcentration.loess, data.frame(x=x))

lrumination.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=rumination))
lrumination.predict <- predict(lrumination.loess, data.frame(x=x))

lself_worth.loess <- loess(y ~ x, span=0.1, data.frame(x=x, y=self_worth))
lself_worth.predict <- predict(lself_worth.loess, data.frame(x=x))

real_data <- data.frame(
  x = x/300,
  eating_candy  = leating_candy.predict,
  cheerfulness  = lcheerfulness.predict,
  agitation     = lagitation.predict,
  concentration = lconcentration.predict,
  rumination    = lrumination.predict,
  self_worth    = lself_worth.predict
)


names(real_data) <- c('x', 'Eating candy',
                      'Cheerfulness',
                      'Agitation', 'Concentration', 'Rumination', 'Self-esteem')

d <- melt(real_data, id="x")

real_plot <- ggplot(aes(x=x, value, colour=variable), data=d, show_guide = TRUE) +
  geom_line()+
  scale_colour_manual(values=c("cornflowerblue", "black", 'forestgreen','orange','darkblue','coral4'))+
  annotate('text', x=0,y=1.4, label='Shock \non agitation')+
  annotate('pointrange', x=0,y=1, ymin=-.2, ymax=1.15, size=.5, colour='red')+
  annotate('segment', x=-.6,y=1.15, yend=1.15, xend=.6, size=.5, colour='red')+
  theme(legend.background = element_rect(colour="white"))+
  theme(panel.background = element_rect(fill = 'transparent', size=0)) +
  theme(legend.key =       element_rect(fill = "white", colour = "white"))+
  labs(x = "Horizon (Time steps)", y = "Response (Yt - d)") +
  theme(text = element_text(size=14))+
  scale_x_continuous(breaks = round(seq(min(test_data$x), max(test_data$x), by = 1),1))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  theme(panel.grid.major.y = element_line(colour="gray"), panel.grid.minor.y = element_blank()) +
  theme(panel.border = element_blank()) +
  theme(axis.line = element_blank())+
  theme(legend.position="bottom", legend.box = "horizontal")+
  theme(legend.title = element_blank())+
  theme(legend.text = element_text(size = 18))+
  coord_fixed(ratio = 1.5)

#pdf(file="example_response_aira.pdf", width=1790, height=960)
plot(real_plot)
ggsave("inst/output/example_response_aira.pdf", real_plot, width=12.00, height=6.4, units="in", scale=.7)
#dev.off()

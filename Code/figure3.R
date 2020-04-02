# Figure 3

# Read in monthly average correlation data, summarize, list names

data <- read.delim("./monthly_rho_values.txt", header=TRUE)

summary (data)

names(data)

# Reformat data using melt

library(reshape2)

mdata <- melt(data=data, id.vars="order")

#Plot rolloying monthly rho values

library(ggplot2)

p <- ggplot(mdata, aes(order, variable, fill=value))
p + geom_tile() + scale_fill_gradient2(low="darkorchid2", high="olivedrab4") + theme(axis.text.x = element_text(angle = 90, hjust = 1))+ geom_vline(xintercept=c(1,32,62,93,123,154,185,215,246,276,338,366,397,427,458)) + geom_vline(xintercept=307, size=2)

ggsave("rho_values.jpg")

# Read in summer weather data, summarize, list names

data <- read.delim("./summer_weather.txt", header=TRUE)

summary(data)

names(data)

# Plot max. air temp. by year

library(ggplot2)

p <- ggplot(data, aes(order, max_air_temp_F, colour=as.factor(year),group=as.factor(year)))
p + geom_smooth(span=0.1, se=FALSE, alpha=0.75) + theme_bw() + geom_vline(xintercept=c(1,32,63))+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave("temperature.jpg")

# Plot precipitation by year

p <- ggplot(data, aes(order, cumulative_precip, colour=as.factor(year),group=as.factor(year)))
p + stat_smooth(span=0.03, se=FALSE, size=3, alpha=0.75) + theme_bw() + geom_vline(xintercept=c(1,32,63))+ theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

ggsave("precipitation.jpg")
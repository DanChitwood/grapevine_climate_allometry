# Figure 1

# Read in dataset, summarize, and list names

data <- read.delim("./dataset.txt", header=TRUE)

summary(data)

names(data)

# Calculate the overall area of a leaf using the shoestring algorithm
# Convert to cm2 using the conversion factor
# Attach and detach data, because of the large number of landmarks to keep track of

attach(data)

data$all_area <- (0.5*abs(

(x4*y3 + x3*y2 + x2*y1 + x1*y6 + x6*y14 + x14*y15 + x15*y16 + x16*y17 + x17*y18 + x18*y19 + x19*y20 + x20*y21 + x21*y13 + x13*y4) - 

(y4*x3 + y3*x2 + y2*x1 + y1*x6 + y6*x14 + y14*x15 + y15*x16 + y16*x17 + y17*x18 + y18*x19 + y19*x20 + y20*x21 + y21*x13 + y13*x4) 

))/(px2_cm2)

detach(data)

# Calculate the area of the proximal veins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(data)

data$prox <- (0.5*abs(

(x2*y1 + x1*y6 + x6*y14 + x14*y5 + x5*y15 + x15*y7 + x7*y2) -

(y2*x1 + y1*x6 + y6*x14 + y14*x5 + y5*x15 + y15*x7 + y7*x2)

))/(px2_cm2)

detach(data)

# Calculate the area of the distal veins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(data)

data$dist <- (0.5*abs(

(x3*y2 + x2*y9 + x9*y17 + x17*y8 + x8*y18 + x18*y10 + x10*y3) -

(y3*x2 + y2*x9 + y9*x17 + y17*x8 + y8*x18 + y18*x10 + y10*x3)

))/(px2_cm2)

detach(data)

# Calculate the area of the midveins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(data)

data$mid <- (0.5*abs(

(x4*y3 + x3*y12 + x12*y20 + x20*y11 + x11*y21 + x21*y13 + x13*y4) -

(y4*x3 + y3*x12 + y12*x20 + y20*x11 + y11*x21 + y21*x13 + y13*x4)

))/(px2_cm2)

detach(data)

# Calculate overall vein area as the sum of the proximal, distal, and midveins

data$veins <- data$prox + data$dist + data$mid

# Calculate blade area as the overall area of the leaf minus vein area

data$blade <- data$all_area - data$veins

# Calculate vein-to-blade ratio 
# Use natural log transformation

data$veins_to_blade <- log(data$veins / data$blade)

# Create boxplots of blade, vein, and vein-to-blade ratio by year

library(ggplot2)

p <- ggplot(data, aes(as.factor(year), sqrt(blade), fill = as.factor(year)))
p + geom_boxplot() + theme_bw()

ggsave("blade_year.jpg")

p <- ggplot(data, aes(as.factor(year), sqrt(veins), fill = as.factor(year)))
p + geom_boxplot() + theme_bw()

ggsave("vein_year.jpg")

p <- ggplot(data, aes(as.factor(year), veins_to_blade, fill = as.factor(year)))
p + geom_boxplot() + theme_bw()

ggsave("vein_to_blade_year.jpg")

# Create scatter plot and regression model showing allometric relationship of
# vein-to-blade ratio to ln(all_area) by year

p <- ggplot(data, aes(log(all_area), veins_to_blade, color = as.factor(year)))
p + geom_point(size = 0.75, alpha=0.75) + theme_bw()

ggsave("allometry_points.jpg")

p <- ggplot(data, aes(log(all_area), veins_to_blade, color = as.factor(year)))
p + geom_smooth() + theme_bw()

ggsave("allometry_loess.jpg")

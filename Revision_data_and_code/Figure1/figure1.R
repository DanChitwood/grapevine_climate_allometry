########################################################
########################################################
# Stats for each species
########################################################
########################################################

data <- read.table("./dataset.txt", header=TRUE)

unique_species_vine <- droplevels(unique(data[c("species","vine")]))

sort(summary(unique_species_vine$species), decreasing=TRUE)

length(unique_species_vine$vine)

########################################################
########################################################
# Figure 1
########################################################
########################################################

library(ggplot2)

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

# ggsave("blade_year.jpg")

p <- ggplot(data, aes(as.factor(year), sqrt(veins), fill = as.factor(year)))
p + geom_boxplot() + theme_bw()

# ggsave("vein_year.jpg")

p <- ggplot(data, aes(as.factor(year), veins_to_blade, fill = as.factor(year)))
p + geom_boxplot() + theme_bw()

# ggsave("vein_to_blade_year.jpg")

# Calculate ln of leaf area and store as a variable

data$ln_area <- log(data$all_area)

#####
# Model selection for allometric relationship between vein-to-blade ratio vs leaf area
#####

# Load in dplyr

library(dplyr)

# Create variables to select polynomial models

data = mutate(data, a2=ln_area*ln_area, a3=ln_area*ln_area*ln_area, a4=ln_area*ln_area*ln_area*ln_area, a5=ln_area*ln_area*ln_area*ln_area*ln_area, a6=ln_area*ln_area*ln_area*ln_area*ln_area*ln_area, a7=ln_area*ln_area*ln_area*ln_area*ln_area*ln_area*ln_area)

# Create models

model.1 = lm(veins_to_blade ~ ln_area, data=data)
model.2 = lm(veins_to_blade ~ ln_area + a2, data=data)
model.3 = lm(veins_to_blade ~ ln_area + a2 + a3, data=data)
model.4 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4, data=data)
model.5 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5, data=data)
model.6 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5 + a6, data=data)
model.7 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5 + a6 + a7, data=data)

# Calculate AIC

AIC(model.1) # 72.25397
AIC(model.2) # -166.8877
AIC(model.3) # -178.7342
AIC(model.4) # -180.6906
AIC(model.5) # -183.3859
AIC(model.6) # -183.5118
AIC(model.7) # -183.4799

# Calculate adjusted R2

summary(model.1) # 0.6681
summary(model.2) # 0.6805
summary(model.3) # 0.6812
summary(model.4) # 0.6813
summary(model.5) # 0.6815
summary(model.6) # 0.6816
summary(model.7) # 0.6816

# Caculate p vals between models

anova(model.1, model.2) # < 2.2e-16
anova(model.2, model.3) # 0.0001994 ***
anova(model.3, model.4) # 0.04679 *
anova(model.4, model.5) # 0.03033 *
anova(model.5, model.6) # 0.1451
anova(model.6, model.7) # 0.1609

# Make a plot of vein-to-blade ratio vs ln area colored by year with 5th order polynomial model for all leaves superimposed

p <- ggplot(data, aes(ln_area, veins_to_blade, color = as.factor(year)))
p + geom_point(size = 0.75, alpha=0.75) + stat_smooth(method="lm", se=TRUE, fill=NA, formula=y ~ poly(x, 5, raw=TRUE),colour="black")+ theme_bw()

# ggsave("overall_allometry.jpg")

# Isolate leaves from each year

only2013 <- droplevels(subset(data, data$year=="2013"))
only2015 <- droplevels(subset(data, data$year=="2015"))
only2016 <- droplevels(subset(data, data$year=="2016"))
only2017 <- droplevels(subset(data, data$year=="2017"))

# Create 5th order polynomial models by year

model_2013 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5, data=only2013)
model_2015 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5, data=only2015)
model_2016 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5, data=only2016)
model_2017 = lm(veins_to_blade ~ ln_area + a2 + a3 + a4 + a5, data=only2017)

# Plot out models by year

p <- ggplot(data, aes(ln_area, veins_to_blade))

p + stat_smooth(data=only2013, method="lm", se=TRUE, fill="#F8766D", formula=y ~ poly(x, 5, raw=TRUE),colour="#F8766D") +

stat_smooth(data=only2015, method="lm", se=TRUE, fill="#7CAE00", formula=y ~ poly(x, 5, raw=TRUE),colour="#7CAE00") +

stat_smooth(data=only2016, method="lm", se=TRUE, fill="#00BFC4", formula=y ~ poly(x, 5, raw=TRUE),colour="#00BFC4") +

stat_smooth(data=only2017, method="lm", se=TRUE, fill="#C77CFF", formula=y ~ poly(x, 5, raw=TRUE),colour="#C77CFF") + theme_bw() + geom_vline(xintercept=median(data$ln_area))

# ggsave("allometry_by_year.jpg")

# Calculate intercepts for each year at the overall median leaf area value

# median intercept 2013 = -2.867668 

coef(model_2013)[1] + coef(model_2013)[2]*median(data$ln_area) + coef(model_2013)[3]*(median(data$ln_area))**2 + coef(model_2013)[4]*(median(data$ln_area))**3 + coef(model_2013)[5]*(median(data$ln_area))**4 + coef(model_2013)[6]*(median(data$ln_area))**5

# median intercept 2015 = -2.933306

coef(model_2015)[1] + coef(model_2015)[2]*median(data$ln_area) + coef(model_2015)[3]*(median(data$ln_area))**2 + coef(model_2015)[4]*(median(data$ln_area))**3 + coef(model_2015)[5]*(median(data$ln_area))**4 + coef(model_2015)[6]*(median(data$ln_area))**5

# median intercept 2016 = -2.715273

coef(model_2016)[1] + coef(model_2016)[2]*median(data$ln_area) + coef(model_2016)[3]*(median(data$ln_area))**2 + coef(model_2016)[4]*(median(data$ln_area))**3 + coef(model_2016)[5]*(median(data$ln_area))**4 + coef(model_2016)[6]*(median(data$ln_area))**5

# median intercept 2017 = -2.582221 

coef(model_2017)[1] + coef(model_2017)[2]*median(data$ln_area) + coef(model_2017)[3]*(median(data$ln_area))**2 + coef(model_2017)[4]*(median(data$ln_area))**3 + coef(model_2017)[5]*(median(data$ln_area))**4 + coef(model_2017)[6]*(median(data$ln_area))**5

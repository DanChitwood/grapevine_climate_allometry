# Figure 2

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

# Read in yearly climate averages, summarize, and list names

climate <- read.delim("./climate_year_averages.txt", header=TRUE)

summary(climate)

names(climate)

# Merge leaf and climate data (on year), summarize, and list names of new dataframe

mean_data <- merge(data, climate)

summary(mean_data)

names(mean_data)

tail(mean_data)

# Spearman's rho for different areas of the leaf and vein-to-blade ratio
# correlated with climate variables

cor.test(mean_data$all_area, mean_data$avg_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins, mean_data$avg_air_temp_F, method="spearman")[4]
cor.test(mean_data$blade, mean_data$avg_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins_to_blade, mean_data$avg_air_temp_F, method="spearman")[4]

cor.test(mean_data$all_area, mean_data$max_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins, mean_data$max_air_temp_F, method="spearman")[4]
cor.test(mean_data$blade, mean_data$max_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins_to_blade, mean_data$max_air_temp_F, method="spearman")[4]

cor.test(mean_data$all_area, mean_data$min_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins, mean_data$min_air_temp_F, method="spearman")[4]
cor.test(mean_data$blade, mean_data$min_air_temp_F, method="spearman")[4]
cor.test(mean_data$veins_to_blade, mean_data$min_air_temp_F, method="spearman")[4]

cor.test(mean_data$all_area, mean_data$total_precip_IN, method="spearman")[4]
cor.test(mean_data$veins, mean_data$total_precip_IN, method="spearman")[4]
cor.test(mean_data$blade, mean_data$total_precip_IN, method="spearman")[4]
cor.test(mean_data$veins_to_blade, mean_data$total_precip_IN, method="spearman")[4]

cor.test(mean_data$all_area, mean_data$leaf_wetness_hours, method="spearman")[4]
cor.test(mean_data$veins, mean_data$leaf_wetness_hours, method="spearman")[4]
cor.test(mean_data$blade, mean_data$leaf_wetness_hours, method="spearman")[4]
cor.test(mean_data$veins_to_blade, mean_data$leaf_wetness_hours, method="spearman")[4]

# Produce plots of vein-to-blade ratio by max. air temp. and total precipitation

library(ggplot2)

p <- ggplot(mean_data, aes(max_air_temp_F, veins_to_blade, group=max_air_temp_F, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("max_temp_vtb_ratio.jpg")

p <- ggplot(mean_data, aes(total_precip_IN, veins_to_blade, group=total_precip_IN, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("precip_vtb_ratio.jpg")

###################################
###################################

# Calculate leaf count for each vine across years

# Find only the unique vines in the dataset

uni <- unique(data$vine)

# The dataset we have been using has only leaves matched by node and vine across year
# We need access to all leaves, not just those that are matched
# Read in original datasets with all leaves

y2013 <- read.delim("./all_year_2013.txt", header=TRUE)
y2015 <- read.delim("./all_year_2015.txt", header=TRUE)
y2016 <- read.delim("./all_year_2016.txt", header=TRUE)
y2017 <- read.delim("./all_year_2017.txt", header=TRUE)

# Use only the vines in the original dataset for each year

u2013 <- y2013[y2013$vine %in% uni,]
u2015 <- y2015[y2015$vine %in% uni,]
u2016 <- y2016[y2016$vine %in% uni,]
u2017 <- y2017[y2017$vine %in% uni,]

# Create a new dataframe shoots, with all leaves for vines in each year
# Leaves in this dataset are not matched by node

shoots <- rbind(u2013, u2015, u2016, u2017)

# Calculate the overall area of a leaf using the shoestring algorithm
# Convert to cm2 using the conversion factor
# Attach and detach data, because of the large number of landmarks to keep track of

attach(shoots)

shoots$all_area <- (0.5*abs(

(x4*y3 + x3*y2 + x2*y1 + x1*y6 + x6*y14 + x14*y15 + x15*y16 + x16*y17 + x17*y18 + x18*y19 + x19*y20 + x20*y21 + x21*y13 + x13*y4) - 

(y4*x3 + y3*x2 + y2*x1 + y1*x6 + y6*x14 + y14*x15 + y15*x16 + y16*x17 + y17*x18 + y18*x19 + y19*x20 + y20*x21 + y21*x13 + y13*x4) 

))/(px2_cm2)

detach(shoots)

# Calculate the area of the proximal veins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(shoots)

shoots$prox <- (0.5*abs(

(x2*y1 + x1*y6 + x6*y14 + x14*y5 + x5*y15 + x15*y7 + x7*y2) -

(y2*x1 + y1*x6 + y6*x14 + y14*x5 + y5*x15 + y15*x7 + y7*x2)

))/(px2_cm2)

detach(shoots)

# Calculate the area of the distal veins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(shoots)

shoots$dist <- (0.5*abs(

(x3*y2 + x2*y9 + x9*y17 + x17*y8 + x8*y18 + x18*y10 + x10*y3) -

(y3*x2 + y2*x9 + y9*x17 + y17*x8 + y8*x18 + y18*x10 + y10*x3)

))/(px2_cm2)

detach(shoots)

# Calculate the area of the midveins
# Convert to cm2 using the conversion factor
# Remember to attach and detach data

attach(shoots)

shoots$mid <- (0.5*abs(

(x4*y3 + x3*y12 + x12*y20 + x20*y11 + x11*y21 + x21*y13 + x13*y4) -

(y4*x3 + y3*x12 + y12*x20 + y20*x11 + y11*x21 + y21*x13 + y13*x4)

))/(px2_cm2)

detach(shoots)

# Calculate overall vein area as the sum of the proximal, distal, and midveins

shoots$veins <- shoots$prox + shoots$dist + shoots$mid

# Calculate blade area as the overall area of the leaf minus vein area

shoots$blade <- shoots$all_area - shoots$veins

# Calculate vein-to-blade ratio 
# Use natural log transformation

shoots$veins_to_blade <- log(shoots$veins / shoots$blade)

# Aggregate by vine_year and take mean, to have a single count value per each vine per each year

counts <- aggregate(.~vine_year, shoots, mean)

# Create a dataframe of the counts for each vine for each year

counts_df <- as.data.frame(cbind(counts$vine, counts$year, counts$count))

names(counts_df) <- c("vine", "year", "count")

# Read in climate averages by year and merge with counts dataframe

climate <- read.delim("./climate_year_averages.txt", header=TRUE)

counts_climate <- merge(counts_df, climate)

# Calculate Spearman's rho for correlation of vine leaf counts with climate variables

cor.test(counts_climate$max_air_temp_F, counts_climate$count, method="spearman")
cor.test(counts_climate$max_air_temp_F, counts_climate$count, method="spearman")[3]

cor.test(counts_climate$total_precip_IN, counts_climate$count, method="spearman")
cor.test(counts_climate$total_precip_IN, counts_climate$count, method="spearman")[3]

# Create a plot of leaf counts by climate variables

p <- ggplot(counts_climate, aes(max_air_temp_F, count, group=max_air_temp_F, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("count_temp.jpg")

p <- ggplot(counts_climate, aes(total_precip_IN, count, group=total_precip_IN, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("count_rain.jpg")

###################################
###################################

# Calculate overall leaf area for each shoot

# Aggregate the shoots dataset by vine_year and take the sum

library(tidyr)

shoot_area <- aggregate(.~vine_year, shoots, sum)

# Split the vine_year column into separate columns

split_shoot_area <- shoot_area %>% separate(vine_year, c("vine", "year"), sep ="x")

# Create a dataframe with the sum of leaf areas for each shoot

shoot_area_df <- as.data.frame(cbind(split_shoot_area$vine, split_shoot_area$year, split_shoot_area$all_area))

names(shoot_area_df) <- c("vine", "year", "all_area_sum")

# Merge climate data with shoot leaf area 

climate <- read.delim("./climate_year_averages.txt", header=TRUE)

shoot_area_climate <- merge(shoot_area_df, climate)

shoot_area_climate$all_area_sum <- as.numeric(as.character(shoot_area_climate$all_area_sum))

# Calculate Spearman's rho between climate variables and overall leaf shoot area

cor.test(shoot_area_climate$max_air_temp_F, shoot_area_climate$all_area_sum, method="spearman")
cor.test(shoot_area_climate$max_air_temp_F, shoot_area_climate$all_area_sum, method="spearman")[3]

cor.test(shoot_area_climate$total_precip_IN, shoot_area_climate$all_area_sum, method="spearman")
cor.test(shoot_area_climate$total_precip_IN, shoot_area_climate$all_area_sum, method="spearman")[3]

# Create plots of 

p <- ggplot(shoot_area_climate, aes(max_air_temp_F, sqrt(all_area_sum), group=max_air_temp_F, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("shoot_area_temp.jpg")

p <- ggplot(shoot_area_climate, aes(total_precip_IN, sqrt(all_area_sum), group=max_air_temp_F, fill=as.factor(year)))
p  + geom_point(position="jitter", color="gray", size=0.5) + geom_boxplot(alpha=0.4, outlier.shape=NA) + theme_bw()

ggsave("shoot_area_rain.jpg")

###################################
###################################

# Sum the leaf areas across years

year_areas <- aggregate(.~split_shoot_area$year, split_shoot_area[c(52,56,57)], sum)

names(year_areas) <- c("year", "all_area", "veins", "blade")

year_areas$ratio <- log(year_areas$veins/year_areas$blade)

# Merge overall vineyard area by year with climate data

climate <- read.delim("./climate_year_averages.txt", header=TRUE)

year_climate <- merge(year_areas, climate)

# Plot overall vineyard leaf area by year by climate variables

p <- ggplot(year_climate, aes(max_air_temp_F, all_area, stat="bar", fill=year))
p  + geom_col() + theme_bw()

ggsave("vineyard_area_temp.jpg")

p <- ggplot(year_climate, aes(total_precip_IN, all_area, stat="bar", fill=year))
p  + geom_col() + theme_bw()

ggsave("vineyard_area_rain.jpg")





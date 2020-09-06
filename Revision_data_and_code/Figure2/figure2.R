########################################################
# Figure 2
########################################################

##########
# LEAF COUNT
##########

# Read in dataset, summarize, and list names

data <- read.delim("./dataset.txt", header=TRUE)

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

climate <- read.delim("./current_previous_climate.txt", header=TRUE)

counts_climate <- merge(counts_df, climate)

# Load in repeated measures correlation

library(rmcorr)

# totalPre

lf_count_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=count, dataset=counts_climate)

lf_count_prevTotalPre$r # 0.4391843
lf_count_prevTotalPre$p # 7.408928e-31
lf_count_prevTotalPre$CI # 0.3735184 0.5004706

lf_count_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=count, dataset=counts_climate)

lf_count_curTotalPre$r # 0.06452404
lf_count_curTotalPre$p # 0.1070596
lf_count_curTotalPre$CI # -0.01409931  0.14235440

# leafWet

lf_count_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=count, dataset=counts_climate)

lf_count_preLfWet$r # 0.5373011
lf_count_preLfWet$p # 4.880738e-48
lf_count_preLfWet$CI # 0.4789644 0.5909129

lf_count_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=count, dataset=counts_climate)

lf_count_curLfWet$r # 0.1959739
lf_count_curLfWet$p # 7.91325e-07
lf_count_curLfWet$CI # 0.1192579 0.2703637

#maxTemp

lf_count_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=count, dataset=counts_climate)

lf_count_prevMaxTemp$r # -0.1605619
lf_count_prevMaxTemp$p # 5.525656e-05
lf_count_prevMaxTemp$CI # -0.23613558 -0.08305762

lf_count_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=count, dataset=counts_climate)

lf_count_curMaxTemp$r # -0.290333
lf_count_curMaxTemp$p # 1.322814e-13
lf_count_curMaxTemp$CI # -0.3606596 -0.2167238

#avgTemp

lf_count_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=count, dataset=counts_climate)

lf_count_prevAvgTemp$r # 0.02238317
lf_count_prevAvgTemp$p # 0.5764832
lf_count_prevAvgTemp$CI # -0.05626764  0.10075790

lf_count_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=count, dataset=counts_climate)

lf_count_curAvgTemp$r # -0.4548106
lf_count_curAvgTemp$p # 3.110259e-33
lf_count_curAvgTemp$CI # -0.5149647 -0.3901990

#minTemp

lf_count_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=count, dataset=counts_climate)

lf_count_prevMinTemp$r # 0.2419379
lf_count_prevMinTemp$p # 8.921405e-10
lf_count_prevMinTemp$CI # 0.1665513 0.3145126

lf_count_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=count, dataset=counts_climate)

lf_count_curMinTemp$r # -0.03202054
lf_count_curMinTemp$p # 0.4242231
lf_count_curMinTemp$CI # -0.11029500  0.04664867

##########
# SHOOT AREA
##########

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

climate <- read.delim("./current_previous_climate.txt", header=TRUE)

shoot_area_climate <- merge(shoot_area_df, climate)

shoot_area_climate$all_area_sum <- as.numeric(as.character(shoot_area_climate$all_area_sum))

# Load in repeated measures correlation

library(rmcorr)

# totalPre

shootarea_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_prevTotalPre$r # 0.5073888
shootarea_prevTotalPre$p # 3.384209e-42
shootarea_prevTotalPre$CI # 0.4466383 0.5634823

shootarea_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_curTotalPre$r # -0.09657355
shootarea_curTotalPre$p # 0.01572861
shootarea_curTotalPre$CI # -0.17380692 -0.01815942

# leafWet

shootarea_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_preLfWet$r # 0.3684844
shootarea_preLfWet$p # 1.556463e-21
shootarea_preLfWet$CI # 0.2985748 0.4344608

shootarea_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_curLfWet$r # -0.01528844
shootarea_curLfWet$p # 0.7028559
shootarea_curLfWet$CI # -0.09372776  0.06333951

#maxTemp

shootarea_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_prevMaxTemp$r # -0.2994272
shootarea_prevMaxTemp$p # 2.060008e-14
shootarea_prevMaxTemp$CI # -0.3692931 -0.2261956

shootarea_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_curMaxTemp$r # -0.07588361
shootarea_curMaxTemp$p # 0.05795648
shootarea_curMaxTemp$CI # -0.153520383  0.002684276

#avgTemp

shootarea_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_prevAvgTemp$r # -0.1559468
shootarea_prevAvgTemp$p # 9.046413e-05
shootarea_prevAvgTemp$CI # -0.23166081 -0.07835471

shootarea_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_curAvgTemp$r # -0.3351624
shootarea_curAvgTemp$p # 7.140156e-18
shootarea_curAvgTemp$CI # -0.4031016 -0.2635492

#minTemp

shootarea_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_prevMinTemp$r # 0.03925943
shootarea_prevMinTemp$p # 0.3271361
shootarea_prevMinTemp$CI # -0.0394140  0.1174491

shootarea_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=all_area_sum, dataset=shoot_area_climate)

shootarea_curMinTemp$r # -0.22496
shootarea_curMinTemp$p # 1.302181e-08
shootarea_curMinTemp$CI # -0.2982417 -0.1490419

##########
# CORRELATION BETWEEN VEIN-TO-BLADE RATIO AND CLIMATE
##########

data <- read.table("./dataset.txt", header=TRUE)

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

# Calculate ln area

data$ln_area <- log(data$all_area)

# Read in climate data and merge with other data

PrevCurrClimate <- read.table("./current_previous_climate.txt", header=TRUE)

data_climate <- merge(data, PrevCurrClimate)

# Make year a factor

data_climate$year <- as.factor(data_climate$year)

# Load in repeated measures correlation

library(rmcorr)

# Subset by node

node1 <- droplevels(subset(data_climate, from_base==1))
node2 <- droplevels(subset(data_climate, from_base==2))
node3 <- droplevels(subset(data_climate, from_base==3))
node4 <- droplevels(subset(data_climate, from_base==4))
node5 <- droplevels(subset(data_climate, from_base==5))
node6 <- droplevels(subset(data_climate, from_base==6))
node7 <- droplevels(subset(data_climate, from_base==7))
node8 <- droplevels(subset(data_climate, from_base==8))
node9 <- droplevels(subset(data_climate, from_base==9))
node10 <- droplevels(subset(data_climate, from_base==10))

names(data_climate)

#####  #####
# NODE 1
#####   #####

# TotalPre

n1_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node1)

n1_prevTotalPre$r # -0.47454
n1_prevTotalPre$p # 2.044171e-36
n1_prevTotalPre$CI # -0.5332157 -0.4113205

n1_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node1)

n1_curTotalPre$r # -0.02589254
n1_curTotalPre$p # 0.5181977
n1_curTotalPre$CI # -0.10423243  0.05276666

# LfWet

n1_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node1)

n1_preLfWet$r # -0.1114854
n1_preLfWet$p # 0.005266616
n1_preLfWet$CI # -0.18838746 -0.03322444

n1_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node1)

n1_curLfWet$r # -0.006752766
n1_curLfWet$p # 0.8662032
n1_curLfWet$CI # -0.08525942  0.07183722

# MaxTemp

n1_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node1)

n1_prevMaxTemp$r # 0.4443722
n1_prevMaxTemp$p # 1.242368e-31
n1_prevMaxTemp$CI # 0.3790516 0.5052864

n1_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node1)

n1_curMaxTemp$r # 0.08933992
n1_curMaxTemp$p # 0.02551635
n1_curMaxTemp$CI # 0.01086428 0.16672178

# AvgTemp

n1_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node1)

n1_prevAvgTemp$r # 0.3431752
n1_prevAvgTemp$p # 1.029925e-18
n1_prevAvgTemp$CI # 0.2719544 0.4106570

n1_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node1)

n1_curAvgTemp$r # 0.4256706
n1_curAvgTemp$p # 6.729971e-29
n1_curAvgTemp$CI # 0.3591269 0.4879082

# MinTemp

n1_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node1)

n1_prevMinTemp$r # 0.1898572
n1_prevMinTemp$p # 1.748824e-06
n1_prevMinTemp$CI # 0.1129904 0.2644649

n1_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node1)

n1_curMinTemp$r # 0.315939
n1_curMinTemp$p # 5.927138e-16
n1_curMinTemp$CI # 0.2434284 0.3849376

#####  #####
# NODE 2
#####   #####

# TotalPre

n2_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node2)

n2_prevTotalPre$r # -0.5519996
n2_prevTotalPre$p # 8.441521e-48
n2_prevTotalPre$CI # -0.6061272 -0.4927828

n2_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node2)

n2_curTotalPre$r # 0.01698138
n2_curTotalPre$p # 0.6824137
n2_curTotalPre$CI # -0.06445127  0.09818938

# LfWet

n2_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node2)

n2_preLfWet$r # -0.1039958
n2_preLfWet$p # 0.01198972
n2_preLfWet$CI # -0.18378475 -0.02284546

n2_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node2)

n2_curLfWet$r # 0.0431949
n2_curLfWet$p # 0.2977803
n2_curLfWet$CI # -0.03828324  0.12410247

# MaxTemp

n2_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node2)

n2_prevMaxTemp$r # 0.5221239
n2_prevMaxTemp$p # 4.392824e-42
n2_prevMaxTemp$CI # 0.4603312 0.5788816

n2_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node2)

n2_curMaxTemp$r # 0.04956297
n2_curMaxTemp$p # 0.2321325
n2_curMaxTemp$CI # -0.0319093  0.1303810

# AvgTemp

n2_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node2)

n2_prevAvgTemp$r # 0.4145367
n2_prevAvgTemp$p # 1.290244e-25
n2_prevAvgTemp$CI # 0.3448204 0.4797047

n2_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node2)

n2_curAvgTemp$r # 0.4446196
n2_curAvgTemp$p # 1.204074e-29
n2_curAvgTemp$CI # 0.3769076 0.5076047

# MinTemp

n2_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node2)

n2_prevMinTemp$r # 0.2448225
n2_prevMinTemp$p # 2.097878e-09
n2_prevMinTemp$CI # 0.1668007 0.3197974

n2_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node2)

n2_curMinTemp$r # 0.394025
n2_curMinTemp$p # 4.303961e-23
n2_curMinTemp$CI # 0.3230351 0.4606056

#####  #####
# NODE 3
#####   #####

# TotalPre

n3_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node3)

n3_prevTotalPre$r # -0.5992862
n3_prevTotalPre$p # 5.391757e-59
n3_prevTotalPre$CI # -0.6486294 -0.5449265

n3_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node3)

n3_curTotalPre$r # 0.0005978752
n3_curTotalPre$p # 0.9884182
n3_curTotalPre$CI # -0.08012636  0.08131431

# LfWet

n3_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node3)

n3_preLfWet$r # -0.1504885
n3_preLfWet$p # 0.0002380801
n3_preLfWet$CI # -0.22843399 -0.07062609

n3_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node3)

n3_curLfWet$r # 0.01770613
n3_curLfWet$p # 0.6672451
n3_curLfWet$CI # -0.06310442  0.09828602

# MaxTemp

n3_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node3)

n3_prevMaxTemp$r # 0.5482294
n3_prevMaxTemp$p # 9.503871e-48
n3_prevMaxTemp$CI # 0.4891557 0.6022962

n3_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node3)

n3_curMaxTemp$r # 0.08527058
n3_curMaxTemp$p # 0.03806849
n3_curMaxTemp$CI # 0.004581752 0.164856226

# AvgTemp

n3_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node3)

n3_prevAvgTemp$r # 0.4221428
n3_prevAvgTemp$p # 5.531575e-27
n3_prevAvgTemp$CI # 0.3534670 0.4862925

n3_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node3)

n3_curAvgTemp$r # 0.5030182
n3_curAvgTemp$p # 2.738406e-39
n3_curAvgTemp$CI # 0.4401705 0.5609614

# MinTemp

n3_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node3)

n3_prevMinTemp$r # 0.2298737
n3_prevMinTemp$p # 1.538836e-08
n3_prevMinTemp$CI # 0.1519732 0.3049358

n3_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node3)

n3_curMinTemp$r # 0.4016764
n3_curMinTemp$p # 2.322587e-24
n3_curMinTemp$CI # 0.3317112 0.4672470


#####  #####
# NODE 4
#####   #####

# TotalPre

n4_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node4)

n4_prevTotalPre$r # -0.5877909
n4_prevTotalPre$p # 2.140792e-57
n4_prevTotalPre$CI # -0.6377464 -0.5329111

n4_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node4)

n4_curTotalPre$r # 0.006592815
n4_curTotalPre$p # 0.8715469
n4_curTotalPre$CI # -0.07335704  0.08645847

# LfWet

n4_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node4)

n4_preLfWet$r # -0.2628853
n4_preLfWet$p # 5.267292e-11
n4_preLfWet$CI # -0.3357434 -0.1869004

n4_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node4)

n4_curLfWet$r # -0.01806865
n4_curLfWet$p # 0.6576364
n4_curLfWet$CI # -0.09783858  0.06193198

# MaxTemp

n4_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node4)

n4_prevMaxTemp$r # 0.4675281
n4_prevMaxTemp$p # 3.957671e-34
n4_prevMaxTemp$CI # 0.4026606 0.5277232

n4_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node4)

n4_curMaxTemp$r # 0.1229859
n4_curMaxTemp$p # 0.002463465
n4_curMaxTemp$CI # 0.04350226 0.20092248

# AvgTemp

n4_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node4)

n4_prevAvgTemp$r # 0.3219677
n4_prevAvgTemp$p # 4.920105e-16
n4_prevAvgTemp$CI # 0.2484488 0.3917984

n4_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node4)

n4_curAvgTemp$r # 0.4919143
n4_curAvgTemp$p # 3.997461e-38
n4_curAvgTemp$CI # 0.4288614 0.5501975

# MinTemp

n4_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node4)

n4_prevMinTemp$r # 0.1142781
n4_prevMinTemp$p # 0.004924285
n4_prevMinTemp$CI # 0.03468366 0.19243202

n4_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node4)

n4_curMinTemp$r # 0.3289956
n4_curMinTemp$p # 1.032666e-16
n4_curMinTemp$CI # 0.2558097 0.3984318

#####  #####
# NODE 5
#####   #####

# TotalPre

n5_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node5)

n5_prevTotalPre$r # -0.6431539
n5_prevTotalPre$p # 9.227847e-71
n5_prevTotalPre$CI # -0.6880401 -0.5933649

n5_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node5)

n5_curTotalPre$r # 0.03827329
n5_curTotalPre$p # 0.351355
n5_curTotalPre$CI # -0.04237306  0.11842413

# LfWet

n5_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node5)

n5_preLfWet$r # -0.3517018
n5_preLfWet$p # 9.150523e-19
n5_preLfWet$CI # -0.4203153 -0.2790892

n5_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node5)

n5_curLfWet$r # -0.01476182
n5_curLfWet$p # 0.7193393
n5_curLfWet$CI # -0.09516449  0.06583219

# MaxTemp

n5_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node5)

n5_prevMaxTemp$r # 0.4666015
n5_prevMaxTemp$p # 1.683087e-33
n5_prevMaxTemp$CI # 0.4011566 0.5273070

n5_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node5)

n5_curMaxTemp$r # 0.1303418
n5_curMaxTemp$p # 0.001441072
n5_curMaxTemp$CI # 0.05035451 0.20866773

# AvgTemp

n5_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node5)

n5_prevAvgTemp$r # 0.2981716
n5_prevAvgTemp$p # 1.10669e-13
n5_prevAvgTemp$CI # 0.2230097 0.3698091

n5_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node5)

n5_curAvgTemp$r # 0.5083317
n5_curAvgTemp$p # 2.051826e-40
n5_curAvgTemp$CI # 0.4460731 0.5656943

# MinTemp

n5_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node5)

n5_prevMinTemp$r # 0.06280498
n5_prevMinTemp$p # 0.1259505
n5_prevMinTemp$CI # -0.0178008  0.1425997

n5_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node5)

n5_curMinTemp$r # 0.3307691
n5_curMinTemp$p # 1.174057e-16
n5_curMinTemp$CI # 0.2571004 0.4006156

#####  #####
# NODE 6
#####   #####

# TotalPre

n6_prevTotalPre <- rmcorr(participant=as.factor(vine), measure1=prevTotalPre, measure2=veins_to_blade, dataset=node6)

n6_prevTotalPre$r # -0.6340965
n6_prevTotalPre$p # 7.341446e-63
n6_prevTotalPre$CI # -0.6817762 -0.5810525

n6_curTotalPre <- rmcorr(participant=as.factor(vine), measure1=curTotalPre, measure2=veins_to_blade, dataset=node6)

n6_curTotalPre$r # 0.08776394
n6_curTotalPre$p # 0.04018026
n6_curTotalPre$CI # 0.003802646 0.170496504

# LfWet

n6_preLfWet <- rmcorr(participant=as.factor(vine), measure1=preLfWet, measure2=veins_to_blade, dataset=node6)

n6_preLfWet$r # -0.4382702
n6_preLfWet$p # 4.428831e-27
n6_preLfWet$CI # -0.5037177 -0.3678203

n6_curLfWet <- rmcorr(participant=as.factor(vine), measure1=curLfWet, measure2=veins_to_blade, dataset=node6)

n6_curLfWet$r # -0.002361756
n6_curLfWet$p # 0.9560504
n6_curLfWet$CI # -0.08633396  0.08164377

# MaxTemp

n6_prevMaxTemp <- rmcorr(participant=as.factor(vine), measure1=prevMaxTemp, measure2=veins_to_blade, dataset=node6)

n6_prevMaxTemp$r # 0.3945731
n6_prevMaxTemp$p # 8.040305e-22
n6_prevMaxTemp$CI # 0.3212293 0.4632117

n6_curMaxTemp <- rmcorr(participant=as.factor(vine), measure1=curMaxTemp, measure2=veins_to_blade, dataset=node6)

n6_curMaxTemp$r # 0.1171451
n6_curMaxTemp$p # 0.006088501
n6_curMaxTemp$CI # 0.03348528 0.19917481

# AvgTemp

n6_prevAvgTemp <- rmcorr(participant=as.factor(vine), measure1=prevAvgTemp, measure2=veins_to_blade, dataset=node6)

n6_prevAvgTemp$r # 0.216041
n6_prevAvgTemp$p # 3.36823e-07
n6_prevAvgTemp$CI # 0.1344921 0.2946833

n6_curAvgTemp <- rmcorr(participant=as.factor(vine), measure1=curAvgTemp, measure2=veins_to_blade, dataset=node6)

n6_curAvgTemp$r # 0.4524961
n6_curAvgTemp$p # 5.777607e-29
n6_curAvgTemp$CI # 0.3830651 0.5168429

# MinTemp

n6_prevMinTemp <- rmcorr(participant=as.factor(vine), measure1=prevMinTemp, measure2=veins_to_blade, dataset=node6)

n6_prevMinTemp$r # -0.02709524
n6_prevMinTemp$p # 0.5271446
n6_prevMinTemp$CI # -0.11083235  0.05702385

n6_curMinTemp <- rmcorr(participant=as.factor(vine), measure1=curMinTemp, measure2=veins_to_blade, dataset=node6)

n6_curMinTemp$r # 0.2857753
n6_curMinTemp$p # 9.693781e-12
n6_curMinTemp$CI # 0.2067483 0.3610975

##########
# P VALUE MULTIPLE TEST ADJUSTMENT
##########

pvals <- read.table("./rmcorr_values.txt", header=TRUE)

corr_pvals <- p.adjust(pvals$pval, method="bonferroni")

pvals_corrected <- cbind(pvals, corr_pvals)

# write.table(pvals_corrected, "rmcorr_corrected.txt")

##########
# VISUALIZE CORRELATION AS A HEATMAP
##########

pval_data <- read.table("./rmcorr_corrected.txt", header=TRUE)

head(pval_data)

library(ggplot2)

p <- ggplot(pval_data, aes(x=trait, y=climate, fill=rmcorr_nas))
p + geom_tile() + geom_text(aes(x=trait, y=climate, label=round(rmcorr, digits=2))) + scale_fill_gradient2(low='orange', mid='snow', high='blue', na.value="gray95") + scale_y_discrete(limits = rev(levels(pval_data$climate))) + coord_fixed()

# ggsave("correlation_heatmap.jpg")

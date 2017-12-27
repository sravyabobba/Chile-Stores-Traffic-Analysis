#Reading the CSV file
chile <- read.csv("ChileStoreTrafficAnalysis.csv")
str(chile)
summary(chile)

#removing data records where traffic volume is zero
chile <- chile[!(chile$Traffic == 0), ]
summary(chile)

#Installing libraries
#install.packages("dplyr")
library(dplyr)

#install.packages("tidyr")
library(tidyr)

#install.packages("ggplot2")
library(ggplot2)

# Answer 1(a)
# Taking traffic per week (tpw) over weeks indexed every year (like week 1 for 2014 is 1, for 2015 is 53)
tpw <- chile %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
       summarise(Traffic = mean(Traffic))

# Making combinations of 6 stores to decide on different traffic patterns
choose1 <- c(1, 2, 3, 5, 6, 7)
choose2 <- c(8, 9, 10, 16, 17, 18)
choose3 <- c(19, 20, 21, 22, 23, 24)
choose4 <- c(25, 26, 27, 28, 29, 30)

tpw.filtered <- tpw %>% filter(StoreCode %in% choose1)
ggplot(tpw.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
                                               scale_x_continuous(breaks = seq(0, 3*52, by = 26))
# Storecode 1, 3 and 6 shows different traffic pattern

tpw.filtered <- tpw %>% filter(StoreCode %in% choose2)
ggplot(tpw.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
                                               scale_x_continuous(breaks = seq(0, 3*52, by = 26))
# Storecode 17 shows different traffic pattern addition to old ones

tpw.filtered <- tpw %>% filter(StoreCode %in% choose3)
ggplot(tpw.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
                                               scale_x_continuous(breaks = seq(0, 3*52, by = 26))
# Storecode 19 shows different traffic pattern addition to old ones

tpw.filtered <- tpw %>% filter(StoreCode %in% choose4)
ggplot(tpw.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
                                               scale_x_continuous(breaks = seq(0, 3*52, by = 26))
# Storecode 29 shows different traffic pattern addition to old ones

# Choosing different Traffic patterns
choose <- c(1, 3, 6, 17, 19, 29)

tpw.filtered <- tpw %>% filter(StoreCode %in% choose)

# Will give final required plot
ggplot(tpw.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
                                               scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Answer 1(b)

# Changing day names from spanish to english
levels(chile$DayName) <- c("Sunday", "Thursday", "Monday", "Tuesday", "Wednesday", "Saturday", "Friday")
chile$DayName = factor(chile$DayName,levels(chile$DayName)[c(1, 3, 4, 5, 2, 7, 6)])

# Taking average traffic value over days for different stores
chile1 <- chile %>% group_by(StoreCode, DayName) %>% summarise(Traffic = mean(Traffic))

chile1 <- chile1 %>% spread(DayName, Traffic)
summary(chile1)
View(chile1)

# Answer 1(c)
#Normalizing the data
#install.packages("DMwR")
library(DMwR)
chileNorm=scale(chile1)
summary(chileNorm)

#Compute distances
distances = dist(chileNorm, method = "euclidean")

# Hierarchical clustering using Ward distance #Ward.D2 not just Ward 
clusterchile = hclust(distances, method = "ward.D2")

plot(clusterchile)
rect.hclust(clusterchile, h=5) # Giving big clusters like having 13 stores 

plot(clusterchile)
rect.hclust(clusterchile, h=4) # Gives nice clusters with well spreaded number of stores
# Choosing k = 6 (h=4)

clusterGroups = cutree(clusterchile, k=6)
table(clusterGroups)
cluster1 = subset(chile1, clusterGroups == 1) # Store 1, 9, 10
cluster2 = subset(chile1, clusterGroups == 2) # Store 2, 19, 21, 25
cluster3 = subset(chile1, clusterGroups == 3) # Store 3, 5, 6, 7, 8
cluster4 = subset(chile1, clusterGroups == 4) # Store 16
cluster5 = subset(chile1, clusterGroups == 5) # Store 17, 18, 20, 22, 23, 24, 26, 27, 28
cluster6 = subset(chile1, clusterGroups == 6) # Store 29, 30

# Answer 1(d)
#K means clustering
set.seed(5000)
chileKMC = kmeans(chileNorm, centers = 6)
table(chileKMC$cluster)
chileKMC$size
chileKMC$centers

#install.packages("cluster")
library(cluster)
clusplot(chile1, chileKMC$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(chileNorm[,1],chileKMC$cluster)

clust1 = unscale(subset(chileNorm, chileKMC$cluster == 1),chileNorm) # Store code 2, 19, 21, 25
clust2 = unscale(subset(chileNorm, chileKMC$cluster == 2),chileNorm) # Store code 29,30
clust3 = unscale(subset(chileNorm, chileKMC$cluster == 3),chileNorm) # Store code 3, 5, 6, 7, 8
clust4 = unscale(subset(chileNorm, chileKMC$cluster == 4),chileNorm) 
         # Store code 17, 20, 23, 24, 26, 27, 28
clust5 = unscale(subset(chileNorm, chileKMC$cluster == 5),chileNorm) # Store code 1, 9, 10, 16
clust6 = unscale(subset(chileNorm, chileKMC$cluster == 6),chileNorm) # Store code 18, 22

# Answer 1(e)
# Store 30 has very little data (Only 44 records, about 0.5% of data)
chile2 <- chile[!(chile$StoreCode == 30), ]

# We can delete some bad data where sensor detected traffic but we really didn't have any customers,
# sales people, and ticket information. There are 246 records.
chile2 <- chile2[!(chile2$Customers == 0 & chile2$Sales==0 & chile2$Salespeople==0), ]

# Lets delete records where Sales people working together is negative (Total of 4 records only)
chile2 <- chile2[!(chile2$Salespeople.Working.Same.Time < 0), ]

# Additionally, we see some records where Sales people working together are more than Sales people.
# Thus, deleting these 1178 records.
chile2 <- chile2[!(chile2$Salespeople.Working.Same.Time > chile2$Salespeople), ]

summary(chile2)

# We see so many records where we have customers and sales but no salespeople, so we can assign value
# 10 (median) to salespeople. 
chile2$Salespeople <- ifelse(chile2$Salespeople == 0, 10, chile2$Salespeople)

# Same goes for salespeople working at same time and we are assigning value # 9 (considering mean and 
# median)
chile2$Salespeople.Working.Same.Time <- ifelse(chile2$Salespeople.Working.Same.Time == 0, 9, 
                                               chile2$Salespeople.Working.Same.Time)

summary(chile2)

# Creating file like chile1
chile3 <- chile2 %>% group_by(StoreCode, DayName) %>% summarise(Traffic = mean(Traffic))

chile3 <- chile3 %>% spread(DayName, Traffic)
summary(chile3)
View(chile3)

AvgCustomers <- chile2 %>% group_by(StoreCode) %>% summarise(AvgCustomers = 
                                                   mean(Customers)) %>%.$AvgCustomers

AvgSales <- chile2 %>% group_by(StoreCode) %>% summarise(AvgSales = mean(Sales)) %>%.$AvgSales

AvgSalesppl <- chile2 %>% group_by(StoreCode) %>% summarise(AvgSalesppl = 
                                                   mean(Salespeople)) %>%.$AvgSalesppl

AvgSPWST <- chile2 %>% group_by(StoreCode) %>% summarise(AvgSPWST = 
                                                   mean(Salespeople.Working.Same.Time)) %>%.$AvgSPWST

AvgPopulation <- chile2 %>% group_by(StoreCode) %>% summarise(AvgPopulation = 
                                                   mean(Population)) %>%.$AvgPopulation

AvgTraffic <- chile2 %>% group_by(StoreCode) %>% summarise(AvgTraffic = mean(Traffic)) %>%.$AvgTraffic

RatioTraffic <- chile2 %>% group_by(StoreCode) %>% summarise(RatioTraffic = 
                max(Traffic)/min(Traffic)) %>%.$RatioTraffic

# Updating chile3 to add other parameters 
# (given + found externally like population, AvgTraffic, RationTraffic)

chile3$AvgCustomers <- AvgCustomers
chile3$AvgSales <- AvgSales
chile3$AvgSalesppl <- AvgSalesppl
chile3$AvgSPWST <- AvgSPWST # Average Sales People Working at Same Time
chile3$AvgPopulation <- AvgPopulation 
chile3$AvgTraffic <- AvgTraffic
chile3$RatioTraffic <- RatioTraffic

summary(chile3)
View(chile3)

# Answer 1(f)
#Normalizing the data
#install.packages("DMwR")
library(DMwR)
chileNorm1=scale(chile3)
summary(chileNorm1)

#Compute distances
distances1 = dist(chileNorm1, method = "euclidean")

# Hierarchical clustering using Ward distance #Ward.D2 not just Ward 
clusterchile1 = hclust(distances1, method = "ward.D2")

plot(clusterchile1)
rect.hclust(clusterchile1, h=6)

plot(clusterchile1)
rect.hclust(clusterchile1, h=5)

# Considering h=6 as h=5 just gives one extra cluster (thus, k=6)
clusterGroups1 = cutree(clusterchile1, k=6)
table(clusterGroups1)
cluster11 = subset(chile3, clusterGroups1 == 1) # Store 1, 8, 9, 10, 16
cluster21 = subset(chile3, clusterGroups1 == 2) # Store 2, 29
cluster31 = subset(chile3, clusterGroups1 == 3) # Store 3, 5, 6, 7, 18, 22
cluster41 = subset(chile3, clusterGroups1 == 4) # Store 17
cluster51 = subset(chile3, clusterGroups1 == 5) # Store 19, 20, 23, 24, 25, 26, 27, 28
cluster61 = subset(chile3, clusterGroups1 == 6) # Store 21

# Answer 1(g)
#K means clustering
set.seed(5000)
chileKMC1 = kmeans(chileNorm1, centers = 6)
table(chileKMC1$cluster)
chileKMC1$size
chileKMC1$centers

#install.packages("cluster")
library(cluster)
clusplot(chile3, chileKMC1$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(chileNorm1[,1],chileKMC1$cluster)

clust11 = unscale(subset(chileNorm1, chileKMC1$cluster == 1),chileNorm1) # Store code 17 
clust21 = unscale(subset(chileNorm1, chileKMC1$cluster == 2),chileNorm1) # Store code 16
clust31 = unscale(subset(chileNorm1, chileKMC1$cluster == 3),chileNorm1) # Store code 3, 5, 6, 7, 18, 22
clust41 = unscale(subset(chileNorm1, chileKMC1$cluster == 4),chileNorm1) 
# Store code 2, 19, 20, 23, 24, 25, 26, 27, 28, 29
clust51 = unscale(subset(chileNorm1, chileKMC1$cluster == 5),chileNorm1) # Store code 1, 8, 9, 10
clust61 = unscale(subset(chileNorm1, chileKMC1$cluster == 6),chileNorm1) # Store code 21

# Answer 1(h)
# Taking traffic per day (tpd) over days indexed every year (like day 1 for 2014 is 1, for 2015 is 366)
tpd <- chile %>% mutate(Index = (Year-2014)*365+(Month-12)*30+Day) %>% group_by(StoreCode, Index) %>% 
  summarise(Traffic = mean(Traffic))

# Making combinations of 6 stores to decide on different traffic patterns
choose1 <- c(17) #Cluster1
choose2 <- c(16) #Cluster2

tpd.filtered <- tpd %>% filter(StoreCode %in% choose1)
ggplot(tpd.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 3*365, by = 180))

tpd.filtered <- tpd %>% filter(StoreCode %in% choose2)
ggplot(tpd.filtered, aes(Index, Traffic, col = as.factor(StoreCode))) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 3*365, by = 180))


# Answer 2(a)
# We will be using K means clusters to answer further questions
# Cluster #1 (# Store code 17), Plotting weekly mean of sales over 3 years
C1 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
      summarise(Sales = mean(Sales))
C1.filtered <- C1 %>% filter(StoreCode %in% 17)
ggplot(C1.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
       scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Cluster #2 (# Store code 16), Plotting weekly mean of sales over 3 years
C2 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
      summarise(Sales = mean(Sales))
C2.filtered <- C2 %>% filter(StoreCode %in% 16)
ggplot(C2.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
       scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Cluster #3 (# Store code 3, 5, 6, 7, 18, 22)
# Finding best store in Cluster 3
C3 <- chile3 %>% filter(StoreCode %in% c(3, 5, 6, 7, 18, 22))
C3 <- subset(C3, C3$AvgCustomers/C3$AvgSalesppl == max(C3$AvgCustomers/C3$AvgSalesppl))
print(C3$StoreCode)

# Plotting weekly mean of sales over 3 years for best store in Cluster 3
C31 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
  summarise(Sales = mean(Sales))
C31.filtered <- C31 %>% filter(StoreCode %in% 7)
ggplot(C31.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Cluster #4 (# Store code 2, 19, 20, 23, 24, 25, 26, 27, 28, 29)
# Finding best store in Cluster 4
C4 <- chile3 %>% filter(StoreCode %in% c(2, 19, 20, 23, 24, 25, 26, 27, 28, 29))
C4 <- subset(C4, C4$AvgCustomers/C4$AvgSalesppl == max(C4$AvgCustomers/C4$AvgSalesppl))
print(C4$StoreCode)

# Plotting weekly mean of sales over 3 years for best store in Cluster 4
C41 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
       summarise(Sales = mean(Sales))
C41.filtered <- C41 %>% filter(StoreCode %in% 23)
ggplot(C41.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
       scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Cluster #5 (# Store code 1, 8, 9, 10)
# Finding best store in Cluster 5
C5 <- chile3 %>% filter(StoreCode %in% c(1, 8, 9, 10))
C5 <- subset(C5, C5$AvgCustomers/C5$AvgSalesppl == max(C5$AvgCustomers/C5$AvgSalesppl))
print(C5$StoreCode)

# Plotting weekly mean of sales over 3 years for best store in Cluster 5
C51 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
  summarise(Sales = mean(Sales))
C51.filtered <- C51 %>% filter(StoreCode %in% 9)
ggplot(C51.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 3*52, by = 26))

# Cluster #6 (# Store code 21)
# Plotting weekly mean of sales over 3 years store 21
C6 <- chile2 %>% mutate(Index = (Year-2014)*52+Week) %>% group_by(StoreCode, Index) %>% 
  summarise(Sales = mean(Sales))
C6.filtered <- C6 %>% filter(StoreCode %in% 21)
ggplot(C6.filtered, aes(Index, Sales, col = as.factor(StoreCode))) + geom_line() + 
  scale_x_continuous(breaks = seq(0, 3*52, by = 26))


# Answer 2(b)
# Choosing store code and sales people only
chile4 <- subset(chile3, select = c(1, 11))
chileNorm2=scale(chile4)
summary(chileNorm2)

#K means clustering
set.seed(5000)
#Trying different values of centers
chileKMC2 = kmeans(chileNorm2, centers = 4)
table(chileKMC2$cluster)

chileKMC2 = kmeans(chileNorm2, centers = 5)
table(chileKMC2$cluster)

chileKMC2 = kmeans(chileNorm2, centers = 6) #This gives good composition of cluster (numbers)
table(chileKMC2$cluster)
chileKMC2$size
chileKMC2$centers

#install.packages("cluster")
library(cluster)
clusplot(chile4, chileKMC2$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(chileNorm2[,1],chileKMC2$cluster)

clust21 = unscale(subset(chileNorm2, chileKMC2$cluster == 1),chileNorm2) # Store code 2
clust22 = unscale(subset(chileNorm2, chileKMC2$cluster == 2),chileNorm2) # Store code 1, 8, 10, 21
clust23 = unscale(subset(chileNorm2, chileKMC2$cluster == 3),chileNorm2) # Store code 25, 26, 27, 28, 29
clust24 = unscale(subset(chileNorm2, chileKMC2$cluster == 4),chileNorm2) # Store code 17, 23, 24
clust25 = unscale(subset(chileNorm2, chileKMC2$cluster == 5),chileNorm2) # Store code 16, 18, 19, 20, 22
clust26 = unscale(subset(chileNorm2, chileKMC2$cluster == 6),chileNorm2) # Store code 3, 5, 6, 7, 9

# Determination of best and worst store based on Average sales in Cluster #2
C22 <- chile3 %>% filter(StoreCode %in% c(1, 8, 10, 21))
C22B <- subset(C22, C22$AvgSales == max(C22$AvgSales))
print(C22B$StoreCode) #Store 1
C22W <- subset(C22, C22$AvgSales == min(C22$AvgSales))
print(C22W$StoreCode) #Store 21

# Determination of best and worst store based on Average sales in Cluster #3
C23 <- chile3 %>% filter(StoreCode %in% c(25, 26, 27, 28, 29))
C23B <- subset(C23, C23$AvgSales == max(C23$AvgSales))
print(C23B$StoreCode) #Store 26
C23W <- subset(C23, C23$AvgSales == min(C23$AvgSales))
print(C23W$StoreCode) #Store 29

# Determination of best and worst store based on Average sales in Cluster #4
C24 <- chile3 %>% filter(StoreCode %in% c(17, 23, 24))
C24B <- subset(C24, C24$AvgSales == max(C24$AvgSales))
print(C24B$StoreCode) #Store 23
C24W <- subset(C24, C24$AvgSales == min(C24$AvgSales))
print(C24W$StoreCode) #Store 17

# Determination of best and worst store based on Average sales in Cluster #5
C25 <- chile3 %>% filter(StoreCode %in% c(16, 18, 19, 20, 22))
C25B <- subset(C25, C25$AvgSales == max(C25$AvgSales))
print(C25B$StoreCode) #Store 19
C25W <- subset(C25, C25$AvgSales == min(C25$AvgSales))
print(C25W$StoreCode) #Store 20

# Determination of best and worst store based on Average sales in Cluster #6
C26 <- chile3 %>% filter(StoreCode %in% c(3, 5, 6, 7, 9))
C26B <- subset(C26, C26$AvgSales == max(C26$AvgSales))
print(C26B$StoreCode) #Store 9
C26W <- subset(C26, C26$AvgSales == min(C26$AvgSales))
print(C26W$StoreCode) #Store 3

# Answer 2(c)
# Choosing store code and sales people working concurrently
chile5 <- subset(chile3, select = c(1, 12))
chileNorm3=scale(chile5)
summary(chileNorm3)

#K means clustering
set.seed(5000)
# Trying different values of centers
chileKMC3 = kmeans(chileNorm3, centers = 4)
table(chileKMC3$cluster)

chileKMC3 = kmeans(chileNorm3, centers = 5)
table(chileKMC3$cluster)

chileKMC3 = kmeans(chileNorm3, centers = 6) #This gives good composition of cluster (numbers)
table(chileKMC3$cluster)
chileKMC3$size
chileKMC3$centers

#install.packages("cluster")
library(cluster)
clusplot(chile5, chileKMC3$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
table(chileNorm3[,1],chileKMC3$cluster)

clust31 = unscale(subset(chileNorm3, chileKMC3$cluster == 1),chileNorm2) # Store code 2, 3
clust32 = unscale(subset(chileNorm3, chileKMC3$cluster == 2),chileNorm2) # Store code 21
clust33 = unscale(subset(chileNorm3, chileKMC3$cluster == 3),chileNorm2) # Store code 25, 26, 27, 28, 29
clust34 = unscale(subset(chileNorm3, chileKMC3$cluster == 4),chileNorm2) # Store code 17, 23, 24
clust35 = unscale(subset(chileNorm3, chileKMC3$cluster == 5),chileNorm2) # Store code 16, 18, 19, 20, 22
clust36 = unscale(subset(chileNorm3, chileKMC3$cluster == 6),chileNorm2) # Store code 1, 5, 6, 7, 8, 9, 10

# Determination of best and worst store based on Average sales in Cluster #1
CL31 <- chile3 %>% filter(StoreCode %in% c(2, 3))
CL31B <- subset(CL31, CL31$AvgSales == max(CL31$AvgSales))
print(CL31B$StoreCode) #Store 3
CL31W <- subset(CL31, CL31$AvgSales == min(CL31$AvgSales))
print(CL31W$StoreCode) #Store 2

# Determination of best and worst store based on Average sales in Cluster #3
C33 <- chile3 %>% filter(StoreCode %in% c(25, 26, 27, 28, 29))
C33B <- subset(C33, C33$AvgSales == max(C33$AvgSales))
print(C33B$StoreCode) #Store 26
C33W <- subset(C33, C33$AvgSales == min(C33$AvgSales))
print(C33W$StoreCode) #Store 29

# Determination of best and worst store based on Average sales in Cluster #4
C34 <- chile3 %>% filter(StoreCode %in% c(17, 23, 24))
C34B <- subset(C34, C34$AvgSales == max(C34$AvgSales))
print(C34B$StoreCode) #Store 23
C34W <- subset(C34, C34$AvgSales == min(C34$AvgSales))
print(C34W$StoreCode) #Store 17

# Determination of best and worst store based on Average sales in Cluster #5
C35 <- chile3 %>% filter(StoreCode %in% c(16, 18, 19, 20, 22))
C35B <- subset(C35, C35$AvgSales == max(C35$AvgSales))
print(C35B$StoreCode) #Store 19
C35W <- subset(C35, C35$AvgSales == min(C35$AvgSales))
print(C35W$StoreCode) #Store 20

# Determination of best and worst store based on Average sales in Cluster #6
C36 <- chile3 %>% filter(StoreCode %in% c(1, 5, 6, 7, 8, 9, 10))
C36B <- subset(C36, C36$AvgSales == max(C36$AvgSales))
print(C36B$StoreCode) #Store 1
C36W <- subset(C36, C36$AvgSales == min(C36$AvgSales))
print(C36W$StoreCode) #Store 7

# Answer 2(d)
# Determination of overall best and overall worst store based on Average sales
CB <- chile3 %>% filter(StoreCode %in% c(1, 3, 9, 19, 23, 26))
Best <- subset(CB, CB$AvgSales == max(CB$AvgSales))
print(Best$StoreCode) #Store 1

CW <- chile3 %>% filter(StoreCode %in% c(2, 7, 17, 20, 21, 29))
Worst <- subset(CW, CW$AvgSales == min(CW$AvgSales))
print(Worst$StoreCode) #Store 29

## R script for analyzing surface sediment dinoflagellate cyst distribution in the Southern Hemisphere
## Supplement to: An expanded database of Southern Hemisphere surface sediment dinoflagellate cysts and their oceanographic affinities
## Thole et al., 2022
# 1. LOAD PACKAGES ----

library(readxl)
library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(gtools)
library(vegan)
library(tibble)
library(ggplot2)
library(plotly)
library(devtools)
library(egg)
library(cowplot)
library(ggrepel)
library(ca)
library(dbscan)
library(factoextra)
library(maps)
library(viridis)
library(RColorBrewer)
library(analogue)
library(pls)
library(ggmap)
library(rnaturalearthdata)

#update.packages(checkBuilt = TRUE)


# 2. LOAD DATA ----

# 2.1 MARRET DATA SET Southern Hemisphere  ----
# Candel data is excluded in the excel sheet
marret19 <- read_excel("SurfaceDinos.xlsx", sheet = "Marret2019", col_names = TRUE,
                       col_types = c("text", "numeric","numeric", "text",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                     "numeric","numeric","numeric","numeric"))

as.data.frame(marret19)
str(marret19)

# We will just consider the Southern Hemisphere --> Cutoff at Equator
marret19_sh <- filter(marret19, Latitude < 0)

# Treat NAs as zeros
marret19_sh[is.na(marret19_sh)] = 0

# Exclude columns that do not have an entry
marret19_sh <- marret19_sh[, colSums(marret19_sh != 0) > 0]
as.data.frame(marret19_sh)

# Have a look at the data frame
head(marret19_sh)
str(marret19_sh)
colnames(marret19_sh)

# Find out number of samples
sample_marret19_sh <-c(1:nrow(marret19_sh))
str(sample_marret19_sh)

# Give each location a sample number from 1-595
marret19_sh <- mutate(marret19_sh, sample_marret19_sh)
str(marret19_sh)
names(marret19_sh)[68] <- "sample"

# 2.2 NEWLY COUNTED DATA ----
#ALREADY DELETED IN EXCEL FILES ARE SAMPLES WITH LESS THAN 25 DINOS, extra data sheet for them (see 2.3)

thole21_start <- read_excel("SurfaceDinos.xlsx", sheet = "Thole2021", col_names = TRUE,
                            col_types = c("text", "numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                          "numeric","numeric","numeric","numeric","numeric","numeric"))
as.data.frame(thole21_start)
str(thole21_start)

thole21 <-thole21_start

# Make NAs to zero
thole21[is.na(thole21)] = 0

# Delete columns with no entry
thole21 <- thole21[, colSums(thole21 != 0) > 0]

# Check out data frame 
as.data.frame(thole21)
head(thole21)
str(thole21)
colnames(thole21)

# Find out sample number 
sample_thole21 <-c(1:nrow(thole21))
str(sample_thole21)
thole21 <- mutate(thole21, sample_thole21)
str(thole21)
names(thole21)[38] <- "sample"


# 2.3 NEWLY COUNTED BUT DELETED ----
# Just to show where sample with too low dino counts are 

thole21_deleted_start <- read_excel("SurfaceDinos.xlsx", sheet = "Thole2021_deleted", col_names = TRUE)
as.data.frame(thole21_deleted_start)
str(thole21_deleted_start)



# 3. CALCULATE SUMS AND PERCENTAGES ----

# COMMENT: IN MARRET2019: Pietkowski reports concentrations, so total dino sum is wrong.
# COMMENT: This is no problem for abundace calcs, but gives weird total dino sums

# have a look at data frame
str(marret19_sh)

# select only dino columns and sample, 
# For this we exclude Polarella glacialis in the Thole21_data frame 

marret19_sh_dinosonly <- select(marret19_sh,- Station, - Longitude, - Latitude, - Contributors, - Dino_tot)
str(marret19_sh_dinosonly)

# 
thole21_dinosonly <- select(thole21, -Station, -Longitude, - Latitude, - Depth, - Weight, - Lycopodium, - Lyc_tab, -"Non-quat dinos", -Pgla)
str(thole21_dinosonly)

# These are function to calculate sums and fractions

# First sums
dino_sums <- function(d) {
  require(tidyverse)
  d_gather <- d %>%
    gather(key = "dino_species", value = "dino_count", -sample)
  d_sum <- d_gather %>%
    group_by(sample) %>%
    summarise(dino_sum = sum(dino_count))
  d %>% left_join(d_sum, by = "sample")}

# Then fractions 
dino_probs <- function(d) {
  require(tidyverse)
  d_gather <- d %>% 
    gather(key = "dino_species", value = "dino_count", -sample)
  d_sum <- d_gather %>% 
    group_by(sample) %>% 
    summarise(sample_sum = sum(dino_count))
  d_prop <- d_gather %>% 
    left_join(d_sum, by = "sample") %>% 
    mutate(prop = dino_count / sample_sum) %>% 
    select(-dino_count, -sample_sum) %>% 
    mutate(dino_species = paste0(dino_species,"_prop")) %>% 
    spread(key = dino_species, value = prop)
  d %>% left_join(d_prop, by = "sample")}

dino_sums_marret19_sh <- dino_sums(marret19_sh_dinosonly)
dino_sums_thole21 <- dino_sums(thole21_dinosonly)

str(thole21_dinosonly)

# Check out the dino sum
dino_sums_marret19_sh$dino_sum # here you see that sometimes the dinos are already reported in percentages (clean 100 sum), sometimes in counts and sometimes in concetrations
dino_sums_thole21$dino_sum # here dinos are presented in counts


dino_probs_marret19_sh <- dino_probs(marret19_sh_dinosonly) 
dino_probs_thole21 <- dino_probs(thole21_dinosonly)

str(dino_probs_marret19_sh)
str(dino_probs_thole21)

# 4. COMBINE DATA ----

# 4.1 MARRET19_SH ----
# have a look at the different data frames again
str(marret19_sh) # Here we need Station, Long, Lat
marret19_sh_loc <- select(marret19_sh, Station, Longitude, Latitude)
str(marret19_sh_loc)

str(dino_sums_marret19_sh) # only need sample and dino_sum
marret19_sh_sum <- select(dino_sums_marret19_sh, dino_sum, sample)
str(marret19_sh_sum)

str(dino_probs_marret19_sh) # only need _prop
marret19_sh_dinoprobs <- select(dino_probs_marret19_sh, contains("prop"))
str(marret19_sh_dinoprobs)

# Combine into one data frame
marret19_sh_all <- cbind(marret19_sh_loc,marret19_sh_sum, marret19_sh_dinoprobs)
str(marret19_sh_all)

# 4.2 THOLE21 ----

# have a look at the different data frames again
str(thole21) # we need station, Long, Lat
thole21_loc <- select(thole21, Station, Longitude, Latitude)

str(dino_sums_thole21) # we need sample and dino_sum
thole21_sum <- select(dino_sums_thole21, sample, dino_sum)

str(dino_probs_thole21) # we need _prop
thole21_dinoprobs <- select(dino_probs_thole21, contains ("prop"))

# Combine into one data frame
thole21_all <- cbind(thole21_loc, thole21_sum, thole21_dinoprobs)
str(thole21_all)


# 5. MARRET 19: COMBINE DINO SPECIES COUNTINGS ----

# Following Prebble et al., 2013 & Marret et al., 2019, we combine: 
# a) Protoperidiniacean cysts: Lejeunecysta spp., cysts of Protoperidinium stellatum, Quinquecuspis concreta, and Votadinium calvum: Vcal + Qcon + Lspp
# b) Spiniferites bulloides was combined with Spiniferites ramosus: Sbul + Sram
# c) Spiniferites belerius was combined with Spiniferites membranaceus: Sbel + Smem
# d) Cysts of Protoperidinium nudum were assigned to Selenopemphix quanta: Pnud -> Squa
# e) All Echinidinium were grouped: Eacu + Edel + Egra + Espp + Etra

# This is not necessary for the Thole21_data set, as it does not have these species 

str(marret19_sh_all)

marret19_sh_all_speciescomb <- mutate(marret19_sh_all, 
                                      Pspp_prop = Vcal_prop + Qcon_prop + Lspp_prop, 
                                      Sram_bul_prop = Sbul_prop + Sram_prop,
                                      Smem_bel_prop = Sbel_prop + Smem_prop, 
                                      Squa_comb_prop = Pnud_prop + Squa_prop, 
                                      Espp_comb_prop = Eacu_prop + Edel_prop + Egra_prop + Espp_prop + Etra_prop) %>%
                               select(-Vcal_prop, -Qcon_prop, -Lspp_prop, -Sbul_prop, -Sram_prop, -Sbel_prop, -Smem_prop,
                                      -Pnud_prop, -Squa_prop, -Eacu_prop, -Edel_prop, -Egra_prop, -Espp_prop, -Etra_prop) 
str(marret19_sh_all_speciescomb)


# 6. THOLE21: CONCENTRATIONS ----

# Calculate dino concentrations 
# Go nack to data frame with raw countings
str(thole21)
str(thole21_sum)

# add dino sum to raw countings
thole21_conc <- cbind(thole21, thole21_sum$dino_sum)
str(thole21_conc)
names(thole21_conc)[38] <- "dino_sum"

# calculate total dino conentrations (and Polarella concentrations) based on Lycopodium counts 
thole21_conc <- mutate(thole21_conc,
                       dino_conc = ((Lyc_tab/Lycopodium) * dino_sum)/Weight,
                       Pgla_conc = ((Lyc_tab/Lycopodium)* Pgla)/Weight)
thole21_conc <- as.data.frame(thole21_conc)
str(thole21_conc)

# 7. THOLE21: RAREFACTION ----
str(thole21)
# choose data frame with only dinos 
thole21_rf <- thole21_dinosonly
thole21_rf <- as.data.frame(thole21_rf)
str(thole21_rf)

row.names(thole21_rf) <- thole21_rf$sample
row.names(thole21_rf)
thole21_rf$sample <- NULL
thole21_rf$sample

# it is 60 samples so we need to pre-define 15 (color) x 4 (style) = 60 different plot lines 
col <- c("black", "darkred", "red", "dark blue", "dark green", "forestgreen", "orange", "blue", "yellow", "hotpink", "brown", "grey", "orange4", "purple", "green2")
lty <- c("solid", "dashed", "longdash", "dotdash")
pars <- expand.grid(col = col, lty = lty, stringsAsFactors = FALSE)
head(pars)
pars

rf_thole21 <- with(pars[1:60, ], 
                    rarecurve(thole21_rf, sample = sum(thole21_rf), col = col, lty = lty, label = FALSE))
#ggsave( filename = "Rarefaction_Thole21.pdf", width = 20, height = 15, unit = "cm")



# 8. COMBINE DATA SETS ----

# combine marret19_sh and thole21
str(marret19_sh_all_speciescomb)
names(marret19_sh_all_speciescomb)[57] <- "Squa_prop"
#write.table(marret19_sh_all_speciescomb, file="marret19_sh_all_speciescomb.txt", row.names=TRUE, col.names=TRUE)

str(thole21_all)
#write.table(thole21_all, file="thole21_all.txt", row.names=TRUE, col.names=TRUE)

# Prep both data frames
marret19_sh_all_comb <- select(marret19_sh_all_speciescomb, -sample, -dino_sum)
#write.table(marret19_sh_all_comb, file="marret19_sh_all_comb.txt", row.names=TRUE, col.names=TRUE)
thole21_all_comb <- select(thole21_all, - sample, - dino_sum) 
#write.table(thole21_all_comb, file="thole21_all_comb.txt", row.names=TRUE, col.names=TRUE)

# Combine data sets

# first data set will have S ant separated 
sh21_Sant_sep <- smartbind(marret19_sh_all_comb, thole21_all_comb)
str(sh21_Sant_sep)
sh21_Sant_sep[is.na(sh21_Sant_sep)] <- 0


# second one will have S ant combined 
sh21_Sant_combo <- mutate(sh21_Sant_sep, 
                          Sant_all_prop = Sant_prop + Sant_2_prop)

sh21 <- select(sh21_Sant_combo, - Sant_prop, - Sant_2_prop)
sh21[is.na(sh21)] <- 0
str(sh21) 
#write.table(sh21, file="sh21.txt", row.names=TRUE, col.names=TRUE)


# 9.Southern Hemisphere Clustering ----
# we choose the S ant combined data set

str(sh21)
# make sure there are no NAs
sh21[is.na(sh21)] = 0
sh21_cluster <- select (sh21, -Station, - Longitude, - Latitude)
sh21_cluster <- as.data.frame(sh21_cluster)
str(sh21_cluster)
write.table(sh21_cluster, file="sh21_cluster.txt", row.names=TRUE, col.names=TRUE)

# 9.1 DETERMINE # of Ks ----

# Library needed is: factoextra
# Elbow method
fviz_nbclust(sh21_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(sh21_cluster, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy, but recommended value nboot= 500 for analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(sh21_cluster, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# CHOOSING K 

k_sh21 <- list ()
for (i in 1:20){
  k_sh21[[i]] <- kmeans(sh21_cluster, i, nstart = 100)
} 
k_sh21  

# look at ratios between_sum of squares/totalsumofsquares

betweenss_totss_sh21 <- list()
for (i in 1:20){
  betweenss_totss_sh21[[i]] <- k_sh21[[i]]$betweenss/k_sh21[[i]]$totss
}  

plot(1:20, betweenss_totss_sh21, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)" )  


# 9.2 ASSIGN CLUSTERS ----


# run clustering for different cluster solutions
set.seed(1230)
fitK_sh21_7 <- kmeans(sh21_cluster, 7, iter.max = 1000, nstart = 100)
fitK_sh21_8 <- kmeans(sh21_cluster, 8, iter.max = 1000, nstart = 100)
fitK_sh21_9 <- kmeans(sh21_cluster, 9, iter.max = 1000, nstart = 100)
fitK_sh21_10 <- kmeans(sh21_cluster, 10, iter.max = 1000, nstart = 100)
fitK_sh21_11 <- kmeans(sh21_cluster, 11, iter.max = 1000, nstart = 100)
#str(fitK_sh21_9)
# This means that R will try 100 different random starting assignments and then select the best results corresponding to the one with the lowest within cluster variation.


str(sh21)
# Create data frames with samples assigned to clusters (from 7-11 cluster solution)
sh21_7Cluster <- cbind(sh21, fitK_sh21_7$cluster)
sh21_7Cluster$`fitK_sh21_7$cluster` <- as.character(sh21_7Cluster$`fitK_sh21_7$cluster`)
str(sh21_7Cluster)
names(sh21_7Cluster)[61] <- "cluster"


sh21_8Cluster <- cbind(sh21, fitK_sh21_8$cluster)
sh21_8Cluster$`fitK_sh21_8$cluster` <- as.character(sh21_8Cluster$`fitK_sh21_8$cluster`)
names(sh21_8Cluster)[61] <- "cluster"
str(sh21_8Cluster)

sh21_9Cluster <- cbind(sh21, fitK_sh21_9$cluster)
sh21_9Cluster$`fitK_sh21_9$cluster` <- as.character(sh21_9Cluster$`fitK_sh21_9$cluster`)
names(sh21_9Cluster)[61] <- "cluster"
str(sh21_9Cluster)

sh21_10Cluster <- cbind(sh21, fitK_sh21_10$cluster)
sh21_10Cluster$`fitK_sh21_10$cluster` <- as.character(sh21_10Cluster$`fitK_sh21_10$cluster`)
names(sh21_10Cluster)[61] <- "cluster"
str(sh21_10Cluster)

sh21_11Cluster <- cbind(sh21, fitK_sh21_11$cluster)
sh21_11Cluster$`fitK_sh21_11$cluster` <- as.character(sh21_11Cluster$`fitK_sh21_11$cluster`)
names(sh21_11Cluster)[61] <- "cluster"
str(sh21_11Cluster)

# 9.3 DINOS in  CLUSTERS ----

# 9.3.1 Split data frames in cluster number data frames ----
sh21_7Cluster_split <- split(sh21_7Cluster, sh21_7Cluster$cluster)
str(sh21_7Cluster_split)

sh21_8Cluster_split <- split(sh21_8Cluster, sh21_8Cluster$cluster)
str(sh21_8Cluster_split)

sh21_9Cluster_split <- split(sh21_9Cluster, sh21_9Cluster$cluster)
str(sh21_9Cluster_split)

sh21_10Cluster_split <- split(sh21_10Cluster, sh21_10Cluster$cluster)
str(sh21_10Cluster_split)

sh21_11Cluster_split <- split(sh21_11Cluster, sh21_11Cluster$cluster)
str(sh21_11Cluster_split)

# 9.3.2 7 cluster solution ----

sh21_7Cluster1 <- sh21_7Cluster_split$"1"
sh21_7Cluster2 <- sh21_7Cluster_split$"2"
sh21_7Cluster3 <- sh21_7Cluster_split$"3"
sh21_7Cluster4 <- sh21_7Cluster_split$"4"
sh21_7Cluster5 <- sh21_7Cluster_split$"5"
sh21_7Cluster6 <- sh21_7Cluster_split$"6"
sh21_7Cluster7 <- sh21_7Cluster_split$"7"

# have a look at how the data frames look like
str(sh21_7Cluster1)

# this prepares them for plotting later on 
sh21_7Cluster1_ready <- select(sh21_7Cluster1, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster1_long <- pivot_longer(sh21_7Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )
str(sh21_7Cluster1_long)

sh21_7Cluster2_ready <- select(sh21_7Cluster2, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster2_long <- pivot_longer(sh21_7Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_7Cluster3_ready <- select(sh21_7Cluster3, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster3_long <- pivot_longer(sh21_7Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_7Cluster4_ready <- select(sh21_7Cluster4, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster4_long <- pivot_longer(sh21_7Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_7Cluster5_ready <- select(sh21_7Cluster5, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster5_long <- pivot_longer(sh21_7Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_7Cluster6_ready <- select(sh21_7Cluster6, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster6_long <- pivot_longer(sh21_7Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_7Cluster7_ready <- select(sh21_7Cluster7, -Station, - Longitude, - Latitude, -cluster) 
sh21_7Cluster7_long <- pivot_longer(sh21_7Cluster7_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 9.3.4 8 cluster solution ----

sh21_8Cluster1 <- sh21_8Cluster_split$"1"
sh21_8Cluster2 <- sh21_8Cluster_split$"2"
sh21_8Cluster3 <- sh21_8Cluster_split$"3"
sh21_8Cluster4 <- sh21_8Cluster_split$"4"
sh21_8Cluster5 <- sh21_8Cluster_split$"5"
sh21_8Cluster6 <- sh21_8Cluster_split$"6"
sh21_8Cluster7 <- sh21_8Cluster_split$"7"
sh21_8Cluster8 <- sh21_8Cluster_split$"8"

str(sh21_8Cluster1)

sh21_8Cluster1_ready <- select(sh21_8Cluster1, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster1_long <- pivot_longer(sh21_8Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )
str(sh21_8Cluster1_long)

sh21_8Cluster2_ready <- select(sh21_8Cluster2, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster2_long <- pivot_longer(sh21_8Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster3_ready <- select(sh21_8Cluster3, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster3_long <- pivot_longer(sh21_8Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster4_ready <- select(sh21_8Cluster4, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster4_long <- pivot_longer(sh21_8Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster5_ready <- select(sh21_8Cluster5, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster5_long <- pivot_longer(sh21_8Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster6_ready <- select(sh21_8Cluster6, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster6_long <- pivot_longer(sh21_8Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster7_ready <- select(sh21_8Cluster7, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster7_long <- pivot_longer(sh21_8Cluster7_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_8Cluster8_ready <- select(sh21_8Cluster8, -Station, - Longitude, - Latitude, -cluster) 
sh21_8Cluster8_long <- pivot_longer(sh21_8Cluster8_ready, cols = everything (), names_to = "species", values_to = "percentage" )

# 9.3.5 9 cluster solution ----

sh21_9Cluster1 <- sh21_9Cluster_split$"1"
sh21_9Cluster2 <- sh21_9Cluster_split$"2"
sh21_9Cluster3 <- sh21_9Cluster_split$"3"
sh21_9Cluster4 <- sh21_9Cluster_split$"4"
sh21_9Cluster5 <- sh21_9Cluster_split$"5"
sh21_9Cluster6 <- sh21_9Cluster_split$"6"
sh21_9Cluster7 <- sh21_9Cluster_split$"7"
sh21_9Cluster8 <- sh21_9Cluster_split$"8"
sh21_9Cluster9 <- sh21_9Cluster_split$"9"

str(sh21_9Cluster1)
#write.table(sh21_9Cluster1, file="sh21_9Cluster1.txt", row.names=TRUE, col.names=TRUE)

sh21_9Cluster1_ready <- select(sh21_9Cluster1, -Station, - Longitude, - Latitude, -cluster) 
#write.table(sh21_9Cluster1_ready, file="sh21_9Cluster1_ready.txt", row.names=TRUE, col.names=TRUE)


sh21_9Cluster1_long <- pivot_longer(sh21_9Cluster1_ready, cols = everything(), names_to = "species", names_prefix = "prop", values_to = "percentage")
str(sh21_9Cluster1_long)

# just to check all the speices in cluster 1
sh21_9Cluster1_long$species
x <- unique(sh21_9Cluster1_long$species)
x

# and continue with clusters
sh21_9Cluster2_ready <- select(sh21_9Cluster2, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster2_long <- pivot_longer(sh21_9Cluster2_ready, cols = everything (), names_to = "species", names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster3_ready <- select(sh21_9Cluster3, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster3_long <- pivot_longer(sh21_9Cluster3_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster4_ready <- select(sh21_9Cluster4, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster4_long <- pivot_longer(sh21_9Cluster4_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster5_ready <- select(sh21_9Cluster5, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster5_long <- pivot_longer(sh21_9Cluster5_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster6_ready <- select(sh21_9Cluster6, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster6_long <- pivot_longer(sh21_9Cluster6_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster7_ready <- select(sh21_9Cluster7, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster7_long <- pivot_longer(sh21_9Cluster7_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster8_ready <- select(sh21_9Cluster8, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster8_long <- pivot_longer(sh21_9Cluster8_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster9_ready <- select(sh21_9Cluster9, -Station, - Longitude, - Latitude, -cluster) 
sh21_9Cluster9_long <- pivot_longer(sh21_9Cluster9_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

# 9.3.6 9 cluster solution, less dinos ----

sh21_9Cluster1_short_ready <- select(sh21_9Cluster1, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster1_short_long <- pivot_longer(sh21_9Cluster1_short_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster2_short_ready <- select(sh21_9Cluster2, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster2_short_long <- pivot_longer(sh21_9Cluster2_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster3_short_ready <- select(sh21_9Cluster3, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster3_short_long <- pivot_longer(sh21_9Cluster3_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster4_short_ready <- select(sh21_9Cluster4, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster4_short_long <- pivot_longer(sh21_9Cluster4_short_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster5_short_ready <- select(sh21_9Cluster5, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster5_short_long <- pivot_longer(sh21_9Cluster5_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster6_short_ready <- select(sh21_9Cluster6, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster6_short_long <- pivot_longer(sh21_9Cluster6_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster7_short_ready <- select(sh21_9Cluster7, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster7_short_long <- pivot_longer(sh21_9Cluster7_short_ready, cols = everything (), names_to = "species", names_prefix = "prop",values_to = "percentage" )

sh21_9Cluster8_short_ready <- select(sh21_9Cluster8, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster8_short_long <- pivot_longer(sh21_9Cluster8_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )

sh21_9Cluster9_short_ready <- select(sh21_9Cluster9, -Station, - Longitude, - Latitude, -cluster,
                                     -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop, 
                                     -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                     -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                     -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                     -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                     -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop, 
                                     -Espp_comb_prop,-Sram_bul_prop,-Smem_bel_prop,-Ipli_prop,-Istr_prop, 
                                     -Ivar_prop,-Ivel_prop) 
sh21_9Cluster9_short_long <- pivot_longer(sh21_9Cluster9_short_ready, cols = everything (), names_to = "species",names_prefix = "prop", values_to = "percentage" )


# 9.3.7 10 cluster solution ----

sh21_10Cluster1 <- sh21_10Cluster_split$"1"
sh21_10Cluster2 <- sh21_10Cluster_split$"2"
sh21_10Cluster3 <- sh21_10Cluster_split$"3"
sh21_10Cluster4 <- sh21_10Cluster_split$"4"
sh21_10Cluster5 <- sh21_10Cluster_split$"5"
sh21_10Cluster6 <- sh21_10Cluster_split$"6"
sh21_10Cluster7 <- sh21_10Cluster_split$"7"
sh21_10Cluster8 <- sh21_10Cluster_split$"8"
sh21_10Cluster9 <- sh21_10Cluster_split$"9"
sh21_10Cluster10 <- sh21_10Cluster_split$"10"

str(sh21_10Cluster1)

sh21_10Cluster1_ready <- select(sh21_10Cluster1, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster1_long <- pivot_longer(sh21_10Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )
str(sh21_10Cluster1_long)

sh21_10Cluster2_ready <- select(sh21_10Cluster2, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster2_long <- pivot_longer(sh21_10Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster3_ready <- select(sh21_10Cluster3, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster3_long <- pivot_longer(sh21_10Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster4_ready <- select(sh21_10Cluster4, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster4_long <- pivot_longer(sh21_10Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster5_ready <- select(sh21_10Cluster5, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster5_long <- pivot_longer(sh21_10Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster6_ready <- select(sh21_10Cluster6, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster6_long <- pivot_longer(sh21_10Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster7_ready <- select(sh21_10Cluster7, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster7_long <- pivot_longer(sh21_10Cluster7_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster8_ready <- select(sh21_10Cluster8, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster8_long <- pivot_longer(sh21_10Cluster8_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster9_ready <- select(sh21_10Cluster9, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster9_long <- pivot_longer(sh21_10Cluster9_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster10_ready <- select(sh21_10Cluster10, -Station, - Longitude, - Latitude, -cluster)
sh21_10Cluster10_long <- pivot_longer(sh21_10Cluster10_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 9.3.8 10 cluster solution, less dinos ----

str(sh21_10Cluster1)

sh21_10Cluster1_short_ready <- select(sh21_10Cluster1, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster1_short_long <- pivot_longer(sh21_10Cluster1_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster2_short_ready <- select(sh21_10Cluster2, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster2_short_long <- pivot_longer(sh21_10Cluster2_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster3_short_ready <- select(sh21_10Cluster3, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster3_short_long <- pivot_longer(sh21_10Cluster3_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster4_short_ready <- select(sh21_10Cluster4, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster4_short_long <- pivot_longer(sh21_10Cluster4_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster5_short_ready <- select(sh21_10Cluster5, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster5_short_long <- pivot_longer(sh21_10Cluster5_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster6_short_ready <- select(sh21_10Cluster6, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster6_short_long <- pivot_longer(sh21_10Cluster6_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster7_short_ready <- select(sh21_10Cluster7, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster7_short_long <- pivot_longer(sh21_10Cluster7_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster8_short_ready <- select(sh21_10Cluster8, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster8_short_long <- pivot_longer(sh21_10Cluster8_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster9_short_ready <- select(sh21_10Cluster9, -Station, - Longitude, - Latitude, -cluster,
                                      -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                      -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                      -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                      -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                      -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                      -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster9_short_long <- pivot_longer(sh21_10Cluster9_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_10Cluster10_short_ready <- select(sh21_10Cluster10, -Station, - Longitude, - Latitude, -cluster,
                                       -Xxan_prop, -Vspi_prop, -Tvan_prop, -Tapp_prop, - Sste_prop,
                                       -Atax_prop,-Bspo_prop,-Btep_prop,-Dubr_prop,-Ibre_prop,
                                       -Lmac_prop,-Lspp_prop,-Oisr_prop,-Ojan_prop,-Olon_prop,
                                       -Pame_prop,-Pdal_prop,-Peri_prop,-Pkof_prop,-Ppsi_prop,
                                       -Psch_prop,-Pspp_prop,-Pzoh_prop,-Sben_prop,-Sdel_prop,
                                       -Spac_prop,-Srei_prop,-Srob_prop,-Tpel_prop, - Char_prop)
sh21_10Cluster10_short_long <- pivot_longer(sh21_10Cluster10_short_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 9.3.9 11 cluster solution ----

sh21_11Cluster1 <- sh21_11Cluster_split$"1"
sh21_11Cluster2 <- sh21_11Cluster_split$"2"
sh21_11Cluster3 <- sh21_11Cluster_split$"3"
sh21_11Cluster4 <- sh21_11Cluster_split$"4"
sh21_11Cluster5 <- sh21_11Cluster_split$"5"
sh21_11Cluster6 <- sh21_11Cluster_split$"6"
sh21_11Cluster7 <- sh21_11Cluster_split$"7"
sh21_11Cluster8 <- sh21_11Cluster_split$"8"
sh21_11Cluster9 <- sh21_11Cluster_split$"9"
sh21_11Cluster10 <- sh21_11Cluster_split$"10"
sh21_11Cluster11 <- sh21_11Cluster_split$"11"

str(sh21_11Cluster1)

sh21_11Cluster1_ready <- select(sh21_11Cluster1, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster1_long <- pivot_longer(sh21_11Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )
str(sh21_11Cluster1_long)

sh21_11Cluster2_ready <- select(sh21_11Cluster2, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster2_long <- pivot_longer(sh21_11Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_11Cluster3_ready <- select(sh21_11Cluster3, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster3_long <- pivot_longer(sh21_11Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

sh21_11Cluster4_ready <- select(sh21_11Cluster4, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster4_long <- pivot_longer(sh21_11Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster5_ready <- select(sh21_11Cluster5, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster5_long <- pivot_longer(sh21_11Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster6_ready <- select(sh21_11Cluster6, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster6_long <- pivot_longer(sh21_11Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster7_ready <- select(sh21_11Cluster7, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster7_long <- pivot_longer(sh21_11Cluster7_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster8_ready <- select(sh21_11Cluster8, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster8_long <- pivot_longer(sh21_11Cluster8_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster9_ready <- select(sh21_11Cluster9, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster9_long <- pivot_longer(sh21_11Cluster9_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster10_ready <- select(sh21_11Cluster10, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster10_long <- pivot_longer(sh21_11Cluster10_ready, cols = everything (), names_to = "species", values_to = "percentage" )
sh21_11Cluster11_ready <- select(sh21_11Cluster11, -Station, - Longitude, - Latitude, -cluster)
sh21_11Cluster11_long <- pivot_longer(sh21_11Cluster11_ready, cols = everything (), names_to = "species", values_to = "percentage" )




##################################################################################

# 10. OVERLYING SURFACE DATA ----

# 10.1 original output files ----
# I've taken out the Candel files from the output file folder
# I've taken out the 13 samples I have no dinos for

list_of_files1 <- list.files(path = "./Model_Surface1", recursive = TRUE,
                             pattern = "\\.txt$", 
                             full.names = TRUE)
list_of_files1

#assuming tab separated values with a header    
datalist1 <- lapply(list_of_files1, function(x)read.table(x, header=T)) 

#assuming the same header/columns for all files
# shorten the data sets to 3 years by filtering for time, and Latitude 

variables_surface1 <-  do.call("rbind", datalist1) 
variables_surface1 <- variables_surface1[rowSums(variables_surface1 != 0) > 0,]
variables_surface1 <- variables_surface1[rowSums(is.na(variables_surface1)) != ncol(variables_surface1),]
names(variables_surface1)[2] <- "Time"
str(variables_surface1)
variables_surface1 <- filter(variables_surface1, Time < 1074)

str(variables_surface1)

# Split for each location/trajectory separately
variables_surface1_split <- split(variables_surface1, variables_surface1$Trajectory)
str(variables_surface1_split)


# 358 observations over 3 years = 119.3 per year, = 29.83 per seasons
# Seasons defined as: winter: jja, spring son, summer: djf, autumn: mam
# Calculate annual means 
# Calculate seasonal averages 

win <- 1
sp <- 2
sum <- 3
aut <- 4

seasons <- c(win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win,win, win, win, 
             sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, 
             sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, 
             aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, 
             win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win,win, win, win, win, win, win, win, win, win, win, win, 
             sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp,  
             sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum,
             aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut,  
             win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win, win,win, win, win, win, win, win, win, win, win, win, win, 
             sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, sp, 
             sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, sum, 
             aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut, aut,
             win, win, win, win, win, win, win, win, win, win)
str(seasons)

# 228046/358 = 637
#season <- 419*seasons
season <- rep(seasons, times = 637)
str(season)
variables_surface1 <- data.frame(variables_surface1, season)
str(variables_surface1)

# Initial ZEROS IN Sites 241,242,243 and 612-616, 638 were on land in model: EXCHANGED WITH SLIGHTLY SHIFTED COORDINATES


Long_Model1_mean <- tapply(variables_surface1$Longitude_Model, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Lat_Model1_mean <- tapply(variables_surface1$Latitude_Model, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Long_Dino1_mean <- tapply(variables_surface1$Longitude_Dino, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Lat_Dino1_mean <- tapply(variables_surface1$Latitude_Dino, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Temp1_mean <- tapply(variables_surface1$Temperature, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity1_mean <- tapply(variables_surface1$Salinity, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate1_mean <- tapply(variables_surface1$Nitrate, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice1_mean <- tapply(variables_surface1$Ice, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence1_mean <- tapply(variables_surface1$Ice_presence, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate1_mean <- tapply(variables_surface1$Silicate, variables_surface1$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Long_Model1_median <- tapply(variables_surface1$Longitude_Model, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Lat_Model1_median <- tapply(variables_surface1$Latitude_Model, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Long_Dino1_median <- tapply(variables_surface1$Longitude_Dino, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Lat_Dino1_median <- tapply(variables_surface1$Latitude_Dino, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Temp1_median <- tapply(variables_surface1$Temperature, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity1_median <- tapply(variables_surface1$Salinity, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate1_median <- tapply(variables_surface1$Nitrate, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice1_median <- tapply(variables_surface1$Ice, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence1_median <- tapply(variables_surface1$Ice_presence, variables_surface1$Trajectory,median, na.rm = TRUE, simplify = TRUE)
Silicate1_median <- tapply(variables_surface1$Silicate, variables_surface1$Trajectory, median, na.rm = TRUE, simplify = TRUE)

# split again, mean 

variables_surface1_winter <- filter(variables_surface1, variables_surface1$season == win)
variables_surface1_spring <- filter(variables_surface1, variables_surface1$season == sp)
variables_surface1_summer <- filter(variables_surface1, variables_surface1$season == sum)
variables_surface1_autumn <- filter(variables_surface1, variables_surface1$season == aut)
str(variables_surface1_winter)

# Means 
Temp1_winter_mean <- tapply(variables_surface1_winter$Temperature, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity1_winter_mean <- tapply(variables_surface1_winter$Salinity, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate1_winter_mean <- tapply(variables_surface1_winter$Nitrate, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice1_winter_mean <- tapply(variables_surface1_winter$Ice, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence1_winter_mean <- tapply(variables_surface1_winter$Ice_presence, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate1_winter_mean <- tapply(variables_surface1_winter$Silicate, variables_surface1_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp1_spring_mean <- tapply(variables_surface1_spring$Temperature, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity1_spring_mean <- tapply(variables_surface1_spring$Salinity, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate1_spring_mean <- tapply(variables_surface1_spring$Nitrate, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice1_spring_mean <- tapply(variables_surface1_spring$Ice, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence1_spring_mean <- tapply(variables_surface1_spring$Ice_presence, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate1_spring_mean <- tapply(variables_surface1_spring$Silicate, variables_surface1_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp1_summer_mean <- tapply(variables_surface1_summer$Temperature, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity1_summer_mean <- tapply(variables_surface1_summer$Salinity, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate1_summer_mean <- tapply(variables_surface1_summer$Nitrate, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice1_summer_mean <- tapply(variables_surface1_summer$Ice, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence1_summer_mean <- tapply(variables_surface1_summer$Ice_presence, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate1_summer_mean <- tapply(variables_surface1_summer$Silicate, variables_surface1_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp1_autumn_mean <- tapply(variables_surface1_autumn$Temperature, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity1_autumn_mean <- tapply(variables_surface1_autumn$Salinity, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate1_autumn_mean <- tapply(variables_surface1_autumn$Nitrate, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice1_autumn_mean <- tapply(variables_surface1_autumn$Ice, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence1_autumn_mean <- tapply(variables_surface1_autumn$Ice_presence, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate1_autumn_mean <- tapply(variables_surface1_autumn$Silicate, variables_surface1_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)


# Medians 
Temp1_winter_median <- tapply(variables_surface1_winter$Temperature, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity1_winter_median <- tapply(variables_surface1_winter$Salinity, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate1_winter_median <- tapply(variables_surface1_winter$Nitrate, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice1_winter_median <- tapply(variables_surface1_winter$Ice, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence1_winter_median <- tapply(variables_surface1_winter$Ice_presence, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate1_winter_median <- tapply(variables_surface1_winter$Silicate, variables_surface1_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp1_spring_median <- tapply(variables_surface1_spring$Temperature, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity1_spring_median <- tapply(variables_surface1_spring$Salinity, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate1_spring_median <- tapply(variables_surface1_spring$Nitrate, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice1_spring_median <- tapply(variables_surface1_spring$Ice, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence1_spring_median <- tapply(variables_surface1_spring$Ice_presence, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate1_spring_median <- tapply(variables_surface1_spring$Silicate, variables_surface1_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp1_summer_median <- tapply(variables_surface1_summer$Temperature, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity1_summer_median <- tapply(variables_surface1_summer$Salinity, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate1_summer_median <- tapply(variables_surface1_summer$Nitrate, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice1_summer_median <- tapply(variables_surface1_summer$Ice, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence1_summer_median <- tapply(variables_surface1_summer$Ice_presence, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate1_summer_median <- tapply(variables_surface1_summer$Silicate, variables_surface1_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp1_autumn_median <- tapply(variables_surface1_autumn$Temperature, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity1_autumn_median <- tapply(variables_surface1_autumn$Salinity, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate1_autumn_median <- tapply(variables_surface1_autumn$Nitrate, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice1_autumn_median <- tapply(variables_surface1_autumn$Ice, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence1_autumn_median <- tapply(variables_surface1_autumn$Ice_presence, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate1_autumn_median <- tapply(variables_surface1_autumn$Silicate, variables_surface1_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)


# Span
Temp1_span_summer_winter_mean <- Temp1_summer_mean - Temp1_winter_mean
Temp1_span_summer_winter_median <- Temp1_summer_median - Temp1_winter_median
Salinity1_span_summer_winter_mean <- Salinity1_summer_mean - Salinity1_winter_mean
Salinity1_span_summer_winter_median <- Salinity1_summer_median - Salinity1_winter_median
Nitrate1_span_summer_winter_mean <- Nitrate1_summer_mean - Nitrate1_winter_mean
Nitrate1_span_summer_winter_median <- Nitrate1_summer_median - Nitrate1_winter_median
Ice1_span_summer_winter_mean <- Ice1_summer_mean - Ice1_winter_mean
Ice1_span_summer_winter_median <- Ice1_summer_median - Ice1_winter_median
Ice_presence1_span_summer_winter_mean <- Ice_presence1_summer_mean - Ice_presence1_winter_mean
Ice_presence1_span_summer_winter_median <- Ice_presence1_summer_median - Ice_presence1_winter_median
Silicate1_span_summer_winter_mean <- Silicate1_summer_mean - Silicate1_winter_mean
Silicate1_span_summer_winter_median <- Silicate1_summer_median - Silicate1_winter_median

# 10.2 extra output files ----

list_of_files2 <- list.files(path = "./Model_Surface2", recursive = TRUE,
                             pattern = "\\.txt$", 
                             full.names = TRUE)

list_of_files2

#assuming tab separated values with a header    
datalist2 = lapply(list_of_files2, function(x)read.table(x, header=T)) 

#assuming the same header/columns for all files
# shorten the data sets to 3 years by filtering for time 
variables_surface2 <-  do.call("rbind", datalist2) 
str(variables_surface2)
variables_surface2 <- variables_surface2[rowSums(variables_surface2 != 0) > 0,]
variables_surface2 <- variables_surface2[rowSums(is.na(variables_surface2)) != ncol(variables_surface2),]
names(variables_surface2)[2] <- "Time"
variables_surface2 <- filter(variables_surface2, Time < 1074)
variables_surface2 <- filter(variables_surface2, Time > - 1)
str(variables_surface2)

# varsurf_2_traj <- variables_surface2$Trajectory
# write.table(varsurf_1_traj, file = "varsurf_2_traj.txt")
# varsurf_2_long <- variables_surface2$Longitude
# write.table(varsurf_1_long, file = "varsurf_2_long.txt")
# varsurf_2_lat <- variables_surface2Latitude
# write.table(varsurf_1_lat, file = "varsurf_2_lat.txt")
# 

# Calculate annual means 

# seasonal means 
# add season column
# 9082/358
season2 <- rep(seasons, times = 18)
variables_surface2 <- data.frame(variables_surface2, season2)


# ZEROS IN  ExtraSites site 17, model thinks on land: EXCHANGED WITH SLIGHTLY SHIFTED COORDINATES
Long_Model2_mean  <- tapply(variables_surface2$Longitude_Model, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Lat_Model2_mean  <- tapply(variables_surface2$Latitude_Model, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Long_Dino2_mean  <- tapply(variables_surface2$Longitude_Dino, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Lat_Dino2_mean  <- tapply(variables_surface2$Latitude_Dino, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Temp2_mean  <- tapply(variables_surface2$Temperature, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity2_mean <- tapply(variables_surface2$Salinity, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate2_mean <- tapply(variables_surface2$Nitrate, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice2_mean <- tapply(variables_surface2$Ice, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence2_mean <- tapply(variables_surface2$Ice_presence, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate2_mean <- tapply(variables_surface2$Silicate, variables_surface2$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Long_Model2_median  <- tapply(variables_surface2$Longitude_Model, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Lat_Model2_median  <- tapply(variables_surface2$Latitude_Model, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Long_Dino2_median  <- tapply(variables_surface2$Longitude_Dino, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Lat_Dino2_median  <- tapply(variables_surface2$Latitude_Dino, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Temp2_median  <- tapply(variables_surface2$Temperature, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity2_median <- tapply(variables_surface2$Salinity, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate2_median <- tapply(variables_surface2$Nitrate, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice2_median <- tapply(variables_surface2$Ice, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence2_median <- tapply(variables_surface2$Ice_presence, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate2_median <- tapply(variables_surface2$Silicate, variables_surface2$Trajectory, median, na.rm = TRUE, simplify = TRUE)

# split again 

variables_surface2_winter <- filter(variables_surface2, variables_surface2$season == win)
variables_surface2_spring <- filter(variables_surface2, variables_surface2$season == sp)
variables_surface2_summer <- filter(variables_surface2, variables_surface2$season == sum)
variables_surface2_autumn <- filter(variables_surface2, variables_surface2$season == aut)
str(variables_surface2_winter)

# Means 
Temp2_winter_mean <- tapply(variables_surface2_winter$Temperature, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity2_winter_mean <- tapply(variables_surface2_winter$Salinity, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate2_winter_mean <- tapply(variables_surface2_winter$Nitrate, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice2_winter_mean <- tapply(variables_surface2_winter$Ice, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence2_winter_mean <- tapply(variables_surface2_winter$Ice_presence, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate2_winter_mean <- tapply(variables_surface2_winter$Silicate, variables_surface2_winter$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp2_spring_mean <- tapply(variables_surface2_spring$Temperature, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity2_spring_mean <- tapply(variables_surface2_spring$Salinity, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate2_spring_mean <- tapply(variables_surface2_spring$Nitrate, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice2_spring_mean <- tapply(variables_surface2_spring$Ice, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence2_spring_mean <- tapply(variables_surface2_spring$Ice_presence, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate2_spring_mean <- tapply(variables_surface2_spring$Silicate, variables_surface2_spring$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp2_summer_mean <- tapply(variables_surface2_summer$Temperature, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity2_summer_mean <- tapply(variables_surface2_summer$Salinity, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate2_summer_mean <- tapply(variables_surface2_summer$Nitrate, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice2_summer_mean <- tapply(variables_surface2_summer$Ice, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence2_summer_mean <- tapply(variables_surface2_summer$Ice_presence, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate2_summer_mean <- tapply(variables_surface2_summer$Silicate, variables_surface2_summer$Trajectory, mean, na.rm = TRUE, simplify = TRUE)

Temp2_autumn_mean <- tapply(variables_surface2_autumn$Temperature, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Salinity2_autumn_mean <- tapply(variables_surface2_autumn$Salinity, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Nitrate2_autumn_mean <- tapply(variables_surface2_autumn$Nitrate, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice2_autumn_mean <- tapply(variables_surface2_autumn$Ice, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Ice_presence2_autumn_mean <- tapply(variables_surface2_autumn$Ice_presence, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)
Silicate2_autumn_mean <- tapply(variables_surface2_autumn$Silicate, variables_surface2_autumn$Trajectory, mean, na.rm = TRUE, simplify = TRUE)


# Medians 
Temp2_winter_median <- tapply(variables_surface2_winter$Temperature, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity2_winter_median <- tapply(variables_surface2_winter$Salinity, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate2_winter_median <- tapply(variables_surface2_winter$Nitrate, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice2_winter_median <- tapply(variables_surface2_winter$Ice, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence2_winter_median <- tapply(variables_surface2_winter$Ice_presence, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate2_winter_median <- tapply(variables_surface2_winter$Silicate, variables_surface2_winter$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp2_spring_median <- tapply(variables_surface2_spring$Temperature, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity2_spring_median <- tapply(variables_surface2_spring$Salinity, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate2_spring_median <- tapply(variables_surface2_spring$Nitrate, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice2_spring_median <- tapply(variables_surface2_spring$Ice, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence2_spring_median <- tapply(variables_surface2_spring$Ice_presence, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate2_spring_median <- tapply(variables_surface2_spring$Silicate, variables_surface2_spring$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp2_summer_median <- tapply(variables_surface2_summer$Temperature, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity2_summer_median <- tapply(variables_surface2_summer$Salinity, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate2_summer_median <- tapply(variables_surface2_summer$Nitrate, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice2_summer_median <- tapply(variables_surface2_summer$Ice, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence2_summer_median <- tapply(variables_surface2_summer$Ice_presence, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate2_summer_median <- tapply(variables_surface2_summer$Silicate, variables_surface2_summer$Trajectory, median, na.rm = TRUE, simplify = TRUE)

Temp2_autumn_median <- tapply(variables_surface2_autumn$Temperature, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Salinity2_autumn_median <- tapply(variables_surface2_autumn$Salinity, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Nitrate2_autumn_median <- tapply(variables_surface2_autumn$Nitrate, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice2_autumn_median <- tapply(variables_surface2_autumn$Ice, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Ice_presence2_autumn_median <- tapply(variables_surface2_autumn$Ice_presence, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)
Silicate2_autumn_median <- tapply(variables_surface2_autumn$Silicate, variables_surface2_autumn$Trajectory, median, na.rm = TRUE, simplify = TRUE)


# Span
Temp2_span_summer_winter_mean <- Temp2_summer_mean - Temp2_winter_mean
Temp2_span_summer_winter_median <- Temp2_summer_median - Temp2_winter_median
Salinity2_span_summer_winter_mean <- Salinity2_summer_mean - Salinity2_winter_mean
Salinity2_span_summer_winter_median <- Salinity2_summer_median - Salinity2_winter_median
Nitrate2_span_summer_winter_mean <- Nitrate2_summer_mean - Nitrate2_winter_mean
Nitrate2_span_summer_winter_median <- Nitrate2_summer_median - Nitrate2_winter_median
Ice2_span_summer_winter_mean <- Ice2_summer_mean - Ice2_winter_mean
Ice2_span_summer_winter_median <- Ice2_summer_median - Ice2_winter_median
Ice_presence2_span_summer_winter_mean <- Ice_presence2_summer_mean - Ice_presence2_winter_mean
Ice_presence2_span_summer_winter_median <- Ice_presence2_summer_median - Ice_presence2_winter_median
Silicate2_span_summer_winter_mean <- Silicate2_summer_mean - Silicate2_winter_mean
Silicate2_span_summer_winter_median <- Silicate2_summer_median - Silicate2_winter_median

# 10.3 COMBINE DATA SETS ----

env_surface1 <- cbind(Temp1_mean, Salinity1_mean, Nitrate1_mean, Ice1_mean, Ice_presence1_mean, Silicate1_mean,
                      Temp1_median, Salinity1_median, Nitrate1_median, Ice1_median, Ice_presence1_median, Silicate1_median,
                      Temp1_spring_mean, Salinity1_spring_mean, Nitrate1_spring_mean, Ice1_spring_mean, Ice_presence1_spring_mean, Silicate1_spring_mean,
                      Temp1_summer_mean, Salinity1_summer_mean, Nitrate1_summer_mean, Ice1_summer_mean, Ice_presence1_summer_mean, Silicate1_summer_mean,
                      Temp1_autumn_mean, Salinity1_autumn_mean, Nitrate1_autumn_mean, Ice1_autumn_mean, Ice_presence1_autumn_mean, Silicate1_autumn_mean,
                      Temp1_winter_mean, Salinity1_winter_mean, Nitrate1_winter_mean, Ice1_winter_mean, Ice_presence1_winter_mean, Silicate1_winter_mean,
                      Temp1_spring_median, Salinity1_spring_median, Nitrate1_spring_median, Ice1_spring_median, Ice_presence1_spring_median, Silicate1_spring_median,
                      Temp1_summer_median, Salinity1_summer_median, Nitrate1_summer_median, Ice1_summer_median, Ice_presence1_summer_median, Silicate1_summer_median,
                      Temp1_autumn_median, Salinity1_autumn_median, Nitrate1_autumn_median, Ice1_autumn_median, Ice_presence1_autumn_median, Silicate1_autumn_median,
                      Temp1_winter_median, Salinity1_winter_median, Nitrate1_winter_median, Ice1_winter_median, Ice_presence1_winter_median, Silicate1_winter_median,
                      Temp1_span_summer_winter_mean, Temp1_span_summer_winter_median, Salinity1_span_summer_winter_mean, Salinity1_span_summer_winter_median,
                      Nitrate1_span_summer_winter_mean, Nitrate1_span_summer_winter_median, Ice1_span_summer_winter_mean, Ice1_span_summer_winter_median, 
                      Ice_presence1_span_summer_winter_mean, Ice_presence1_span_summer_winter_median, Silicate1_span_summer_winter_mean, Silicate1_span_summer_winter_median, 
                      Long_Model1_mean, Lat_Model1_mean,Long_Dino1_mean, Lat_Dino1_mean)

env_surface1 <- as.data.frame(env_surface1)
names(env_surface1)[1] <- "Temp_mean"
names(env_surface1)[2] <- "Salinity_mean"
names(env_surface1)[3] <- "Nitrate_mean"
names(env_surface1)[4] <- "Ice_mean"
names(env_surface1)[5] <- "Ice_presence_mean"
names(env_surface1)[6] <- "Silicate_mean"
names(env_surface1)[7] <- "Temp_median"
names(env_surface1)[8] <- "Salinity_median"
names(env_surface1)[9] <- "Nitrate_median"
names(env_surface1)[10] <- "Ice_median"
names(env_surface1)[11] <- "Ice_presence_median"
names(env_surface1)[12] <- "Silicate_median"
names(env_surface1)[13] <- "Temp_spring_mean"
names(env_surface1)[14] <- "Salinity_spring_mean"
names(env_surface1)[15] <- "Nitrate_spring_mean"
names(env_surface1)[16] <- "Ice_spring_mean"
names(env_surface1)[17] <- "Ice_presence_spring_mean"
names(env_surface1)[18] <- "Silicate_spring_mean"
names(env_surface1)[19] <- "Temp_summer_mean"
names(env_surface1)[20] <- "Salinity_summer_mean"
names(env_surface1)[21] <- "Nitrate_summer_mean"
names(env_surface1)[22] <- "Ice_summer_mean"
names(env_surface1)[23] <- "Ice_presence_summer_mean"
names(env_surface1)[24] <- "Silicate_summer_mean"
names(env_surface1)[25] <- "Temp_autumn_mean"
names(env_surface1)[26] <- "Salinity_autumn_mean"
names(env_surface1)[27] <- "Nitrate_autumn_mean"
names(env_surface1)[28] <- "Ice_autumn_mean"
names(env_surface1)[29] <- "Ice_presence_autumn_mean"
names(env_surface1)[30] <- "Silicate_autumn_mean"
names(env_surface1)[31] <- "Temp_winter_mean"
names(env_surface1)[32] <- "Salinity_winter_mean"
names(env_surface1)[33] <- "Nitrate_winter_mean"
names(env_surface1)[34] <- "Ice_winter_mean"
names(env_surface1)[35] <- "Ice_presence_winter_mean"
names(env_surface1)[36] <- "Silicate_winter_mean"
names(env_surface1)[37] <- "Temp_spring_median"
names(env_surface1)[38] <- "Salinity_spring_median"
names(env_surface1)[39] <- "Nitrate_spring_median"
names(env_surface1)[40] <- "Ice_spring_median"
names(env_surface1)[41] <- "Ice_presence_spring_median"
names(env_surface1)[42] <- "Silicate_spring_median"
names(env_surface1)[43] <- "Temp_summer_median"
names(env_surface1)[44] <- "Salinity_summer_median"
names(env_surface1)[45] <- "Nitrate_summer_median"
names(env_surface1)[46] <- "Ice_summer_median"
names(env_surface1)[47] <- "Ice_presence_summer_median"
names(env_surface1)[48] <- "Silicate_summer_median"
names(env_surface1)[49] <- "Temp_autumn_median"
names(env_surface1)[50] <- "Salinity_autumn_median"
names(env_surface1)[51] <- "Nitrate_autumn_median"
names(env_surface1)[52] <- "Ice_autumn_median"
names(env_surface1)[53] <- "Ice_presence_autumn_median"
names(env_surface1)[54] <- "Silicate_autumn_median"
names(env_surface1)[55] <- "Temp_winter_median"
names(env_surface1)[56] <- "Salinity_winter_median"
names(env_surface1)[57] <- "Nitrate_winter_median"
names(env_surface1)[58] <- "Ice_winter_median"
names(env_surface1)[59] <- "Ice_presence_winter_median"
names(env_surface1)[60] <- "Silicate_winter_median"
names(env_surface1)[61] <- "Temp_span_summer_winter_mean"
names(env_surface1)[62] <- "Temp_span_summer_winter_median"
names(env_surface1)[63] <- "Salinity_span_summer_winter_mean"
names(env_surface1)[64] <- "Salinity_span_summer_winter_median"
names(env_surface1)[65] <- "Nitrate_span_summer_winter_mean"
names(env_surface1)[66] <- "Nitrate_span_summer_winter_median"
names(env_surface1)[67] <- "Ice_span_summer_winter_mean"
names(env_surface1)[68] <- "Ice_span_summer_winter_median"
names(env_surface1)[69] <- "Ice_presence_span_summer_winter_mean"
names(env_surface1)[70] <- "Ice_presence_span_summer_winter_median"
names(env_surface1)[71] <- "Silicate_span_summer_winter_mean"
names(env_surface1)[72] <- "Silicate_span_summer_winter_median"
names(env_surface1)[73] <- "Long_Model"
names(env_surface1)[74] <- "Lat_Model"
names(env_surface1)[75] <- "Long_Dino"
names(env_surface1)[76] <- "Lat_Dino"



env_surface2 <- cbind(Temp2_mean, Salinity2_mean, Nitrate2_mean, Ice2_mean, Ice_presence2_mean, Silicate2_mean,
                      Temp2_median, Salinity2_median, Nitrate2_median, Ice2_median, Ice_presence2_median, Silicate2_median,
                      Temp2_spring_mean, Salinity2_spring_mean, Nitrate2_spring_mean, Ice2_spring_mean, Ice_presence2_spring_mean, Silicate2_spring_mean,
                      Temp2_summer_mean, Salinity2_summer_mean, Nitrate2_summer_mean, Ice2_summer_mean, Ice_presence2_summer_mean, Silicate2_summer_mean,
                      Temp2_autumn_mean, Salinity2_autumn_mean, Nitrate2_autumn_mean, Ice2_autumn_mean, Ice_presence2_autumn_mean, Silicate2_autumn_mean,
                      Temp2_winter_mean, Salinity2_winter_mean, Nitrate2_winter_mean, Ice2_winter_mean, Ice_presence2_winter_mean, Silicate2_winter_mean,
                      Temp2_spring_median, Salinity2_spring_median, Nitrate2_spring_median, Ice2_spring_median, Ice_presence2_spring_median, Silicate2_spring_median,
                      Temp2_summer_median, Salinity2_summer_median, Nitrate2_summer_median, Ice2_summer_median, Ice_presence2_summer_median, Silicate2_summer_median,
                      Temp2_autumn_median, Salinity2_autumn_median, Nitrate2_autumn_median, Ice2_autumn_median, Ice_presence2_autumn_median, Silicate2_autumn_median,
                      Temp2_winter_median, Salinity2_winter_median, Nitrate2_winter_median, Ice2_winter_median, Ice_presence2_winter_median, Silicate2_winter_median,
                      Temp2_span_summer_winter_mean, Temp2_span_summer_winter_median, Salinity2_span_summer_winter_mean, Salinity2_span_summer_winter_median,
                      Nitrate2_span_summer_winter_mean, Nitrate2_span_summer_winter_median, Ice2_span_summer_winter_mean, Ice2_span_summer_winter_median, 
                      Ice_presence2_span_summer_winter_mean, Ice_presence2_span_summer_winter_median, Silicate2_span_summer_winter_mean, Silicate2_span_summer_winter_median,
                      Long_Model2_mean, Lat_Model2_mean,Long_Dino2_mean, Lat_Dino2_mean)

env_surface2 <- as.data.frame(env_surface2)
names(env_surface2)[1] <- "Temp_mean"
names(env_surface2)[2] <- "Salinity_mean"
names(env_surface2)[3] <- "Nitrate_mean"
names(env_surface2)[4] <- "Ice_mean"
names(env_surface2)[5] <- "Ice_presence_mean"
names(env_surface2)[6] <- "Silicate_mean"
names(env_surface2)[7] <- "Temp_median"
names(env_surface2)[8] <- "Salinity_median"
names(env_surface2)[9] <- "Nitrate_median"
names(env_surface2)[10] <- "Ice_median"
names(env_surface2)[11] <- "Ice_presence_median"
names(env_surface2)[12] <- "Silicate_median"
names(env_surface2)[13] <- "Temp_spring_mean"
names(env_surface2)[14] <- "Salinity_spring_mean"
names(env_surface2)[15] <- "Nitrate_spring_mean"
names(env_surface2)[16] <- "Ice_spring_mean"
names(env_surface2)[17] <- "Ice_presence_spring_mean"
names(env_surface2)[18] <- "Silicate_spring_mean"
names(env_surface2)[19] <- "Temp_summer_mean"
names(env_surface2)[20] <- "Salinity_summer_mean"
names(env_surface2)[21] <- "Nitrate_summer_mean"
names(env_surface2)[22] <- "Ice_summer_mean"
names(env_surface2)[23] <- "Ice_presence_summer_mean"
names(env_surface2)[24] <- "Silicate_summer_mean"
names(env_surface2)[25] <- "Temp_autumn_mean"
names(env_surface2)[26] <- "Salinity_autumn_mean"
names(env_surface2)[27] <- "Nitrate_autumn_mean"
names(env_surface2)[28] <- "Ice_autumn_mean"
names(env_surface2)[29] <- "Ice_presence_autumn_mean"
names(env_surface2)[30] <- "Silicate_autumn_mean"
names(env_surface2)[31] <- "Temp_winter_mean"
names(env_surface2)[32] <- "Salinity_winter_mean"
names(env_surface2)[33] <- "Nitrate_winter_mean"
names(env_surface2)[34] <- "Ice_winter_mean"
names(env_surface2)[35] <- "Ice_presence_winter_mean"
names(env_surface2)[36] <- "Silicate_winter_mean"
names(env_surface2)[37] <- "Temp_spring_median"
names(env_surface2)[38] <- "Salinity_spring_median"
names(env_surface2)[39] <- "Nitrate_spring_median"
names(env_surface2)[40] <- "Ice_spring_median"
names(env_surface2)[41] <- "Ice_presence_spring_median"
names(env_surface2)[42] <- "Silicate_spring_median"
names(env_surface2)[43] <- "Temp_summer_median"
names(env_surface2)[44] <- "Salinity_summer_median"
names(env_surface2)[45] <- "Nitrate_summer_median"
names(env_surface2)[46] <- "Ice_summer_median"
names(env_surface2)[47] <- "Ice_presence_summer_median"
names(env_surface2)[48] <- "Silicate_summer_median"
names(env_surface2)[49] <- "Temp_autumn_median"
names(env_surface2)[50] <- "Salinity_autumn_median"
names(env_surface2)[51] <- "Nitrate_autumn_median"
names(env_surface2)[52] <- "Ice_autumn_median"
names(env_surface2)[53] <- "Ice_presence_autumn_median"
names(env_surface2)[54] <- "Silicate_autumn_median"
names(env_surface2)[55] <- "Temp_winter_median"
names(env_surface2)[56] <- "Salinity_winter_median"
names(env_surface2)[57] <- "Nitrate_winter_median"
names(env_surface2)[58] <- "Ice_winter_median"
names(env_surface2)[59] <- "Ice_presence_winter_median"
names(env_surface2)[60] <- "Silicate_winter_median"
names(env_surface2)[61] <- "Temp_span_summer_winter_mean"
names(env_surface2)[62] <- "Temp_span_summer_winter_median"
names(env_surface2)[63] <- "Salinity_span_summer_winter_mean"
names(env_surface2)[64] <- "Salinity_span_summer_winter_median"
names(env_surface2)[65] <- "Nitrate_span_summer_winter_mean"
names(env_surface2)[66] <- "Nitrate_span_summer_winter_median"
names(env_surface2)[67] <- "Ice_span_summer_winter_mean"
names(env_surface2)[68] <- "Ice_span_summer_winter_median"
names(env_surface2)[69] <- "Ice_presence_span_summer_winter_mean"
names(env_surface2)[70] <- "Ice_presence_span_summer_winter_median"
names(env_surface2)[71] <- "Silicate_span_summer_winter_mean"
names(env_surface2)[72] <- "Silicate_span_summer_winter_median"
names(env_surface2)[73] <- "Long_Model"
names(env_surface2)[74] <- "Lat_Model"
names(env_surface2)[75] <- "Long_Dino"
names(env_surface2)[76] <- "Lat_Dino"

head(env_surface2)
str(env_surface1)

env_surface <- smartbind(env_surface1, env_surface2)

str(env_surface)

# 10.4 Order Locations ----

env_surface_order <- env_surface[order(env_surface$Lat_Dino),]
str(env_surface_order)


# 11. COMBINE SURFACE DATA WITH DINO DATA SET ----

str(env_surface_order)
#write.table(env_surface_order, file="env_surface_order.txt", row.names=FALSE, col.names=TRUE)

# example of how cluster solution looks like right now
str(sh21_9Cluster)


# do it for different cluster solutions
str(sh21_7Cluster)

# order them by lat as well
sh21_7Cluster_order <- sh21_7Cluster[order(sh21_7Cluster$Latitude),]
sh21_surface_7Cluster <- cbind(sh21_7Cluster_order, env_surface_order)
str(sh21_surface_7Cluster)


sh21_8Cluster_order <- sh21_8Cluster[order(sh21_8Cluster$Latitude),]
sh21_surface_8Cluster <- cbind(sh21_8Cluster_order, env_surface_order)
str(sh21_surface_8Cluster)

sh21_9Cluster_order <- sh21_9Cluster[order(sh21_9Cluster$Latitude),]
sh21_surface_9Cluster <- cbind(sh21_9Cluster_order, env_surface_order)
str(sh21_surface_9Cluster)
#write.table(sh21_surface_9Cluster, file="sh21_9Cluster_all.txt", row.names=FALSE, col.names=TRUE)

sh21_10Cluster_order <- sh21_10Cluster[order(sh21_10Cluster$Latitude),]
sh21_surface_10Cluster <- cbind(sh21_10Cluster_order, env_surface_order)
str(sh21_surface_10Cluster)

sh21_11Cluster_order <- sh21_11Cluster[order(sh21_11Cluster$Latitude),]
sh21_surface_11Cluster <- cbind(sh21_11Cluster_order, env_surface_order)
str(sh21_surface_11Cluster)


# 12. CALCULATING ENVIRONMENTAL PARAMETERS FOR EACH CLUSTER ----

# 12.1 For 7-cluster-solution ----

# split data set by clusters again
sh21_surface_7Cluster_split <- split(sh21_surface_7Cluster, sh21_surface_7Cluster$cluster)
str(sh21_surface_7Cluster_split)
sh21_surface_7Cluster1 <- sh21_surface_7Cluster_split$"1"
sh21_surface_7Cluster2 <- sh21_surface_7Cluster_split$"2"
sh21_surface_7Cluster3 <- sh21_surface_7Cluster_split$"3"
sh21_surface_7Cluster4 <- sh21_surface_7Cluster_split$"4"
sh21_surface_7Cluster5 <- sh21_surface_7Cluster_split$"5"
sh21_surface_7Cluster6 <- sh21_surface_7Cluster_split$"6"
sh21_surface_7Cluster7 <- sh21_surface_7Cluster_split$"7"


str(sh21_surface_7Cluster1)
sh21_surface_7Cluster1_ready_mean <- select(sh21_surface_7Cluster1, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster1_ready_median <- select(sh21_surface_7Cluster1, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster1_long_mean <- pivot_longer(sh21_surface_7Cluster1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster1_long_median <- pivot_longer(sh21_surface_7Cluster1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster1_long_mean)
str(sh21_surface_7Cluster1_long_median)

sh21_surface_7Cluster2_ready_mean <- select(sh21_surface_7Cluster2, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster2_ready_median <- select(sh21_surface_7Cluster2, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster2_long_mean <- pivot_longer(sh21_surface_7Cluster2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster2_long_median <- pivot_longer(sh21_surface_7Cluster2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster2_long_mean)
str(sh21_surface_7Cluster2_long_median)

sh21_surface_7Cluster3_ready_mean <- select(sh21_surface_7Cluster3, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster3_ready_median <- select(sh21_surface_7Cluster3, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster3_long_mean <- pivot_longer(sh21_surface_7Cluster3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster3_long_median <- pivot_longer(sh21_surface_7Cluster3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster3_long_mean)
str(sh21_surface_7Cluster3_long_median)

sh21_surface_7Cluster4_ready_mean <- select(sh21_surface_7Cluster4, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster4_ready_median <- select(sh21_surface_7Cluster4, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster4_long_mean <- pivot_longer(sh21_surface_7Cluster4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster4_long_median <- pivot_longer(sh21_surface_7Cluster4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster4_long_mean)
str(sh21_surface_7Cluster4_long_median)

sh21_surface_7Cluster5_ready_mean <- select(sh21_surface_7Cluster5, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster5_ready_median <- select(sh21_surface_7Cluster5, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster5_long_mean <- pivot_longer(sh21_surface_7Cluster5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster5_long_median <- pivot_longer(sh21_surface_7Cluster5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster5_long_mean)
str(sh21_surface_7Cluster5_long_median)

sh21_surface_7Cluster6_ready_mean <- select(sh21_surface_7Cluster6, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster6_ready_median <- select(sh21_surface_7Cluster6, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster6_long_mean <- pivot_longer(sh21_surface_7Cluster6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster6_long_median <- pivot_longer(sh21_surface_7Cluster6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster6_long_mean)
str(sh21_surface_7Cluster6_long_median)

sh21_surface_7Cluster7_ready_mean <- select(sh21_surface_7Cluster7, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_7Cluster7_ready_median <- select(sh21_surface_7Cluster7, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_7Cluster7_long_mean <- pivot_longer(sh21_surface_7Cluster7_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_7Cluster7_long_median <- pivot_longer(sh21_surface_7Cluster7_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_7Cluster7_long_mean)
str(sh21_surface_7Cluster7_long_median)

# 12.2 For 8-cluster-solution ----

# split data set by clusters again
sh21_surface_8Cluster_split <- split(sh21_surface_8Cluster, sh21_surface_8Cluster$cluster)
str(sh21_surface_8Cluster_split)
sh21_surface_8Cluster1 <- sh21_surface_8Cluster_split$"1"
sh21_surface_8Cluster2 <- sh21_surface_8Cluster_split$"2"
sh21_surface_8Cluster3 <- sh21_surface_8Cluster_split$"3"
sh21_surface_8Cluster4 <- sh21_surface_8Cluster_split$"4"
sh21_surface_8Cluster5 <- sh21_surface_8Cluster_split$"5"
sh21_surface_8Cluster6 <- sh21_surface_8Cluster_split$"6"
sh21_surface_8Cluster7 <- sh21_surface_8Cluster_split$"7"
sh21_surface_8Cluster8 <- sh21_surface_8Cluster_split$"8"


str(sh21_surface_8Cluster1)
sh21_surface_8Cluster1_ready_mean <- select(sh21_surface_8Cluster1, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster1_ready_median <- select(sh21_surface_8Cluster1, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster1_long_mean <- pivot_longer(sh21_surface_8Cluster1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster1_long_median <- pivot_longer(sh21_surface_8Cluster1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster1_long_mean)
str(sh21_surface_8Cluster1_long_median)

sh21_surface_8Cluster2_ready_mean <- select(sh21_surface_8Cluster2, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster2_ready_median <- select(sh21_surface_8Cluster2, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster2_long_mean <- pivot_longer(sh21_surface_8Cluster2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster2_long_median <- pivot_longer(sh21_surface_8Cluster2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster2_long_mean)
str(sh21_surface_8Cluster2_long_median)

sh21_surface_8Cluster3_ready_mean <- select(sh21_surface_8Cluster3, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster3_ready_median <- select(sh21_surface_8Cluster3, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster3_long_mean <- pivot_longer(sh21_surface_8Cluster3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster3_long_median <- pivot_longer(sh21_surface_8Cluster3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster3_long_mean)
str(sh21_surface_8Cluster3_long_median)

sh21_surface_8Cluster4_ready_mean <- select(sh21_surface_8Cluster4, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster4_ready_median <- select(sh21_surface_8Cluster4, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster4_long_mean <- pivot_longer(sh21_surface_8Cluster4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster4_long_median <- pivot_longer(sh21_surface_8Cluster4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster4_long_mean)
str(sh21_surface_8Cluster4_long_median)

sh21_surface_8Cluster5_ready_mean <- select(sh21_surface_8Cluster5, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster5_ready_median <- select(sh21_surface_8Cluster5, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster5_long_mean <- pivot_longer(sh21_surface_8Cluster5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster5_long_median <- pivot_longer(sh21_surface_8Cluster5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster5_long_mean)
str(sh21_surface_8Cluster5_long_median)

sh21_surface_8Cluster6_ready_mean <- select(sh21_surface_8Cluster6, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster6_ready_median <- select(sh21_surface_8Cluster6, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster6_long_mean <- pivot_longer(sh21_surface_8Cluster6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster6_long_median <- pivot_longer(sh21_surface_8Cluster6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster6_long_mean)
str(sh21_surface_8Cluster6_long_median)

sh21_surface_8Cluster7_ready_mean <- select(sh21_surface_8Cluster7, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster7_ready_median <- select(sh21_surface_8Cluster7, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster7_long_mean <- pivot_longer(sh21_surface_8Cluster7_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster7_long_median <- pivot_longer(sh21_surface_8Cluster7_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster7_long_mean)
str(sh21_surface_8Cluster7_long_median)

sh21_surface_8Cluster8_ready_mean <- select(sh21_surface_8Cluster8, contains("mean"), - contains(c("spring", "autumn", "span")), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_8Cluster8_ready_median <- select(sh21_surface_8Cluster8, contains("median"), - contains(c("spring", "autumn", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_8Cluster8_long_mean <- pivot_longer(sh21_surface_8Cluster8_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_8Cluster8_long_median <- pivot_longer(sh21_surface_8Cluster8_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_8Cluster8_long_mean)
str(sh21_surface_8Cluster8_long_median)




# 12.3 For 9-cluster-solution ----

sh21_surface_9Cluster_split <- split(sh21_surface_9Cluster, sh21_surface_9Cluster$cluster)
str(sh21_surface_9Cluster_split)
sh21_surface_9Cluster1 <- sh21_surface_9Cluster_split$"1"
sh21_surface_9Cluster2 <- sh21_surface_9Cluster_split$"2"
sh21_surface_9Cluster3 <- sh21_surface_9Cluster_split$"3"
sh21_surface_9Cluster4 <- sh21_surface_9Cluster_split$"4"
sh21_surface_9Cluster5 <- sh21_surface_9Cluster_split$"5"
sh21_surface_9Cluster6 <- sh21_surface_9Cluster_split$"6"
sh21_surface_9Cluster7 <- sh21_surface_9Cluster_split$"7"
sh21_surface_9Cluster8 <- sh21_surface_9Cluster_split$"8"
sh21_surface_9Cluster9 <- sh21_surface_9Cluster_split$"9"

str(sh21_surface_9Cluster1)
sh21_surface_9Cluster1_ready_mean <- select(sh21_surface_9Cluster1, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster1_ready_median <- select(sh21_surface_9Cluster1, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster1_ready_year_median <- select(sh21_surface_9Cluster1, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster1_long_mean <- pivot_longer(sh21_surface_9Cluster1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster1_long_median <- pivot_longer(sh21_surface_9Cluster1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster1_long_year_median <- pivot_longer(sh21_surface_9Cluster1_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster1_long_mean)
str(sh21_surface_9Cluster1_long_median)
str(sh21_surface_9Cluster1_long_year_median)

sh21_surface_9Cluster2_ready_mean <- select(sh21_surface_9Cluster2, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster2_ready_median <- select(sh21_surface_9Cluster2, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster2_ready_year_median <- select(sh21_surface_9Cluster2, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster2_long_mean <- pivot_longer(sh21_surface_9Cluster2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster2_long_median <- pivot_longer(sh21_surface_9Cluster2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster2_long_year_median <- pivot_longer(sh21_surface_9Cluster2_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster2_long_mean)
str(sh21_surface_9Cluster2_long_median)
str(sh21_surface_9Cluster2_long_year_median)

sh21_surface_9Cluster3_ready_mean <- select(sh21_surface_9Cluster3, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster3_ready_median <- select(sh21_surface_9Cluster3, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster3_ready_year_median <- select(sh21_surface_9Cluster3, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster3_long_mean <- pivot_longer(sh21_surface_9Cluster3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster3_long_median <- pivot_longer(sh21_surface_9Cluster3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster3_long_year_median <- pivot_longer(sh21_surface_9Cluster3_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster3_long_mean)
str(sh21_surface_9Cluster3_long_median)
str(sh21_surface_9Cluster3_long_year_median)

sh21_surface_9Cluster4_ready_mean <- select(sh21_surface_9Cluster4, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster4_ready_median <- select(sh21_surface_9Cluster4, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster4_ready_year_median <- select(sh21_surface_9Cluster4, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster4_long_mean <- pivot_longer(sh21_surface_9Cluster4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster4_long_median <- pivot_longer(sh21_surface_9Cluster4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster4_long_year_median <- pivot_longer(sh21_surface_9Cluster4_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster4_long_mean)
str(sh21_surface_9Cluster4_long_median)
str(sh21_surface_9Cluster4_long_year_median)

sh21_surface_9Cluster5_ready_mean <- select(sh21_surface_9Cluster5, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster5_ready_median <- select(sh21_surface_9Cluster5, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster5_ready_year_median <- select(sh21_surface_9Cluster5, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster5_long_mean <- pivot_longer(sh21_surface_9Cluster5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster5_long_median <- pivot_longer(sh21_surface_9Cluster5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster5_long_year_median <- pivot_longer(sh21_surface_9Cluster5_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster5_long_mean)
str(sh21_surface_9Cluster5_long_median)
str(sh21_surface_9Cluster5_long_year_median)

sh21_surface_9Cluster6_ready_mean <- select(sh21_surface_9Cluster6, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster6_ready_median <- select(sh21_surface_9Cluster6, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster6_ready_year_median <- select(sh21_surface_9Cluster6, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster6_long_mean <- pivot_longer(sh21_surface_9Cluster6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster6_long_median <- pivot_longer(sh21_surface_9Cluster6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster6_long_year_median <- pivot_longer(sh21_surface_9Cluster6_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster6_long_mean)
str(sh21_surface_9Cluster6_long_median)
str(sh21_surface_9Cluster6_long_year_median)

sh21_surface_9Cluster7_ready_mean <- select(sh21_surface_9Cluster7, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster7_ready_median <- select(sh21_surface_9Cluster7, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster7_ready_year_median <- select(sh21_surface_9Cluster7, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster7_long_mean <- pivot_longer(sh21_surface_9Cluster7_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster7_long_median <- pivot_longer(sh21_surface_9Cluster7_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster7_long_year_median <- pivot_longer(sh21_surface_9Cluster7_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster7_long_mean)
str(sh21_surface_9Cluster7_long_median)
str(sh21_surface_9Cluster7_long_year_median)


sh21_surface_9Cluster8_ready_mean <- select(sh21_surface_9Cluster8, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster8_ready_median <- select(sh21_surface_9Cluster8, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster8_ready_year_median <- select(sh21_surface_9Cluster8, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster8_long_mean <- pivot_longer(sh21_surface_9Cluster8_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster8_long_median <- pivot_longer(sh21_surface_9Cluster8_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster8_long_year_median <- pivot_longer(sh21_surface_9Cluster8_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster8_long_mean)
str(sh21_surface_9Cluster8_long_median)
str(sh21_surface_9Cluster8_long_year_median)

sh21_surface_9Cluster9_ready_mean <- select(sh21_surface_9Cluster9, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_9Cluster9_ready_median <- select(sh21_surface_9Cluster9, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster9_ready_year_median <- select(sh21_surface_9Cluster9, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_9Cluster9_long_mean <- pivot_longer(sh21_surface_9Cluster9_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster9_long_median <- pivot_longer(sh21_surface_9Cluster9_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_9Cluster9_long_year_median <- pivot_longer(sh21_surface_9Cluster9_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_9Cluster9_long_mean)
str(sh21_surface_9Cluster9_long_median)
str(sh21_surface_9Cluster9_long_year_median)

# 12.4 For 10-cluster-solution ----

sh21_surface_10Cluster_split <- split(sh21_surface_10Cluster, sh21_surface_10Cluster$cluster)
str(sh21_surface_10Cluster_split)

sh21_surface_10Cluster1 <- sh21_surface_10Cluster_split$"1"
sh21_surface_10Cluster2 <- sh21_surface_10Cluster_split$"2"
sh21_surface_10Cluster3 <- sh21_surface_10Cluster_split$"3"
sh21_surface_10Cluster4 <- sh21_surface_10Cluster_split$"4"
sh21_surface_10Cluster5 <- sh21_surface_10Cluster_split$"5"
sh21_surface_10Cluster6 <- sh21_surface_10Cluster_split$"6"
sh21_surface_10Cluster7 <- sh21_surface_10Cluster_split$"7"
sh21_surface_10Cluster8 <- sh21_surface_10Cluster_split$"8"
sh21_surface_10Cluster9 <- sh21_surface_10Cluster_split$"9"
sh21_surface_10Cluster10 <- sh21_surface_10Cluster_split$"10"


sh21_surface_10Cluster1_ready_mean <- select(sh21_surface_10Cluster1, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster1_ready_median <- select(sh21_surface_10Cluster1, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster1_ready_year_median <- select(sh21_surface_10Cluster1, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster1_long_mean <- pivot_longer(sh21_surface_10Cluster1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster1_long_median <- pivot_longer(sh21_surface_10Cluster1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster1_long_year_median <- pivot_longer(sh21_surface_10Cluster1_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster1_long_mean)
str(sh21_surface_10Cluster1_long_median)
str(sh21_surface_10Cluster1_long_year_median)

sh21_surface_10Cluster2_ready_mean <- select(sh21_surface_10Cluster2, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster2_ready_median <- select(sh21_surface_10Cluster2, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster2_ready_year_median <- select(sh21_surface_10Cluster2, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster2_long_mean <- pivot_longer(sh21_surface_10Cluster2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster2_long_median <- pivot_longer(sh21_surface_10Cluster2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster2_long_year_median <- pivot_longer(sh21_surface_10Cluster2_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster2_long_mean)
str(sh21_surface_10Cluster2_long_median)
str(sh21_surface_10Cluster2_long_year_median)

sh21_surface_10Cluster3_ready_mean <- select(sh21_surface_10Cluster3, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster3_ready_median <- select(sh21_surface_10Cluster3, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster3_ready_year_median <- select(sh21_surface_10Cluster3, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster3_long_mean <- pivot_longer(sh21_surface_10Cluster3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster3_long_median <- pivot_longer(sh21_surface_10Cluster3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster3_long_year_median <- pivot_longer(sh21_surface_10Cluster3_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster3_long_mean)
str(sh21_surface_10Cluster3_long_median)
str(sh21_surface_10Cluster3_long_year_median)

sh21_surface_10Cluster4_ready_mean <- select(sh21_surface_10Cluster4, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster4_ready_median <- select(sh21_surface_10Cluster4, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster4_ready_year_median <- select(sh21_surface_10Cluster4, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster4_long_mean <- pivot_longer(sh21_surface_10Cluster4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster4_long_median <- pivot_longer(sh21_surface_10Cluster4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster4_long_year_median <- pivot_longer(sh21_surface_10Cluster4_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster4_long_mean)
str(sh21_surface_10Cluster4_long_median)
str(sh21_surface_10Cluster4_long_year_median)

sh21_surface_10Cluster5_ready_mean <- select(sh21_surface_10Cluster5, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster5_ready_median <- select(sh21_surface_10Cluster5, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster5_ready_year_median <- select(sh21_surface_10Cluster5, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster5_long_mean <- pivot_longer(sh21_surface_10Cluster5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster5_long_median <- pivot_longer(sh21_surface_10Cluster5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster5_long_year_median <- pivot_longer(sh21_surface_10Cluster5_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster5_long_mean)
str(sh21_surface_10Cluster5_long_median)
str(sh21_surface_10Cluster5_long_year_median)

sh21_surface_10Cluster6_ready_mean <- select(sh21_surface_10Cluster6, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster6_ready_median <- select(sh21_surface_10Cluster6, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster6_ready_year_median <- select(sh21_surface_10Cluster6, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster6_long_mean <- pivot_longer(sh21_surface_10Cluster6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster6_long_median <- pivot_longer(sh21_surface_10Cluster6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster6_long_year_median <- pivot_longer(sh21_surface_10Cluster6_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster6_long_mean)
str(sh21_surface_10Cluster6_long_median)
str(sh21_surface_10Cluster6_long_year_median)

sh21_surface_10Cluster7_ready_mean <- select(sh21_surface_10Cluster7, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster7_ready_median <- select(sh21_surface_10Cluster7, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster7_ready_year_median <- select(sh21_surface_10Cluster7, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster7_long_mean <- pivot_longer(sh21_surface_10Cluster7_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster7_long_median <- pivot_longer(sh21_surface_10Cluster7_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster7_long_year_median <- pivot_longer(sh21_surface_10Cluster7_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster7_long_mean)
str(sh21_surface_10Cluster7_long_median)
str(sh21_surface_10Cluster7_long_year_median)


sh21_surface_10Cluster8_ready_mean <- select(sh21_surface_10Cluster8, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster8_ready_median <- select(sh21_surface_10Cluster8, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster8_ready_year_median <- select(sh21_surface_10Cluster8, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster8_long_mean <- pivot_longer(sh21_surface_10Cluster8_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster8_long_median <- pivot_longer(sh21_surface_10Cluster8_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster8_long_year_median <- pivot_longer(sh21_surface_10Cluster8_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster8_long_mean)
str(sh21_surface_10Cluster8_long_median)
str(sh21_surface_10Cluster8_long_year_median)

sh21_surface_10Cluster9_ready_mean <- select(sh21_surface_10Cluster9, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster9_ready_median <- select(sh21_surface_10Cluster9, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster9_ready_year_median <- select(sh21_surface_10Cluster9, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster9_long_mean <- pivot_longer(sh21_surface_10Cluster9_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster9_long_median <- pivot_longer(sh21_surface_10Cluster9_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster9_long_year_median <- pivot_longer(sh21_surface_10Cluster9_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster9_long_mean)
str(sh21_surface_10Cluster9_long_median)
str(sh21_surface_10Cluster9_long_year_median)

sh21_surface_10Cluster10_ready_mean <- select(sh21_surface_10Cluster10, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_10Cluster10_ready_median <- select(sh21_surface_10Cluster10, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster10_ready_year_median <- select(sh21_surface_10Cluster10, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_10Cluster10_long_mean <- pivot_longer(sh21_surface_10Cluster10_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster10_long_median <- pivot_longer(sh21_surface_10Cluster10_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_10Cluster10_long_year_median <- pivot_longer(sh21_surface_10Cluster10_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_10Cluster10_long_mean)
str(sh21_surface_10Cluster10_long_median)
str(sh21_surface_10Cluster10_long_year_median)

# 12.5 For 11-cluster-solution ----

sh21_surface_11Cluster_split <- split(sh21_surface_11Cluster, sh21_surface_11Cluster$cluster)
str(sh21_surface_11Cluster_split)

sh21_surface_11Cluster1 <- sh21_surface_11Cluster_split$"1"
sh21_surface_11Cluster2 <- sh21_surface_11Cluster_split$"2"
sh21_surface_11Cluster3 <- sh21_surface_11Cluster_split$"3"
sh21_surface_11Cluster4 <- sh21_surface_11Cluster_split$"4"
sh21_surface_11Cluster5 <- sh21_surface_11Cluster_split$"5"
sh21_surface_11Cluster6 <- sh21_surface_11Cluster_split$"6"
sh21_surface_11Cluster7 <- sh21_surface_11Cluster_split$"7"
sh21_surface_11Cluster8 <- sh21_surface_11Cluster_split$"8"
sh21_surface_11Cluster9 <- sh21_surface_11Cluster_split$"9"
sh21_surface_11Cluster10 <- sh21_surface_11Cluster_split$"10"
sh21_surface_11Cluster11 <- sh21_surface_11Cluster_split$"11"

sh21_surface_11Cluster1_ready_mean <- select(sh21_surface_11Cluster1, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster1_ready_median <- select(sh21_surface_11Cluster1, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster1_ready_year_median <- select(sh21_surface_11Cluster1, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster1_long_mean <- pivot_longer(sh21_surface_11Cluster1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster1_long_median <- pivot_longer(sh21_surface_11Cluster1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster1_long_year_median <- pivot_longer(sh21_surface_11Cluster1_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster1_long_mean)
str(sh21_surface_11Cluster1_long_median)
str(sh21_surface_11Cluster1_long_year_median)

sh21_surface_11Cluster2_ready_mean <- select(sh21_surface_11Cluster2, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster2_ready_median <- select(sh21_surface_11Cluster2, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster2_ready_year_median <- select(sh21_surface_11Cluster2, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster2_long_mean <- pivot_longer(sh21_surface_11Cluster2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster2_long_median <- pivot_longer(sh21_surface_11Cluster2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster2_long_year_median <- pivot_longer(sh21_surface_11Cluster2_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster2_long_mean)
str(sh21_surface_11Cluster2_long_median)
str(sh21_surface_11Cluster2_long_year_median)

sh21_surface_11Cluster3_ready_mean <- select(sh21_surface_11Cluster3, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster3_ready_median <- select(sh21_surface_11Cluster3, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster3_ready_year_median <- select(sh21_surface_11Cluster3, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster3_long_mean <- pivot_longer(sh21_surface_11Cluster3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster3_long_median <- pivot_longer(sh21_surface_11Cluster3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster3_long_year_median <- pivot_longer(sh21_surface_11Cluster3_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster3_long_mean)
str(sh21_surface_11Cluster3_long_median)
str(sh21_surface_11Cluster3_long_year_median)

sh21_surface_11Cluster4_ready_mean <- select(sh21_surface_11Cluster4, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster4_ready_median <- select(sh21_surface_11Cluster4, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster4_ready_year_median <- select(sh21_surface_11Cluster4, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster4_long_mean <- pivot_longer(sh21_surface_11Cluster4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster4_long_median <- pivot_longer(sh21_surface_11Cluster4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster4_long_year_median <- pivot_longer(sh21_surface_11Cluster4_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster4_long_mean)
str(sh21_surface_11Cluster4_long_median)
str(sh21_surface_11Cluster4_long_year_median)

sh21_surface_11Cluster5_ready_mean <- select(sh21_surface_11Cluster5, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster5_ready_median <- select(sh21_surface_11Cluster5, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster5_ready_year_median <- select(sh21_surface_11Cluster5, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster5_long_mean <- pivot_longer(sh21_surface_11Cluster5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster5_long_median <- pivot_longer(sh21_surface_11Cluster5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster5_long_year_median <- pivot_longer(sh21_surface_11Cluster5_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster5_long_mean)
str(sh21_surface_11Cluster5_long_median)
str(sh21_surface_11Cluster5_long_year_median)

sh21_surface_11Cluster6_ready_mean <- select(sh21_surface_11Cluster6, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster6_ready_median <- select(sh21_surface_11Cluster6, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster6_ready_year_median <- select(sh21_surface_11Cluster6, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster6_long_mean <- pivot_longer(sh21_surface_11Cluster6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster6_long_median <- pivot_longer(sh21_surface_11Cluster6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster6_long_year_median <- pivot_longer(sh21_surface_11Cluster6_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster6_long_mean)
str(sh21_surface_11Cluster6_long_median)
str(sh21_surface_11Cluster6_long_year_median)

sh21_surface_11Cluster7_ready_mean <- select(sh21_surface_11Cluster7, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster7_ready_median <- select(sh21_surface_11Cluster7, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster7_ready_year_median <- select(sh21_surface_11Cluster7, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster7_long_mean <- pivot_longer(sh21_surface_11Cluster7_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster7_long_median <- pivot_longer(sh21_surface_11Cluster7_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster7_long_year_median <- pivot_longer(sh21_surface_11Cluster7_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster7_long_mean)
str(sh21_surface_11Cluster7_long_median)
str(sh21_surface_11Cluster7_long_year_median)


sh21_surface_11Cluster8_ready_mean <- select(sh21_surface_11Cluster8, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster8_ready_median <- select(sh21_surface_11Cluster8, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster8_ready_year_median <- select(sh21_surface_11Cluster8, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster8_long_mean <- pivot_longer(sh21_surface_11Cluster8_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster8_long_median <- pivot_longer(sh21_surface_11Cluster8_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster8_long_year_median <- pivot_longer(sh21_surface_11Cluster8_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster8_long_mean)
str(sh21_surface_11Cluster8_long_median)
str(sh21_surface_11Cluster8_long_year_median)

sh21_surface_11Cluster9_ready_mean <- select(sh21_surface_11Cluster9, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster9_ready_median <- select(sh21_surface_11Cluster9, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster9_ready_year_median <- select(sh21_surface_11Cluster9, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster9_long_mean <- pivot_longer(sh21_surface_11Cluster9_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster9_long_median <- pivot_longer(sh21_surface_11Cluster9_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster9_long_year_median <- pivot_longer(sh21_surface_11Cluster9_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster9_long_mean)
str(sh21_surface_11Cluster9_long_median)
str(sh21_surface_11Cluster9_long_year_median)

sh21_surface_11Cluster10_ready_mean <- select(sh21_surface_11Cluster10, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster10_ready_median <- select(sh21_surface_11Cluster10, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster10_ready_year_median <- select(sh21_surface_11Cluster10, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster10_long_mean <- pivot_longer(sh21_surface_11Cluster10_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster10_long_median <- pivot_longer(sh21_surface_11Cluster10_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster10_long_year_median <- pivot_longer(sh21_surface_11Cluster10_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster10_long_mean)
str(sh21_surface_11Cluster10_long_median)
str(sh21_surface_11Cluster10_long_year_median)

sh21_surface_11Cluster11_ready_mean <- select(sh21_surface_11Cluster11, contains("mean"), - contains("span"), - Ice_mean, - Ice_summer_mean, - Ice_winter_mean)
sh21_surface_11Cluster11_ready_median <- select(sh21_surface_11Cluster11, contains("median"), - contains("span"), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster11_ready_year_median <- select(sh21_surface_11Cluster11, contains("median"), - contains(c("winter", "summer", "span")), - Ice_median, - Ice_summer_median, - Ice_winter_median)
sh21_surface_11Cluster11_long_mean <- pivot_longer(sh21_surface_11Cluster11_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster11_long_median <- pivot_longer(sh21_surface_11Cluster11_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
sh21_surface_11Cluster11_long_year_median <- pivot_longer(sh21_surface_11Cluster11_ready_year_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(sh21_surface_11Cluster11_long_mean)
str(sh21_surface_11Cluster11_long_median)
str(sh21_surface_11Cluster11_long_year_median)


# 13. CLUSTER WINTER SEA ICE ----
str(sh21_surface_9Cluster)

winter_seaice_edge <- filter(sh21_surface_9Cluster, Ice_presence_winter_median > 0)
str(winter_seaice_edge)
write.table(winter_seaice_edge, file = "Dataset winter sea ice.txt", row.names=FALSE, col.names=TRUE)

# Exclude columns that do not have an entry
winter_seaice_edge <- winter_seaice_edge[, colSums(winter_seaice_edge != 0) > 0]
str(winter_seaice_edge)

#spring_seaice_edge <- filter(sh21_surface_9Cluster, Ice_presence_spring_median > 0)
#summer_seaice_edge <- filter(sh21_surface_9Cluster, Ice_presence_summer_median > 0)
#autumn_seaice_edge <- filter(sh21_surface_9Cluster, Ice_presence_autumn_median > 0)

wsi_cluster <- select(winter_seaice_edge, contains("prop"))
str(wsi_cluster)
wsi_cluster[is.na(wsi_cluster)] = 0

# 13.1 DETERMINE # of Ks ----

# Elbow method
fviz_nbclust(wsi_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(wsi_cluster, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy, but recommended value nboot= 500 for analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(wsi_cluster, kmeans, nstart = 25,  method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")

# CHOOSING K 

k_wsi_cluster <- list ()
for (i in 1:20){
  k_wsi_cluster[[i]] <- kmeans(wsi_cluster, i, nstart = 100)
} 
k_wsi_cluster  

# look at ratios between_sum of squares/totalsumofsquares

betweenss_totss_wsi_cluster <- list()
for (i in 1:20){
  betweenss_totss_wsi_cluster[[i]] <- k_wsi_cluster[[i]]$betweenss/k_wsi_cluster[[i]]$totss
}  

plot(1:20, betweenss_totss_wsi_cluster, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)" )  


# 13.2 CLUSTERING ----
set.seed(1230)

fitK_wsi_3 <- kmeans(wsi_cluster, 3, iter.max = 1000, nstart = 100)
fitK_wsi_4 <- kmeans(wsi_cluster, 4, iter.max = 1000, nstart = 100)
fitK_wsi_5 <- kmeans(wsi_cluster, 5, iter.max = 1000, nstart = 100)
fitK_wsi_6 <- kmeans(wsi_cluster, 6, iter.max = 1000, nstart = 100)
fitK_wsi_7 <- kmeans(wsi_cluster, 7, iter.max = 1000, nstart = 100)

str(fitK_wsi_5)

# This means that R will try 100 different random starting assignments and then select the best results corresponding to the one with the lowest within cluster variation.

wsie_3Cluster <- cbind(winter_seaice_edge, fitK_wsi_3$cluster)
str(wsie_3Cluster)
wsie_3Cluster$`fitK_wsi_3$cluster` <- as.character(wsie_3Cluster$`fitK_wsi_3$cluster`)
names(wsie_3Cluster)[112] <- "wsie_cluster"
str(wsie_3Cluster)
head(wsie_3Cluster)

wsie_4Cluster <- cbind(winter_seaice_edge, fitK_wsi_4$cluster)
str(wsie_4Cluster)
wsie_4Cluster$`fitK_wsi_4$cluster` <- as.character(wsie_4Cluster$`fitK_wsi_4$cluster`)
names(wsie_4Cluster)[112] <- "wsie_cluster"
str(wsie_4Cluster)
head(wsie_4Cluster)

wsie_5Cluster <- cbind(winter_seaice_edge, fitK_wsi_5$cluster)
str(wsie_5Cluster)
wsie_5Cluster$`fitK_wsi_5$cluster` <- as.character(wsie_5Cluster$`fitK_wsi_5$cluster`)
names(wsie_5Cluster)[112] <- "wsie_cluster"
str(wsie_5Cluster)
head(wsie_5Cluster)

wsie_6Cluster <- cbind(winter_seaice_edge, fitK_wsi_6$cluster)
str(wsie_6Cluster)
wsie_6Cluster$`fitK_wsi_6$cluster` <- as.character(wsie_6Cluster$`fitK_wsi_6$cluster`)
names(wsie_6Cluster)[112] <- "wsie_cluster"
str(wsie_6Cluster)

wsie_7Cluster <- cbind(winter_seaice_edge, fitK_wsi_7$cluster)
str(wsie_7Cluster)
wsie_7Cluster$`fitK_wsi_7$cluster` <- as.character(wsie_7Cluster$`fitK_wsi_7$cluster`)
names(wsie_7Cluster)[112] <- "wsie_cluster"
str(wsie_7Cluster)

# 13.3 DINOS in CLUSTERS----

# 13.3.1 3-cluster-solution ----
wsie_3Cluster_split <- split(wsie_3Cluster, wsie_3Cluster$wsie_cluster)
str(wsie_3Cluster_split)

wsie_3Cluster1 <- wsie_3Cluster_split$"1"
wsie_3Cluster2 <- wsie_3Cluster_split$"2"
wsie_3Cluster3 <- wsie_3Cluster_split$"3"
str(wsie_3Cluster3)

wsie_3Cluster1_ready <- select(wsie_3Cluster1, contains("prop"))
wsie_3Cluster1_long <- pivot_longer(wsie_3Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_3Cluster2_ready <- select(wsie_3Cluster2, contains("prop"))
wsie_3Cluster2_long <- pivot_longer(wsie_3Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_3Cluster3_ready <- select(wsie_3Cluster3, contains("prop"))
wsie_3Cluster3_long <- pivot_longer(wsie_3Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 13.3.2 4-cluster-solution ----
wsie_4Cluster_split <- split(wsie_4Cluster, wsie_4Cluster$wsie_cluster)
str(wsie_4Cluster_split)

wsie_4Cluster1 <- wsie_4Cluster_split$"1"
wsie_4Cluster2 <- wsie_4Cluster_split$"2"
wsie_4Cluster3 <- wsie_4Cluster_split$"3"
wsie_4Cluster4 <- wsie_4Cluster_split$"4"
str(wsie_4Cluster4)

wsie_4Cluster1_ready <- select(wsie_4Cluster1, contains("prop"))
wsie_4Cluster1_long <- pivot_longer(wsie_4Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_4Cluster2_ready <- select(wsie_4Cluster2, contains("prop"))
wsie_4Cluster2_long <- pivot_longer(wsie_4Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_4Cluster3_ready <- select(wsie_4Cluster3, contains("prop"))
wsie_4Cluster3_long <- pivot_longer(wsie_4Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_4Cluster4_ready <- select(wsie_4Cluster4, contains("prop"))
wsie_4Cluster4_long <- pivot_longer(wsie_4Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 13.3.3 5-cluster-solution ----
wsie_5Cluster_split <- split(wsie_5Cluster, wsie_5Cluster$wsie_cluster)
str(wsie_5Cluster_split)

wsie_5Cluster1 <- wsie_5Cluster_split$"1"
wsie_5Cluster2 <- wsie_5Cluster_split$"2"
wsie_5Cluster3 <- wsie_5Cluster_split$"3"
wsie_5Cluster4 <- wsie_5Cluster_split$"4"
wsie_5Cluster5 <- wsie_5Cluster_split$"5"
str(wsie_5Cluster5)

wsie_5Cluster1_ready <- select(wsie_5Cluster1, contains("prop"))
wsie_5Cluster1_long <- pivot_longer(wsie_5Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_5Cluster2_ready <- select(wsie_5Cluster2, contains("prop"))
wsie_5Cluster2_long <- pivot_longer(wsie_5Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_5Cluster3_ready <- select(wsie_5Cluster3, contains("prop"))
wsie_5Cluster3_long <- pivot_longer(wsie_5Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_5Cluster4_ready <- select(wsie_5Cluster4, contains("prop"))
wsie_5Cluster4_long <- pivot_longer(wsie_5Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_5Cluster5_ready <- select(wsie_5Cluster5, contains("prop"))
wsie_5Cluster5_long <- pivot_longer(wsie_5Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 13.3.4 6-cluster-solution ----
wsie_6Cluster_split <- split(wsie_6Cluster, wsie_6Cluster$wsie_cluster)
str(wsie_6Cluster_split)

wsie_6Cluster1 <- wsie_6Cluster_split$"1"
wsie_6Cluster2 <- wsie_6Cluster_split$"2"
wsie_6Cluster3 <- wsie_6Cluster_split$"3"
wsie_6Cluster4 <- wsie_6Cluster_split$"4"
wsie_6Cluster5 <- wsie_6Cluster_split$"5"
wsie_6Cluster6 <- wsie_6Cluster_split$"6"
str(wsie_6Cluster1)

wsie_6Cluster1_ready <- select(wsie_6Cluster1, contains("prop"))
wsie_6Cluster1_long <- pivot_longer(wsie_6Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_6Cluster2_ready <- select(wsie_6Cluster2, contains("prop"))
wsie_6Cluster2_long <- pivot_longer(wsie_6Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_6Cluster3_ready <- select(wsie_6Cluster3, contains("prop"))
wsie_6Cluster3_long <- pivot_longer(wsie_6Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_6Cluster4_ready <- select(wsie_6Cluster4, contains("prop"))
wsie_6Cluster4_long <- pivot_longer(wsie_6Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_6Cluster5_ready <- select(wsie_6Cluster5, contains("prop"))
wsie_6Cluster5_long <- pivot_longer(wsie_6Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_6Cluster6_ready <- select(wsie_6Cluster6, contains("prop"))
wsie_6Cluster6_long <- pivot_longer(wsie_6Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )


# 13.3.5 7-cluster-solution ----
wsie_7Cluster_split <- split(wsie_7Cluster, wsie_7Cluster$wsie_cluster)
str(wsie_7Cluster_split)

wsie_7Cluster1 <- wsie_7Cluster_split$"1"
wsie_7Cluster2 <- wsie_7Cluster_split$"2"
wsie_7Cluster3 <- wsie_7Cluster_split$"3"
wsie_7Cluster4 <- wsie_7Cluster_split$"4"
wsie_7Cluster5 <- wsie_7Cluster_split$"5"
wsie_7Cluster6 <- wsie_7Cluster_split$"6"
wsie_7Cluster7 <- wsie_7Cluster_split$"7"
str(wsie_7Cluster1)

wsie_7Cluster1_ready <- select(wsie_7Cluster1, contains("prop"))
wsie_7Cluster1_long <- pivot_longer(wsie_7Cluster1_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster2_ready <- select(wsie_7Cluster2, contains("prop"))
wsie_7Cluster2_long <- pivot_longer(wsie_7Cluster2_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster3_ready <- select(wsie_7Cluster3, contains("prop"))
wsie_7Cluster3_long <- pivot_longer(wsie_7Cluster3_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster4_ready <- select(wsie_7Cluster4, contains("prop"))
wsie_7Cluster4_long <- pivot_longer(wsie_7Cluster4_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster5_ready <- select(wsie_7Cluster5, contains("prop"))
wsie_7Cluster5_long <- pivot_longer(wsie_7Cluster5_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster6_ready <- select(wsie_7Cluster6, contains("prop"))
wsie_7Cluster6_long <- pivot_longer(wsie_7Cluster6_ready, cols = everything (), names_to = "species", values_to = "percentage" )

wsie_7Cluster7_ready <- select(wsie_7Cluster7, contains("prop"))
wsie_7Cluster7_long <- pivot_longer(wsie_7Cluster7_ready, cols = everything (), names_to = "species", values_to = "percentage" )

##################################################################################

# 14. LATERAL TRANSPORT DATA ----

# 14.1 info  ----
# sinking speed: 25m/day
# I've taken out the Candel files from the output file folder
# I've taken out the 13 samples I have no dinos for
# 5 x the locations are the same and need to be doubled
# samples that had zeros in surface data have been slightly shiftes for these data as well 

# 14.2 read in data ----
# sites
list_of_transport_files1 <- list.files(path = "./Model_Transport1", recursive = TRUE,
                                       pattern = "\\.txt$", 
                                       full.names = TRUE)
str(list_of_transport_files1)
list_of_transport_files1
# extra sites 
list_of_transport_files2 <- list.files(path = "./Model_Transport2", recursive = TRUE,
                                       pattern = "\\.txt$", 
                                       full.names = TRUE)
list_of_transport_files2

# double sites
list_of_transport_files3 <- list.files(path = "./Model_Transport3", recursive = TRUE,
                                       pattern = "\\.txt$", 
                                       full.names = TRUE)
list_of_transport_files3
# double extra sites 
list_of_transport_files4 <- list.files(path = "./Model_Transport4", recursive = TRUE,
                                       pattern = "\\.txt$", 
                                       full.names = TRUE)
list_of_transport_files4

# 14.3 combine data ----

# 14.3.1 list 1 ----
#assuming tab separated values with a header    
transport_data_list1 <- lapply(list_of_transport_files1, function(x)read.table(x, header = TRUE)) 
str(transport_data_list1)
head(transport_data_list1)

#row_length_1 <- lapply(transport_data_list1, function(x)nrow(x))
#row_length_1
#write.table(row_length_1, file = "row_length_1.txt")

#assuming the same header/columns for all files
variables_transport1 <-  do.call("rbind", transport_data_list1) 
str(variables_transport1)
#variables_transport1$Latitude_10m
variables_transport1 <- select(variables_transport1, - Latitude_150m, -Longitude_150m, - Temperature_150m)
#variables_transport1 <- as.numeric(variables_transport1)
str(variables_transport1)

# we create a new column with a time step for each observation
Transport_Time1 <- seq(1, 1076, 1) %>% rep(times = 627)
str(Transport_Time1)
variables_transport_time1 <- cbind(variables_transport1, Transport_Time1)

# 14.3.2 list 2 ----
#assuming tab separated values with a header    
transport_data_list2 <- lapply(list_of_transport_files2, function(x)read.table(x, header = TRUE)) 
str(transport_data_list2)

row_length_2 <- lapply(transport_data_list2, function(x)nrow(x))
row_length_2

#assuming the same header/columns for all files
variables_transport2 <-  do.call("rbind", transport_data_list2) 
str(variables_transport2)
variables_transport2 <- select(variables_transport2, - Latitude_150m, -Longitude_150m, - Temperature_150m)
#variables_transport1 <- as.numeric(variables_transport1)
str(variables_transport2)
Transport_Time2 <- seq(1, 1052, 1) %>% rep(times = 16)
str(Transport_Time2)
variables_transport_time2 <- cbind(variables_transport2, Transport_Time2)

# 14.3.3 list 3 ----
#assuming tab separated values with a header    
transport_data_list3 <- lapply(list_of_transport_files3, function(x)read.table(x, header = TRUE)) 
str(transport_data_list3)

row_length_3 <- lapply(transport_data_list3, function(x)nrow(x))
row_length_3

#assuming the same header/columns for all files
variables_transport3 <-  do.call("rbind", transport_data_list3) 
str(variables_transport3)
variables_transport3 <- select(variables_transport3, - Latitude_150m, -Longitude_150m, - Temperature_150m)
#variables_transport1 <- as.numeric(variables_transport1)
str(variables_transport3)
Transport_Time3 <- seq(1, 1076, 1) %>% rep(times = 10)
str(Transport_Time3)
variables_transport_time3 <- cbind(variables_transport3, Transport_Time3)

# 14.3.4 list 4 ----
#assuming tab separated values with a header    
transport_data_list4 <- lapply(list_of_transport_files4, function(x)read.table(x, header = TRUE)) 
str(transport_data_list4)

row_length_4 <- lapply(transport_data_list4, function(x)nrow(x))
row_length_4

#assuming the same header/columns for all files
variables_transport4 <-  do.call("rbind", transport_data_list4) 
str(variables_transport4)
variables_transport4 <- select(variables_transport4, - Latitude_150m, -Longitude_150m, - Temperature_150m)
#variables_transport1 <- as.numeric(variables_transport1)
str(variables_transport4)
Transport_Time4 <- seq(1, 1052, 1) %>% rep(times = 2)
str(Transport_Time4)
variables_transport_time4 <- cbind(variables_transport4, Transport_Time4)


# 14.4 shorten data sets ----

# transport 1 and 2 need to be shortened to 3 years
# transport 3 and 4 need first only take every second row, then shorten to 3 years
#str(variables_transport_time1)
transport_surface1 <- filter(variables_transport_time1, Transport_Time1 <359)
str(transport_surface1)

transport_surface2 <- filter(variables_transport_time2, Transport_Time2 <359)
str(transport_surface2)

str(variables_transport_time3)
variables_transport_time3_ready <- variables_transport_time3[seq(1, nrow(variables_transport_time3), 2), ]
str(variables_transport_time3_ready)
transport_surface3 <- filter(variables_transport_time3_ready, Transport_Time3 <717)
Transport_Time3_new <- (seq(1, 358, 1) %>% rep(times = 10))
transport_surface3 <- cbind (transport_surface3,Transport_Time3_new)

str(variables_transport_time4)
variables_transport_time4_ready  = variables_transport_time4[seq(1, nrow(variables_transport_time4), 2), ]
str(variables_transport_time4_ready)
transport_surface4 <- filter(variables_transport_time4_ready, Transport_Time4 <717)
Transport_Time4_new <- (seq(1, 358, 1) %>% rep(times = 2))
transport_surface4 <- cbind (transport_surface4,Transport_Time4_new)

# 14.5 combine transport model data ----

# now we have 4 dataframes all three years long 
str(transport_surface1)
transport_surface1 <- select(transport_surface1, -Transport_Time1)
str(transport_surface2)
transport_surface2 <- select(transport_surface2, -Transport_Time2)
str(transport_surface3)
transport_surface3 <- select(transport_surface3, -Transport_Time3, -Transport_Time3_new)
str(transport_surface4)
transport_surface4 <- select(transport_surface4, -Transport_Time4, -Transport_Time4_new)

transport_all <- smartbind(transport_surface1, transport_surface2, transport_surface3, transport_surface4)
str(transport_all)


# 14.6 CALCULATE MEANS AND MEDIANS PER LOCATION ----
str(transport_all)

# SPLIT BY TRAJECTORY/DINO ORDER 
transport_all_split <- split(transport_all, transport_all$Site_number)
str(transport_all_split)

# Calculate annual means & medians 
Transport_Dino_Lat <- tapply(transport_all$Latitude_dinos, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Dino_Long <- tapply(transport_all$Longitude_dinos, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)

str(Transport_Dino_Lat)

# CALCULATE TRANSPORT MEAN AND MEDIAN---
Transport_Temp_mean <- tapply(transport_all$Temperature, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Salinity_mean <- tapply(transport_all$Salinity,transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Nitrate_mean <- tapply(transport_all$Nitrate, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Ice_mean <- tapply(transport_all$Ice, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Ice_presence_mean <- tapply(transport_all$Ice_presence, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)
Transport_Silicate_mean <- tapply(transport_all$Silicate, transport_all$Site_number, mean, na.rm = TRUE, simplify = TRUE)

Transport_Temp_median <- tapply(transport_all$Temperature, transport_all$Site_number, median, na.rm = TRUE, simplify = TRUE)
Transport_Salinity_median <- tapply(transport_all$Salinity, transport_all$Site_number, median, na.rm = TRUE, simplify = TRUE)
Transport_Nitrate_median <- tapply(transport_all$Nitrate, transport_all$Site_number, median, na.rm = TRUE, simplify = TRUE)
Transport_Ice_median <- tapply(transport_all$Ice, transport_all$Site_number, median, na.rm = TRUE, simplify = TRUE)
Transport_Ice_presence_median <- tapply(transport_all$Ice_presence, transport_all$Site_number,median, na.rm = TRUE, simplify = TRUE)
Transport_Silicate_median <- tapply(transport_all$Silicate, transport_all$Site_number, median, na.rm = TRUE, simplify = TRUE)


Transport_Temp_mean <- as.vector (Transport_Temp_mean)
Transport_Salinity_mean <- as.vector (Transport_Salinity_mean)
Transport_Nitrate_mean <- as.vector (Transport_Nitrate_mean)
Transport_Ice_mean <- as.vector (Transport_Ice_mean)
Transport_Ice_presence_mean <- as.vector (Transport_Ice_presence_mean)
Transport_Silicate_mean <- as.vector (Transport_Silicate_mean)

Transport_Temp_median <- as.vector (Transport_Temp_median)
Transport_Salinity_median <- as.vector (Transport_Salinity_median)
Transport_Nitrate_median <- as.vector (Transport_Nitrate_median)
Transport_Ice_median <- as.vector (Transport_Ice_median)
Transport_Ice_presence_median <- as.vector (Transport_Ice_presence_median)
Transport_Silicate_median <- as.vector (Transport_Silicate_median)

Transport_Parameters_all <- data.frame (Transport_Temp_mean,Transport_Salinity_mean, Transport_Nitrate_mean,
                                        Transport_Ice_mean, Transport_Ice_presence_mean, Transport_Silicate_mean,
                                        Transport_Temp_median,Transport_Salinity_median, Transport_Nitrate_median,
                                        Transport_Ice_median, Transport_Ice_presence_median, Transport_Silicate_median,
                                        Transport_Dino_Lat, Transport_Dino_Long)


# 15. COMBINE TRANSPORT AND DINO DATA ----

# 15.1 Order Transport data by Dino lat ----
str(Transport_Parameters_all)
#write.table(Transport_Parameters_all, file = "Transport_Parameters_all.txt")

transport_parameters <- Transport_Parameters_all[ with(Transport_Parameters_all, order(Transport_Dino_Lat, Transport_Dino_Long)),]
str(transport_parameters)
#write.table(transport_parameters, file = "Transport_Parameters_ordered.txt")


# 15.2 Order Dino data ----
# have a look at example dino cluster data set

str(sh21_9Cluster)

sh21_7Cluster_transport_order <- sh21_7Cluster[ with(sh21_7Cluster, order(Latitude, Longitude)),]
str(sh21_7Cluster_transport_order)

sh21_8Cluster_transport_order <- sh21_8Cluster[ with(sh21_8Cluster, order(Latitude, Longitude)),]
str(sh21_8Cluster_transport_order)

sh21_9Cluster_transport_order <- sh21_9Cluster[ with(sh21_9Cluster, order(Latitude, Longitude)),]
str(sh21_9Cluster_transport_order)

sh21_10Cluster_transport_order <- sh21_10Cluster[ with(sh21_10Cluster, order(Latitude, Longitude)),]
str(sh21_10Cluster_transport_order)

sh21_11Cluster_transport_order <- sh21_11Cluster[ with(sh21_11Cluster, order(Latitude, Longitude)),]
str(sh21_11Cluster_transport_order)


# 15.3 COMBINE ----

sh21_transport_7Cluster <- cbind(sh21_7Cluster_transport_order, transport_parameters)
str(sh21_transport_7Cluster)

sh21_transport_8Cluster <- cbind(sh21_8Cluster_transport_order, transport_parameters)
str(sh21_transport_8Cluster)

sh21_transport_9Cluster <- cbind(sh21_9Cluster_transport_order, transport_parameters)
str(sh21_transport_9Cluster)

sh21_transport_10Cluster <- cbind(sh21_10Cluster_transport_order, transport_parameters)
str(sh21_transport_10Cluster)

sh21_transport_11Cluster <- cbind(sh21_11Cluster_transport_order, transport_parameters)
str(sh21_transport_11Cluster)

# 15.4 Separate Clusters ----

transport_dinos_9clusters_split <- split(sh21_transport_9Cluster, sh21_transport_9Cluster$cluster)
str(transport_dinos_9clusters_split)

transport_dinos_9clusters_1 <- transport_dinos_9clusters_split$"1"
transport_dinos_9clusters_2 <- transport_dinos_9clusters_split$"2"
transport_dinos_9clusters_3 <- transport_dinos_9clusters_split$"3"
transport_dinos_9clusters_4 <- transport_dinos_9clusters_split$"4"
transport_dinos_9clusters_5 <- transport_dinos_9clusters_split$"5"
transport_dinos_9clusters_6 <- transport_dinos_9clusters_split$"6"
transport_dinos_9clusters_7 <- transport_dinos_9clusters_split$"7"
transport_dinos_9clusters_8 <- transport_dinos_9clusters_split$"8"
transport_dinos_9clusters_9 <- transport_dinos_9clusters_split$"9"

str(transport_dinos_9clusters_1)
transport_dinos_9clusters_1_ready_mean <- select(transport_dinos_9clusters_1, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")), - Transport_Ice_mean)
transport_dinos_9clusters_1_ready_median <- select(transport_dinos_9clusters_1, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_1_long_mean <- pivot_longer(transport_dinos_9clusters_1_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_1_long_median <- pivot_longer(transport_dinos_9clusters_1_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_1_long_mean)
str(transport_dinos_9clusters_1_long_median)

str(transport_dinos_9clusters_2)
transport_dinos_9clusters_2_ready_mean <- select(transport_dinos_9clusters_2, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_2_ready_median <- select(transport_dinos_9clusters_2, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_2_long_mean <- pivot_longer(transport_dinos_9clusters_2_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_2_long_median <- pivot_longer(transport_dinos_9clusters_2_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_2_long_mean)
str(transport_dinos_9clusters_2_long_median)

str(transport_dinos_9clusters_3)
transport_dinos_9clusters_3_ready_mean <- select(transport_dinos_9clusters_3, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_3_ready_median <- select(transport_dinos_9clusters_3, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_3_long_mean <- pivot_longer(transport_dinos_9clusters_3_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_3_long_median <- pivot_longer(transport_dinos_9clusters_3_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_3_long_mean)
str(transport_dinos_9clusters_3_long_median)

str(transport_dinos_9clusters_4)
transport_dinos_9clusters_4_ready_mean <- select(transport_dinos_9clusters_4, -Transport_Dino_Lat, -Transport_Dino_Long,-cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_4_ready_median <- select(transport_dinos_9clusters_4, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_4_long_mean <- pivot_longer(transport_dinos_9clusters_4_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_4_long_median <- pivot_longer(transport_dinos_9clusters_4_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_4_long_mean)
str(transport_dinos_9clusters_4_long_median)

str(transport_dinos_9clusters_5)
transport_dinos_9clusters_5_ready_mean <- select(transport_dinos_9clusters_5, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_5_ready_median <- select(transport_dinos_9clusters_5, -Transport_Dino_Lat, -Transport_Dino_Long,  -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_5_long_mean <- pivot_longer(transport_dinos_9clusters_5_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_5_long_median <- pivot_longer(transport_dinos_9clusters_5_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_5_long_mean)
str(transport_dinos_9clusters_5_long_median)

str(transport_dinos_9clusters_6)
transport_dinos_9clusters_6_ready_mean <- select(transport_dinos_9clusters_6, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_6_ready_median <- select(transport_dinos_9clusters_6, -Transport_Dino_Lat, -Transport_Dino_Long,  -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_6_long_mean <- pivot_longer(transport_dinos_9clusters_6_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_6_long_median <- pivot_longer(transport_dinos_9clusters_6_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_6_long_mean)
str(transport_dinos_9clusters_6_long_median)

str(transport_dinos_9clusters_7)
transport_dinos_9clusters_7_ready_mean <- select(transport_dinos_9clusters_7, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_7_ready_median <- select(transport_dinos_9clusters_7, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_7_long_mean <- pivot_longer(transport_dinos_9clusters_7_ready_mean, cols = ends_with("mean"), names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_7_long_median <- pivot_longer(transport_dinos_9clusters_7_ready_median, cols = ends_with("median"), names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_7_long_mean)
str(transport_dinos_9clusters_7_long_median)

str(transport_dinos_9clusters_8)
transport_dinos_9clusters_8_ready_mean <- select(transport_dinos_9clusters_8, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_8_ready_median <- select(transport_dinos_9clusters_8, -Transport_Dino_Lat, -Transport_Dino_Long,  -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_8_long_mean <- pivot_longer(transport_dinos_9clusters_8_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_8_long_median <- pivot_longer(transport_dinos_9clusters_8_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_8_long_mean)
str(transport_dinos_9clusters_8_long_median)

str(transport_dinos_9clusters_9)
transport_dinos_9clusters_9_ready_mean <- select(transport_dinos_9clusters_9, -Transport_Dino_Lat, -Transport_Dino_Long, -cluster, - ends_with(c("prop", "median")),  - Transport_Ice_mean)
transport_dinos_9clusters_9_ready_median <- select(transport_dinos_9clusters_9, -Transport_Dino_Lat, -Transport_Dino_Long,  -cluster, - ends_with(c("prop", "mean")), - Transport_Ice_median)
transport_dinos_9clusters_9_long_mean <- pivot_longer(transport_dinos_9clusters_9_ready_mean, cols = ends_with("mean") , names_to = "env_parameter", values_to = "values")
transport_dinos_9clusters_9_long_median <- pivot_longer(transport_dinos_9clusters_9_ready_median, cols = ends_with("median") , names_to = "env_parameter", values_to = "values")
str(transport_dinos_9clusters_9_long_mean)
str(transport_dinos_9clusters_9_long_median)


















# 16. CLUSTER TRANSPORT MAPS ----

str(transport_parameters)
str(transport_all)
transport_all_order <- transport_all[ with(transport_all, order(Latitude_dinos, Longitude_dinos)),]
str(transport_all_order)

str(sh21_9Cluster_transport_order)
cluster_9Cluster_prep <- sh21_9Cluster_transport_order$cluster
str(cluster_9Cluster_prep)

cluster_9Cluster_prep_ready <- rep(cluster_9Cluster_prep, each = 358)
str(cluster_9Cluster_prep_ready)

transport_map <- data.frame(transport_all_order,cluster_9Cluster_prep_ready)
str(transport_map)
names(transport_map)[12] <- "clusters"

transport_map_split <- split(transport_map, transport_map$clusters)
str(transport_map_split)

transport_map_cluster_1 <- transport_map_split$"1"
transport_map_cluster_2 <- transport_map_split$"2"
transport_map_cluster_3 <- transport_map_split$"3"
transport_map_cluster_4 <- transport_map_split$"4"
transport_map_cluster_5 <- transport_map_split$"5"
transport_map_cluster_6 <- transport_map_split$"6"
transport_map_cluster_7 <- transport_map_split$"7"
transport_map_cluster_8 <- transport_map_split$"8"
transport_map_cluster_9 <- transport_map_split$"9"

str(transport_map_cluster_1)

################################################################################# ----




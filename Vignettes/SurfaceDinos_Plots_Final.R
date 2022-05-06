# 1. Overview map----

world <- map_data("world")
head(world)
sh <- filter(world, lat < -0)
so <- filter(world, lat < -40)

# 1.1 SOUTHERN HEMISPHERE ----
# data sets we have
str(marret19_sh) # previously published in Marret et al., 2019
str(thole21) # newly counted
str(thole21_deleted_start) # newly counted but deleted (too low dino counts)

dinomap_all <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "green", alpha = 0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data = marret19_sh, aes(x=Longitude, y=Latitude), col = "grey",  size = 1.0) +
  geom_point(data = thole21_deleted_start, aes(x=Longitude, y=Latitude), shape = 21, col = "black", fill = "white", size = 1.5) + 
  geom_point(data = thole21, aes(x=Longitude, y=Latitude), col = "black", size = 1.5) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#dinomap_all
#ggsave(filename = "Fig2_Map_DinoSamples.pdf", width = 20, height = 20, unit = "cm")

# 1.2 NEW SAMPLES ----
str(thole21) # newly counted
dinomap_new <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "green", alpha = 0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data = thole21, aes(x=Longitude, y=Latitude), col = "black", size = 1.5) + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
#dinomap_new
#ggsave(filename = "Fig3_Map_NewDinoSamples.pdf", width = 20, height = 20, unit = "cm")


# 2. Counts and Concentration ----

thole21_concen <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data = thole21_conc, aes(x=Longitude, y=Latitude, size = dino_conc, col = dino_sum)) +
  scale_color_viridis(discrete = FALSE, limits = c(0, 220),option = "C") + 
  theme_bw() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank())
#thole21_concen

thole21_pglaconc <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=thole21_conc, aes(x=Longitude, y=Latitude, col = dino_sum, size = Pgla_conc)) +
  scale_color_viridis(discrete = FALSE, limits = c(0, 220), option = "C") + 
  theme_bw() +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),axis.text.x = element_blank(),
        axis.text.y = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank())
#thole21_pglaconc

plot_thole21_conc <- plot_grid(thole21_concen, thole21_pglaconc, align = "v", ncol = 1 )
plot_thole21_conc
#ggsave( filename = "SFig3_thole21_dinoConc_PglaConc.pdf", width = 20, height = 25, unit = "cm")

# 3. Dino Assemblages ----

thole21_assemblage <- select (thole21_all, -Latitude,  - Longitude, - sample, - dino_sum)
str(thole21_assemblage)

thole21_assemblage_long <- pivot_longer(thole21_assemblage, cols = !Station, names_to = "species", values_to = "percentage")
str(thole21_assemblage_long)
thole21_assemblage_long$species <- factor(thole21_assemblage_long$species, levels = c("Sspp_prop", "Sram_prop", "Smir_prop", "Pret_prop", "Ocen_prop","Lmac_prop", 
                                                                                      "Nlab_prop","Dcha_prop", "Ivel_prop", "Isph_prop", "Istr_prop", "Ipat_prop", 
                                                                                      "Ipar_prop", "Iacu_prop", "Ipal_prop", "Gymn_prop", "Lspp_prop", "Char_prop",
                                                                                      "Pdal_prop", "Btep_prop", "Squa_prop", "Snep_prop", "Bspp_prop", "Cmer_prop", 
                                                                                      "Imin_prop", "Numb_prop", "Sant_2_prop", "Sant_prop"))
thole21_assemblage_long <- mutate(thole21_assemblage_long, facet_label = fct_recode(
  species,
  "S. antarctica" = "Sant_prop", "S. antarctica sp. 2" = "Sant_2_prop", "N. umbiliphora" = "Numb_prop",
  "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
  "S. nephroides" = "Snep_prop", "P. dalei" = "Pdal_prop", "S. quanta" = "Squa_prop",
  "C. harlandii " = "Char_prop", "Lejeunecysta spp." = "Lspp_prop", "Gymnodinium spp." = "Gymn_prop",
  "I. pallidum" = "Ipal_prop", "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop",
  "I. patulum" = "Ipat_prop", "I. velorum" = "Ivel_prop", "N. labyrinthus" = "Nlab_prop",
  "L. machaerophorum" = "Lmac_prop", "O. centrocarpum" = "Ocen_prop", "P. reticulata" = "Pret_prop",
  "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop", 
  "D. chatamensis"= "Dcha_prop", "I. sphaericum" = "Isph_prop", "I. strialatum" = "Istr_prop", "B. tepikiense" = "Btep_prop"))
thole21_assemblage_long_main <- filter(thole21_assemblage_long, percentage > 0.05)

fig3_thole21_assemblages <- ggplot(thole21_assemblage_long, aes(x = percentage, y = Station, fill = facet_label)) + 
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_viridis(discrete = TRUE, direction = 1, option = "A")
#fig3_thole21_assemblages
#ggsave(plot = fig3_thole21_assemblages, filename = "Fig3_b_AssemblagesNewSamples.pdf", width = 25, height = 30, unit = "cm")


# 4. SH21 DINO CLUSTERING  ----

# 4.1 Cluster maps ----

sh21_7clustering_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster), pch = 21, size = 1) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map
#ggsave( filename = "SH21_7clustering.pdf", width = 8, height = 8, unit = "cm")

sh21_8clustering_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map
#ggsave( filename = "SH21_8clustering.pdf", width = 20, height = 20, unit = "cm")

str(sh21_9Cluster)

# give names to the clusters, adapt if necessary
sh21_9Cluster <- mutate(sh21_9Cluster, facet_label = fct_recode(
  cluster, "high-Ocen" = "1","Sant" = "2","high-Brig" = "3","low-Brig" = "4","Imin" = "5",
  "low-Ocen" = "6","Iacu" = "7","Nlab" = "8","Spin" = "9"))

sh21_9clustering_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map
#ggsave( filename = "SH21_9clustering.pdf", width = 20, height = 20, unit = "cm")

sh21_9Cluster$facet = factor(sh21_9Cluster$cluster, levels = c("2", "5", "3", "8", "4", "6","7", "1", "9"))
sh21_9Cluster <- mutate(sh21_9Cluster, facet_label = fct_recode(
  facet, "high-Ocen" = "1","Sant" = "2","high-Brig" = "3","low-Brig" = "4","Imin" = "5",
  "low-Ocen" = "6","Iacu" = "7","Nlab" = "8","Spin" = "9"))

sh21_9clustering_map_ordered <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
sh21_9clustering_map_ordered
ggsave( filename = "Fig4b_SH21_9clustering.pdf", width = 20, height = 20, unit = "cm")


sh21_10clustering_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_10clustering_map
#ggsave( filename = "SH21_10clustering.pdf", width = 20, height = 20, unit = "cm")

sh21_11clustering_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() + 
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_11clustering_map
#ggsave( filename = "SH21_10clustering.pdf", width = 20, height = 20, unit = "cm")

# 4.2 DINO ASSEMBLAGES IN CLUSTERS ----

# 4.2.1 7-cluster-solution ----
plot_sh21_7clusters_1_dinos <- ggplot(sh21_7Cluster1_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_1_dinos
#ggsave(plot = plot_sh21_7clusters_1_dinos, filename = "plot_sh21_7clusters_1_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_2_dinos <- ggplot(sh21_7Cluster2_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_2_dinos
#ggsave(plot = plot_sh21_7clusters_2_dinos, filename = "plot_sh21_7clusters_2_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_3_dinos <- ggplot(sh21_7Cluster3_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_3_dinos
#ggsave(plot = plot_sh21_7clusters_3_dinos, filename = "plot_sh21_7clusters_3_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_4_dinos <- ggplot(sh21_7Cluster4_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_4_dinos
#ggsave(plot = plot_sh21_7clusters_4_dinos, filename = "plot_sh21_7clusters_4_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_5_dinos <- ggplot(sh21_7Cluster5_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_5_dinos
#ggsave(plot = plot_sh21_7clusters_5_dinos, filename = "sh21_7clusters_5_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_6_dinos <- ggplot(sh21_7Cluster6_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_6_dinos
#ggsave(plot = plot_sh21_7clusters_6_dinos, filename = "plot_sh21_7clusters_6_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_7clusters_7_dinos <- ggplot(sh21_7Cluster7_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_7clusters_7_dinos
#ggsave(plot = plot_sh21_7clusters_7_dinos, filename = "sh21_7clusters_7_dinos.pdf", width = 30, height = 15, unit = "cm")

# 4.2.2 8-cluster-solution ----
plot_sh21_8clusters_1_dinos <- ggplot(sh21_8Cluster1_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_1_dinos
#ggsave(plot = plot_sh21_8clusters_1_dinos, filename = "plot_sh21_8clusters_1_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_2_dinos <- ggplot(sh21_8Cluster2_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_2_dinos
#ggsave(plot = plot_sh21_8clusters_2_dinos, filename = "plot_sh21_8clusters_2_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_3_dinos <- ggplot(sh21_8Cluster3_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_3_dinos
#ggsave(plot = plot_sh21_8clusters_3_dinos, filename = "plot_sh21_8clusters_3_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_4_dinos <- ggplot(sh21_8Cluster4_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_4_dinos
#ggsave(plot = plot_sh21_8clusters_4_dinos, filename = "plot_sh21_8clusters_4_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_5_dinos <- ggplot(sh21_8Cluster5_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_5_dinos
#ggsave(plot = plot_sh21_8clusters_5_dinos, filename = "plot_sh21_8clusters_5_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_6_dinos <- ggplot(sh21_8Cluster6_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_6_dinos
#ggsave(plot = plot_sh21_8clusters_6_dinos, filename = "plot_sh21_8clusters_6_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_7_dinos <- ggplot(sh21_8Cluster7_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_7_dinos
#ggsave(plot = plot_sh21_8clusters_7_dinos, filename = "plot_sh21_8clusters_7_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_8clusters_8_dinos <- ggplot(sh21_8Cluster8_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
#plot_sh21_8clusters_8_dinos
#ggsave(plot = plot_sh21_8clusters_8_dinos, filename = "plot_sh21_8clusters_8_dinos.pdf", width = 30, height = 15, unit = "cm")



# 4.2.3 9-cluster-solution ----

# bring dinos to percentages and order dinos
str(sh21_9Cluster1_long)

# make sure to do this only one time

sh21_9Cluster1_long$species <- factor(sh21_9Cluster1_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster2_long$species <- factor(sh21_9Cluster2_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster3_long$species <- factor(sh21_9Cluster3_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster4_long$species <- factor(sh21_9Cluster4_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster5_long$species <- factor(sh21_9Cluster5_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster6_long$species <- factor(sh21_9Cluster6_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster7_long$species <- factor(sh21_9Cluster7_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster8_long$species <- factor(sh21_9Cluster8_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
sh21_9Cluster9_long$species <- factor(sh21_9Cluster9_long$species, 
                                      levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_prop", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
# PLOT DINOS 
plot_sh21_9clusters_1_dinos <- ggplot(sh21_9Cluster1_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  labs(x = "Dinocyst species", y = "Median speices contribution in total assemblage (%)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))
plot_sh21_9clusters_1_dinos
ggsave(plot = plot_sh21_9clusters_1_dinos, filename = "plot_sh21_9clusters_1_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_2_dinos <- ggplot(sh21_9Cluster2_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  labs(x = "Dinocyst species", y = "Median speices contribution in total assemblage (%)") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))
plot_sh21_9clusters_2_dinos
ggsave(plot = plot_sh21_9clusters_2_dinos, filename = "plot_sh21_9clusters_2_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_3_dinos <- ggplot(sh21_9Cluster3_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_3_dinos
ggsave(plot = plot_sh21_9clusters_3_dinos, filename = "plot_sh21_9clusters_3_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_4_dinos <- ggplot(sh21_9Cluster4_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_4_dinos
ggsave(plot = plot_sh21_9clusters_4_dinos, filename = "plot_sh21_9clusters_4_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_5_dinos <- ggplot(sh21_9Cluster5_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_5_dinos
ggsave(plot = plot_sh21_9clusters_5_dinos, filename = "plot_sh21_9clusters_5_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_6_dinos <- ggplot(sh21_9Cluster6_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_6_dinos
ggsave(plot = plot_sh21_9clusters_6_dinos, filename = "plot_sh21_9clusters_6_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_7_dinos <- ggplot(sh21_9Cluster7_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_7_dinos
ggsave(plot = plot_sh21_9clusters_7_dinos, filename = "plot_sh21_9clusters_7_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_8_dinos <- ggplot(sh21_9Cluster8_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_8_dinos
ggsave(plot = plot_sh21_9clusters_8_dinos, filename = "plot_sh21_9clusters_8_dinos.pdf", width = 30, height = 15, unit = "cm")

plot_sh21_9clusters_9_dinos <- ggplot(sh21_9Cluster9_long, aes(x = species, y = percentage, fill = species)) + 
  geom_boxplot(col = "black")
plot_sh21_9clusters_9_dinos
ggsave(plot = plot_sh21_9clusters_9_dinos, filename = "plot_sh21_9clusters_9_dinos.pdf", width = 30, height = 15, unit = "cm")

# 4.2.4 9-cluster-solution, LESS Dinos ----

# PLOT NAMES ----
# "high-Ocen" = "1", Sant" = "2", "high-Brig" = "3", 
# "low-Brig" = "4", Imin" = "5", "low-Ocen" = "6", 
# "Iacu" = "7", "Nlab" = "8", "Spin" = "9"

# first cluster ----
str(sh21_9Cluster1_short_long)
sh21_9Cluster1_short_long$percentage <- sh21_9Cluster1_short_long$percentage * 100
sh21_9Cluster1_short_long$species <- factor(sh21_9Cluster1_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster1_short_long <- mutate(sh21_9Cluster1_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus" = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

plot_sh21_9clusters_1_dinos_short <- ggplot(sh21_9Cluster1_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "high-Ocen-Cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_1_dinos_short
#ggsave(plot = plot_sh21_9clusters_1_dinos_short, filename = "plot_sh21_9clusters_1_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# second cluster ----
sh21_9Cluster2_short_long$percentage <- sh21_9Cluster2_short_long$percentage * 100
str(sh21_9Cluster2_short_long)
sh21_9Cluster2_short_long$species <- factor(sh21_9Cluster2_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster2_short_long <- mutate(sh21_9Cluster2_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_2_dinos_short <- ggplot(sh21_9Cluster2_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Sant-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_sh21_9clusters_2_dinos_short
ggsave(plot = plot_sh21_9clusters_2_dinos_short, filename = "plot_sh21_9clusters_2_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# third cluster ----
sh21_9Cluster3_short_long$percentage <- sh21_9Cluster3_short_long$percentage * 100
str(sh21_9Cluster3_short_long)
sh21_9Cluster3_short_long$species <- factor(sh21_9Cluster3_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster3_short_long <- mutate(sh21_9Cluster3_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_3_dinos_short <- ggplot(sh21_9Cluster3_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "high-Brig-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_3_dinos_short
#ggsave(plot = plot_sh21_9clusters_3_dinos_short, filename = "plot_sh21_9clusters_3_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# forth cluster ----
sh21_9Cluster4_short_long$percentage <- sh21_9Cluster4_short_long$percentage * 100
str(sh21_9Cluster4_short_long)
sh21_9Cluster4_short_long$species <- factor(sh21_9Cluster4_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster4_short_long <- mutate(sh21_9Cluster4_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_4_dinos_short <- ggplot(sh21_9Cluster4_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "low-Brig-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_4_dinos_short
#ggsave(plot = plot_sh21_9clusters_4_dinos_short, filename = "plot_sh21_9clusters_4_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# fifth cluster ----
sh21_9Cluster5_short_long$percentage <- sh21_9Cluster5_short_long$percentage * 100
str(sh21_9Cluster5_short_long)
sh21_9Cluster5_short_long$species <- factor(sh21_9Cluster5_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster5_short_long <- mutate(sh21_9Cluster5_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_5_dinos_short <- ggplot(sh21_9Cluster5_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Imin-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_5_dinos_short
#ggsave(plot = plot_sh21_9clusters_5_dinos_short, filename = "plot_sh21_9clusters_5_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# sixth cluster ----
sh21_9Cluster6_short_long$percentage <- sh21_9Cluster6_short_long$percentage * 100
str(sh21_9Cluster6_short_long)
sh21_9Cluster6_short_long$species <- factor(sh21_9Cluster6_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster6_short_long <- mutate(sh21_9Cluster6_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_6_dinos_short <- ggplot(sh21_9Cluster6_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "low-Ocen-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_6_dinos_short
#ggsave(plot = plot_sh21_9clusters_6_dinos_short, filename = "plot_sh21_9clusters_6_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# seventh cluster ----
sh21_9Cluster7_short_long$percentage <- sh21_9Cluster7_short_long$percentage * 100
str(sh21_9Cluster7_short_long)
sh21_9Cluster7_short_long$species <- factor(sh21_9Cluster7_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster7_short_long <- mutate(sh21_9Cluster7_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_7_dinos_short <- ggplot(sh21_9Cluster7_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Iacu-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_7_dinos_short
#ggsave(plot = plot_sh21_9clusters_7_dinos_short, filename = "plot_sh21_9clusters_7_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# eighth cluster ----
sh21_9Cluster8_short_long$percentage <- sh21_9Cluster8_short_long$percentage * 100
str(sh21_9Cluster8_short_long)
sh21_9Cluster8_short_long$species <- factor(sh21_9Cluster8_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster8_short_long <- mutate(sh21_9Cluster8_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_8_dinos_short <- ggplot(sh21_9Cluster8_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Nlab-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_8_dinos_short
#ggsave(plot = plot_sh21_9clusters_8_dinos_short, filename = "plot_sh21_9clusters_8_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# ninth cluster ----
sh21_9Cluster9_short_long$percentage <- sh21_9Cluster9_short_long$percentage * 100
str(sh21_9Cluster9_short_long)
sh21_9Cluster9_short_long$species <- factor(sh21_9Cluster9_short_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Nlab_prop", "Ocen_prop", "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop",  "Sspp_prop"))
sh21_9Cluster9_short_long <- mutate(sh21_9Cluster9_short_long, facet_label = fct_recode(species,
                                                                                        "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", 
                                                                                        "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop", "S. nephroides" = "Snep_prop", 
                                                                                        "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop","I. pallidum" = "Ipal_prop", 
                                                                                        "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. sphaericum" = "Isph_prop",
                                                                                        "I. patulum" = "Ipat_prop", "N. labyrinthus" = "Nlab_prop","O. centrocarpum" = "Ocen_prop", 
                                                                                        "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop","S. mirabilis" = "Smir_prop", 
                                                                                        "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))


plot_sh21_9clusters_9_dinos_short <- ggplot(sh21_9Cluster9_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = "Spin-cluster",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_sh21_9clusters_9_dinos_short
#ggsave(plot = plot_sh21_9clusters_9_dinos_short, filename = "plot_sh21_9clusters_9_dinos_short.pdf", width = 30, height = 15, unit = "cm")

# combine all plots (in order) ----
plot_sh21_9clusters_dinos_short <- plot_grid(plot_sh21_9clusters_2_dinos_short, plot_sh21_9clusters_5_dinos_short, plot_sh21_9clusters_3_dinos_short, 
                                             plot_sh21_9clusters_8_dinos_short, plot_sh21_9clusters_4_dinos_short, plot_sh21_9clusters_6_dinos_short, 
                                             plot_sh21_9clusters_7_dinos_short, plot_sh21_9clusters_1_dinos_short, plot_sh21_9clusters_9_dinos_short,  
                                        align = "v", ncol = 3)
#plot_sh21_9clusters_dinos_short
#ggsave(plot = plot_sh21_9clusters_dinos_short, filename = "Fig4b_sh21_9clusters_dinos_short.pdf", width = 50, height = 30, unit = "cm")


# just plot the legend to add it in illustrator later on
plot_sh21_9clusters_species_legend <- ggplot(sh21_9Cluster9_short_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black") +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw()
plot_sh21_9clusters_species_legend
ggsave(plot = plot_sh21_9clusters_species_legend, filename = "Fig4b_sh21_9clusters_species_legend.pdf", width = 10, height = 10, unit = "cm")


# 4.2.5 10-cluster-solution ----

sh21_10Cluster1_long$species <- factor(sh21_10Cluster1_long$species,
                                             levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_1_dinos <- ggplot(sh21_10Cluster1_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_1_dinos
#ggsave(plot = plot_sh21_10clusters_1_dinos, filename = "plot_sh21_10clusters_1_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster2_long$species <- factor(sh21_10Cluster2_long$species,
                                             levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_2_dinos <- ggplot(sh21_10Cluster2_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_2_dinos
#ggsave(plot = plot_sh21_10clusters_2_dinos, filename = "plot_sh21_10clusters_2_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster3_long$species <- factor(sh21_10Cluster3_long$species,
                                             levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_3_dinos <- ggplot(sh21_10Cluster3_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_3_dinos
#ggsave(plot = plot_sh21_10clusters_3_dinos, filename = "plot_sh21_10clusters_3_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster4_long$species <- factor(sh21_10Cluster4_long$species,
                                             levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_4_dinos <- ggplot(sh21_10Cluster4_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_4_dinos
#ggsave(plot = plot_sh21_10clusters_4_dinos, filename = "plot_sh21_10clusters_4_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster5_long$species <- factor(sh21_10Cluster5_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_5_dinos <- ggplot(sh21_10Cluster5_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_5_dinos
#ggsave(plot = plot_sh21_10clusters_5_dinos, filename = "plot_sh21_10clusters_5_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster6_long$species <- factor(sh21_10Cluster6_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_6_dinos <- ggplot(sh21_10Cluster6_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_6_dinos
#ggsave(plot = plot_sh21_10clusters_6_dinos, filename = "plot_sh21_10clusters_6_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster7_long$species <- factor(sh21_10Cluster7_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_7_dinos <- ggplot(sh21_10Cluster7_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_7_dinos
#ggsave(plot = plot_sh21_10clusters_7_dinos, filename = "plot_sh21_10clusters_7_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster8_long$species <- factor(sh21_10Cluster8_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_8_dinos <- ggplot(sh21_10Cluster8_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_8_dinos
#ggsave(plot = plot_sh21_10clusters_8_dinos, filename = "plot_sh21_10clusters_8_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster9_long$species <- factor(sh21_10Cluster9_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_9_dinos <- ggplot(sh21_10Cluster9_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_9_dinos
#ggsave(plot = plot_sh21_10clusters_9_dinos, filename = "plot_sh21_10clusters_9_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_10Cluster10_long$species <- factor(sh21_10Cluster10_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_10_dinos <- ggplot(sh21_10Cluster10_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
##plot_sh21_10clusters_10_dinos
#ggsave(plot = plot_sh21_10clusters_10_dinos, filename = "plot_sh21_10clusters_10_dinos.pdf", width = 30, height = 15, unit = "cm")

# 4.2.6 11-cluster-solution ----

sh21_11Cluster1_long$species <- factor(sh21_11Cluster1_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_1_dinos <- ggplot(sh21_11Cluster1_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_1_dinos
#ggsave(plot = plot_sh21_11clusters_1_dinos, filename = "plot_sh21_11clusters_1_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster2_long$species <- factor(sh21_11Cluster2_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_2_dinos <- ggplot(sh21_11Cluster2_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_2_dinos
#ggsave(plot = plot_sh21_11clusters_2_dinos, filename = "plot_sh21_11clusters_2_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster3_long$species <- factor(sh21_11Cluster3_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_3_dinos <- ggplot(sh21_11Cluster3_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_3_dinos
#ggsave(plot = plot_sh21_11clusters_3_dinos, filename = "plot_sh21_11clusters_3_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster4_long$species <- factor(sh21_11Cluster4_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_4_dinos <- ggplot(sh21_11Cluster4_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_4_dinos
#ggsave(plot = plot_sh21_11clusters_4_dinos, filename = "plot_sh21_11clusters_4_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster5_long$species <- factor(sh21_11Cluster5_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_5_dinos <- ggplot(sh21_11Cluster5_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_5_dinos
#ggsave(plot = plot_sh21_11clusters_5_dinos, filename = "plot_sh21_11clusters_5_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster6_long$species <- factor(sh21_11Cluster6_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_6_dinos <- ggplot(sh21_11Cluster6_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_6_dinos
#ggsave(plot = plot_sh21_11clusters_6_dinos, filename = "plot_sh21_11clusters_6_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster7_long$species <- factor(sh21_11Cluster7_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_7_dinos <- ggplot(sh21_11Cluster7_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_7_dinos
#ggsave(plot = plot_sh21_11clusters_7_dinos, filename = "plot_sh21_11clusters_7_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster8_long$species <- factor(sh21_11Cluster8_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_8_dinos <- ggplot(sh21_11Cluster8_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_8_dinos
#ggsave(plot = plot_sh21_11clusters_8_dinos, filename = "plot_sh21_11clusters_8_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster9_long$species <- factor(sh21_11Cluster9_long$species,
                                       levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_9_dinos <- ggplot(sh21_11Cluster9_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_9_dinos
#ggsave(plot = plot_sh21_11clusters_9_dinos, filename = "plot_sh21_11clusters_9_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster10_long$species <- factor(sh21_10Cluster10_long$species,
                                        levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_10clusters_10_dinos <- ggplot(sh21_10Cluster10_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_10clusters_10_dinos
#ggsave(plot = plot_sh21_10clusters_10_dinos, filename = "plot_sh21_10clusters_10_dinos.pdf", width = 30, height = 15, unit = "cm")

sh21_11Cluster11_long$species <- factor(sh21_11Cluster11_long$species,
                                        levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", "Snep_prop", "Squa_prop", "Pdal_prop", "Ibre_prop", "Dubr_prop", "Peri_prop", "Lspp_prop", "Xxan_prop", "Tapp_prop", "Pame_prop", "Espp_comb_prop", "Srob_prop", "Char_prop", "Gymn_prop", "Pkof_prop", "Psch_prop", "Nlab_prop", "Ocen_prop", "Oisr_prop", "Ojan_prop", "Olon_prop", "Pret_prop", "Ppsi_prop", "Smir_prop", "Sram_prop", "Sram_bul_prop", "Smem_bel_prop", "Sspp_prop", "Sdel_prop", "Sben_prop", "Ipal_prop", "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Dcha_prop", "Btep_prop", "Bspo_prop", "Atax_prop", "Pzoh_prop", "Spac_prop", "Srei_prop", "Sste_prop", "Tpel_pro", "Tvan_prop", "Vspi_prop", "Pspp_prop"))
plot_sh21_11clusters_11_dinos <- ggplot(sh21_11Cluster11_long, aes(x = species, y = percentage, fill = species)) +
  geom_boxplot(col = "black")
#plot_sh21_11clusters_11_dinos
#ggsave(plot = plot_sh21_11clusters_11_dinos, filename = "plot_sh21_11clusters_11_dinos.pdf", width = 30, height = 15, unit = "cm")

# 4.3 dino size plots ----

str(sh21_7Cluster)
str(sh21_8Cluster)
str(sh21_9Cluster)
str(sh21_10Cluster)
str(sh21_11Cluster)

# 4.3.1 7-cluster-solution ----

sh21_7clustering_map_Sant <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Sant_all_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Sant

sh21_7clustering_map_Brig <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Bspp_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Brig

sh21_7clustering_map_Nlab <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Nlab_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Nlab

sh21_7clustering_map_Iacu <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Iacu_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Iacu

sh21_7clustering_map_Ocen <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ocen_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Ocen

sh21_7clustering_map_Pret <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Pret_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Pret

sh21_7clustering_map_Smir <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Smir_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Smir

sh21_7clustering_map_Ipal <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipal_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Ipal

sh21_7clustering_map_Ipat <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipat_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Ipat

sh21_7clustering_map_Ipar <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipar_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Ipar

sh21_7clustering_map_Numb <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_7Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Numb_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_7clustering_map_Numb

sh21_7clustering_map_dinos <- plot_grid(sh21_7clustering_map_Sant,  sh21_7clustering_map_Ipal, sh21_7clustering_map_Numb, 
                                        sh21_7clustering_map_Brig, sh21_7clustering_map_Nlab, sh21_7clustering_map_Smir,
                                        sh21_7clustering_map_Iacu, sh21_7clustering_map_Ipat, sh21_7clustering_map_Ipar,
                                        sh21_7clustering_map_Ocen, sh21_7clustering_map_Pret, align = "v", ncol = 3)
#sh21_7clustering_map_dinos
ggsave(plot = sh21_7clustering_map_dinos, filename = "sh21_7clustering_map_dinos.pdf", width = 45, height = 60, unit = "cm")


# 4.3.2 8-cluster-solution ----

sh21_8clustering_map_Sant <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Sant_all_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Sant

sh21_8clustering_map_Brig <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Bspp_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Brig

sh21_8clustering_map_Nlab <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Nlab_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Nlab

sh21_8clustering_map_Iacu <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Iacu_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Iacu

sh21_8clustering_map_Ocen <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ocen_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Ocen

sh21_8clustering_map_Pret <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Pret_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Pret

sh21_8clustering_map_Smir <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Smir_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Smir

sh21_8clustering_map_Ipal <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipal_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Ipal

sh21_8clustering_map_Ipat <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipat_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Ipat

sh21_8clustering_map_Ipar <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipar_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Ipar

sh21_8clustering_map_Numb <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_8Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Numb_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_8clustering_map_Numb

sh21_8clustering_map_dinos <- plot_grid(sh21_8clustering_map_Sant, sh21_8clustering_map_Ipal, sh21_8clustering_map_Numb, 
                                        sh21_8clustering_map_Brig, sh21_8clustering_map_Nlab, sh21_8clustering_map_Smir,
                                        sh21_8clustering_map_Iacu, sh21_8clustering_map_Ipat, sh21_8clustering_map_Ipar,
                                        sh21_8clustering_map_Ocen, sh21_8clustering_map_Pret, align = "v", ncol = 3)
#sh21_8clustering_map_dinos
ggsave(plot = sh21_8clustering_map_dinos, filename = "sh21_8clustering_map_dinos.pdf", width = 45, height = 60, unit = "cm")


# 4.3.3 9-cluster-solution ----

sh21_9clustering_map_Sant <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Sant_all_prop), pch  = 21, show.legend = FALSE) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("S. antarctica"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Sant

sh21_9clustering_map_Imin <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Imin_prop), pch  = 21,show.legend = FALSE ) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("I. minutum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Imin

# sh21_9clustering_map_Numb <- ggplot() +
#   geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
#   coord_map("ortho", orientation = c(-90, 0, 0)) + 
#   geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Numb_prop), pch  = 21, show.legend = FALSE) +
#   scale_size(limits = c(0, 1)) + 
#   scale_fill_viridis(discrete = TRUE, option = "plasma") +
#   labs(title = expression(italic("I. minutum"))) + 
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
#      axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
# sh21_9clustering_map_INumb


# sh21_9clustering_map_Ipal <- ggplot() +
#   geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
#   coord_map("ortho", orientation = c(-90, 0, 0)) + 
#   geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Ipal_prop), pch  = 21, show.legend = FALSE) +
#   scale_fill_viridis(discrete = TRUE, option = "plasma") +
#   labs(title = expression(italic("I. minutum"))) + 
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
#      axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
# sh21_9clustering_map_Ipal

sh21_9clustering_map_Brig <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Bspp_prop), pch  = 21,show.legend = FALSE ) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("Brigantedinium spp."))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Brig

sh21_9clustering_map_Ocen <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Ocen_prop), pch  = 21, show.legend = FALSE) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("O. centrocarpum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Ocen

sh21_9clustering_map_Nlab <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Nlab_prop), pch  = 21, show.legend = FALSE) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("N. labyrinthus"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Nlab

sh21_9clustering_map_Iacu <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Iacu_prop), pch  = 21, show.legend = FALSE) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("I. aculeatum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Iacu

# sh21_9clustering_map_Ipat <- ggplot() +
#   geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
#   coord_map("ortho", orientation = c(-90, 0, 0)) + 
#   geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Ipat_prop), pch  = 21, show.legend = FALSE) +
#   scale_fill_viridis(discrete = TRUE, option = "plasma") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
#      axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
# sh21_9clustering_map_Ipat
# 
# sh21_9clustering_map_Ipar <- ggplot() +
#   geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
#   coord_map("ortho", orientation = c(-90, 0, 0)) + 
#   geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Ipar_prop), pch  = 21, show.legend = FALSE) +
#   scale_fill_viridis(discrete = TRUE, option = "plasma") +
#   theme_bw() +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
#      axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
# sh21_9clustering_map_Ipar

sh21_9clustering_map_Sspp <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=sh21_9Cluster, aes(x=Longitude, y=Latitude, fill = facet_label, size = Sspp_prop), pch  = 21, show.legend = FALSE) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = expression(italic("Spiniferitis spp."))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#sh21_9clustering_map_Sspp

sh21_9clustering_map_dinos <- plot_grid(sh21_9clustering_map_Sant, sh21_9clustering_map_Imin, sh21_9clustering_map_Brig, 
                                        sh21_9clustering_map_Nlab, sh21_9clustering_map_Ocen, 
                                        sh21_9clustering_map_Iacu, sh21_9clustering_map_Sspp, 
                                        align = "v", ncol = 3)
sh21_9clustering_map_dinos
ggsave(plot = sh21_9clustering_map_dinos, filename = "SFig_sh21_9clustering_map_dinos.pdf", width = 45, height = 20, unit = "cm")

sh21_9clustering_map_dinos_2 <- plot_grid(sh21_9clustering_map_Sant, sh21_9clustering_map_Imin, sh21_9clustering_map_Brig,  
                                          sh21_9clustering_map_Nlab, sh21_9clustering_map_Ocen, sh21_9clustering_map_Iacu, sh21_9clustering_map_Sspp, align = "v", ncol = 4)
sh21_9clustering_map_dinos_2
ggsave(plot = sh21_9clustering_map_dinos_2, filename = "SFig_sh21_9clustering_map_dinos_2.pdf", width = 45, height = 60, unit = "cm")


# 4.3.4 10-cluster-solution ----

sh21_10clustering_map_Sant <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Sant_all_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Sant

sh21_10clustering_map_Brig <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Bspp_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Brig

sh21_10clustering_map_Nlab <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Nlab_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Nlab

sh21_10clustering_map_Iacu <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Iacu_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Iacu

sh21_10clustering_map_Ocen <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ocen_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Ocen

sh21_10clustering_map_Pret <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Pret_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Pret

sh21_10clustering_map_Smir <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Smir_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Smir

sh21_10clustering_map_Ipal <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipal_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Ipal

sh21_10clustering_map_Ipat <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipat_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Ipat

sh21_10clustering_map_Ipar <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipar_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Ipar

sh21_10clustering_map_Numb <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_10Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Numb_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_10clustering_map_Numb

sh21_10clustering_map_dinos <- plot_grid(sh21_10clustering_map_Sant, sh21_10clustering_map_Ipal, sh21_10clustering_map_Numb,
                                        sh21_10clustering_map_Brig, sh21_10clustering_map_Nlab, sh21_10clustering_map_Smir,
                                        sh21_10clustering_map_Iacu, sh21_10clustering_map_Ipat, sh21_10clustering_map_Ipar,
                                        sh21_10clustering_map_Ocen, sh21_10clustering_map_Pret, align = "v", ncol = 3)
sh21_10clustering_map_dinos
ggsave(plot = sh21_10clustering_map_dinos, filename = "sh21_10clustering_map_dinos.pdf", width = 45, height = 60, unit = "cm")

# 4.3.5 11-cluster-solution ----

sh21_11clustering_map_Sant <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Sant_all_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Sant

sh21_11clustering_map_Brig <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Bspp_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Brig

sh21_11clustering_map_Nlab <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Nlab_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Nlab

sh21_11clustering_map_Iacu <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Iacu_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Iacu

sh21_11clustering_map_Ocen <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ocen_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Ocen

sh21_11clustering_map_Pret <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Pret_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Pret

sh21_11clustering_map_Smir <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Smir_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Smir

sh21_11clustering_map_Ipal <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipal_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Ipal

sh21_11clustering_map_Ipat <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipat_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Ipat

sh21_11clustering_map_Ipar <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Ipar_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Ipar

sh21_11clustering_map_Numb <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(data=sh21_11Cluster, aes(x=Longitude, y=Latitude, fill = cluster, size = Numb_prop), pch  = 21) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw()
#sh21_11clustering_map_Numb


sh21_11clustering_map_dinos <- plot_grid(sh21_11clustering_map_Sant, sh21_11clustering_map_Ipal, sh21_11clustering_map_Numb,
                                         sh21_11clustering_map_Brig, sh21_11clustering_map_Nlab, sh21_11clustering_map_Smir,
                                         sh21_11clustering_map_Iacu, sh21_11clustering_map_Ipat, sh21_11clustering_map_Ipar,
                                         sh21_11clustering_map_Ocen, sh21_11clustering_map_Pret, align = "v", ncol = 3)
#sh21_11clustering_map_dinos
ggsave(plot = sh21_11clustering_map_dinos, filename = "sh21_11clustering_map_dinos.pdf", width = 45, height = 60, unit = "cm")



# 5. WSI Clustering ----

# DECISION 5 CLUSTERS

# 5.1 Cluster maps ----

# 5.1.1 3-cluster-solution ----
head(wsie_3Cluster)
wsie_3Cluster <- mutate(wsie_3Cluster, facet_label = fct_recode(
  wsie_cluster, "Sant_1" = "1","Imin" = "2","Brig" = "3"))

wsie_3clustering_map <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_3Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_3clustering_map
#ggsave(wsie_3clustering_map, filename = "wsie_3clustering.pdf", width = 20, height = 20, unit = "cm")

# 5.1.2 4-cluster-solution ----

head(wsie_4Cluster)
wsie_4Cluster <- mutate(wsie_4Cluster, facet_label = fct_recode(
  wsie_cluster, "Sant_1" = "1","Imin" = "2","Brig" = "3","anything" = "4"))

wsie_4clustering_map <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_4Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_4clustering_map
#ggsave(wsie_4clustering_map, filename = "wsie_4clustering.pdf", width = 20, height = 20, unit = "cm")


# 5.1.3 5-cluster-solution ----

head(wsie_5Cluster)
wsie_5Cluster <- mutate(wsie_5Cluster, facet_label = fct_recode(
  wsie_cluster, "Sant_1" = "1","Imin" = "2","Brig" = "3","anything" = "4","Sant_2" = "5"))

wsie_5clustering_map <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
wsie_5clustering_map
ggsave(wsie_5clustering_map, filename = "Fig5b_wsie_5clustering.pdf", width = 20, height = 20, unit = "cm")

# 5.1.4 6-cluster-solution ----

head(wsie_6Cluster)
wsie_6Cluster <- mutate(wsie_6Cluster, facet_label = fct_recode(
  wsie_cluster, "Sant_1" = "1","Imin" = "2","Brig" = "3","anything" = "4","Sant_2" = "5", "x" = "6"))

wsie_6clustering_map <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_6Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_6clustering_map
#ggsave(wsie_6clustering_map, filename = "wsie_6clustering.pdf", width = 20, height = 20, unit = "cm")

# 5.1.5 7-cluster-solution ----
  
head(wsie_7Cluster)
wsie_7Cluster <- mutate(wsie_7Cluster, facet_label = fct_recode(
  wsie_cluster, "Sant_1" = "1","Imin" = "2","Brig" = "3","anything" = "4","Sant_2" = "5", "x" = "6", "y" = "7"))

wsie_7clustering_map <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_7Cluster, aes(x=Longitude, y=Latitude, fill = facet_label), pch = 21, size = 1.8) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_7clustering_map
#ggsave(wsie_7clustering_map, filename = "wsie_7clustering.pdf", width = 20, height = 20, unit = "cm")


wsie_cluster_maps <- plot_grid(wsie_3clustering_map, wsie_4clustering_map, wsie_5clustering_map, wsie_6clustering_map, wsie_7clustering_map, align = "v", ncol = 2)
#wsie_cluster_maps
#ggsave( filename = "wsie_cluster_maps.pdf", width = 25, height = 25, unit = "cm")


# 5.2 DINO ASSEMBLAGES IN CLUSTERS ----

# 5 cluster ----

str(wsie_5Cluster)
wsie_5Cluster1_long$percentage <- wsie_5Cluster1_long$percentage * 100
wsie_5Cluster2_long$percentage <- wsie_5Cluster2_long$percentage * 100
wsie_5Cluster3_long$percentage <- wsie_5Cluster3_long$percentage * 100
wsie_5Cluster4_long$percentage <- wsie_5Cluster4_long$percentage * 100
wsie_5Cluster5_long$percentage <- wsie_5Cluster5_long$percentage * 100

# order dinos ----
wsie_5Cluster1_long$species <- factor(wsie_5Cluster1_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                                              "Btep_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", 
                                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", 
                                                                              "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Lspp_prop",
                                                                              "Nlab_prop", "Ocen_prop", "Oisr_prop", "Pdal_prop", "Pret_prop", 
                                                                              "Peri_prop", "Dcha_prop", "Char_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))
wsie_5Cluster2_long$species <- factor(wsie_5Cluster2_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                                              "Btep_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", 
                                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", 
                                                                              "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Lspp_prop",
                                                                              "Nlab_prop", "Ocen_prop", "Oisr_prop", "Pdal_prop", "Pret_prop", 
                                                                              "Peri_prop", "Dcha_prop", "Char_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))
wsie_5Cluster3_long$species <- factor(wsie_5Cluster3_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                                              "Btep_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", 
                                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", 
                                                                              "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Lspp_prop",
                                                                              "Nlab_prop", "Ocen_prop", "Oisr_prop", "Pdal_prop", "Pret_prop", 
                                                                              "Peri_prop", "Dcha_prop", "Char_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))
wsie_5Cluster4_long$species <- factor(wsie_5Cluster4_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                                              "Btep_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", 
                                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", 
                                                                              "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Lspp_prop",
                                                                              "Nlab_prop", "Ocen_prop", "Oisr_prop", "Pdal_prop", "Pret_prop", 
                                                                              "Peri_prop", "Dcha_prop", "Char_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))
wsie_5Cluster5_long$species <- factor(wsie_5Cluster5_long$species, levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                                              "Btep_prop", "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", 
                                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Ipli_prop", "Isph_prop", 
                                                                              "Istr_prop", "Ivar_prop", "Ivel_prop", "Lmac_prop", "Lspp_prop",
                                                                              "Nlab_prop", "Ocen_prop", "Oisr_prop", "Pdal_prop", "Pret_prop", 
                                                                              "Peri_prop", "Dcha_prop", "Char_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))


wsie_5Cluster1_long <- mutate(wsie_5Cluster1_long, facet_label = fct_recode(species, 
                                                                            "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                                            "B. tepikiense" = "Btep_prop","S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop",
                                                                            "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. plicatum" = "Ipli_prop", "I. sphaericum" = "Isph_prop", 
                                                                            "I. strialatum" = "Istr_prop", "I. variaseptum" = "Ivar_prop", "I. velorum" = "Ivel_prop", "L. machaerophorum " = "Lmac_prop", "Lejeunecysta spp." = "Lspp_prop",
                                                                            "N. labyrinthus" = "Nlab_prop", "O. centrocarpum" = "Ocen_prop", "O. israelianum" = "Oisr_prop", "P. dalei" = "Pdal_prop", "P. reticulata" = "Pret_prop", 
                                                                            "Peridiniium spp." = "Peri_prop", "D. chathamensis" = "Dcha_prop","C. harlandii" = "Char_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

wsie_5Cluster2_long <- mutate(wsie_5Cluster2_long, facet_label = fct_recode(species, "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                                            "B. tepikiense" = "Btep_prop","S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop",
                                                                            "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. plicatum" = "Ipli_prop", "I. sphaericum" = "Isph_prop", 
                                                                            "I. strialatum" = "Istr_prop", "I. variaseptum" = "Ivar_prop", "I. velorum" = "Ivel_prop", "L. machaerophorum " = "Lmac_prop", "Lejeunecysta spp." = "Lspp_prop",
                                                                            "N. labyrinthus" = "Nlab_prop", "O. centrocarpum" = "Ocen_prop", "O. israelianum" = "Oisr_prop", "P. dalei" = "Pdal_prop", "P. reticulata" = "Pret_prop", 
                                                                            "Peridiniium spp." = "Peri_prop", "D. chathamensis" = "Dcha_prop","C. harlandii" = "Char_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

wsie_5Cluster3_long <- mutate(wsie_5Cluster3_long, facet_label = fct_recode(species, "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                                            "B. tepikiense" = "Btep_prop","S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop",
                                                                            "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. plicatum" = "Ipli_prop", "I. sphaericum" = "Isph_prop", 
                                                                            "I. strialatum" = "Istr_prop", "I. variaseptum" = "Ivar_prop", "I. velorum" = "Ivel_prop", "L. machaerophorum " = "Lmac_prop", "Lejeunecysta spp." = "Lspp_prop",
                                                                            "N. labyrinthus" = "Nlab_prop", "O. centrocarpum" = "Ocen_prop", "O. israelianum" = "Oisr_prop", "P. dalei" = "Pdal_prop", "P. reticulata" = "Pret_prop", 
                                                                            "Peridiniium spp." = "Peri_prop", "D. chathamensis" = "Dcha_prop","C. harlandii" = "Char_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

wsie_5Cluster4_long <- mutate(wsie_5Cluster4_long, facet_label = fct_recode(species, "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                                            "B. tepikiense" = "Btep_prop","S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop",
                                                                            "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. plicatum" = "Ipli_prop", "I. sphaericum" = "Isph_prop", 
                                                                            "I. strialatum" = "Istr_prop", "I. variaseptum" = "Ivar_prop", "I. velorum" = "Ivel_prop", "L. machaerophorum " = "Lmac_prop", "Lejeunecysta spp." = "Lspp_prop",
                                                                            "N. labyrinthus" = "Nlab_prop", "O. centrocarpum" = "Ocen_prop", "O. israelianum" = "Oisr_prop", "P. dalei" = "Pdal_prop", "P. reticulata" = "Pret_prop", 
                                                                            "Peridiniium spp." = "Peri_prop", "D. chathamensis" = "Dcha_prop","C. harlandii" = "Char_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

wsie_5Cluster5_long <- mutate(wsie_5Cluster5_long, facet_label = fct_recode(species, "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                                            "B. tepikiense" = "Btep_prop","S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop",
                                                                            "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. plicatum" = "Ipli_prop", "I. sphaericum" = "Isph_prop", 
                                                                            "I. strialatum" = "Istr_prop", "I. variaseptum" = "Ivar_prop", "I. velorum" = "Ivel_prop", "L. machaerophorum " = "Lmac_prop", "Lejeunecysta spp." = "Lspp_prop",
                                                                            "N. labyrinthus" = "Nlab_prop", "O. centrocarpum" = "Ocen_prop", "O. israelianum" = "Oisr_prop", "P. dalei" = "Pdal_prop", "P. reticulata" = "Pret_prop", 
                                                                            "Peridiniium spp." = "Peri_prop", "D. chathamensis" = "Dcha_prop","C. harlandii" = "Char_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

# plot ----
plot_wsie_5Cluster1_dinos <- ggplot(wsie_5Cluster1_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 1",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_wsie_5Cluster1_dinos

plot_wsie_5Cluster2_dinos <- ggplot(wsie_5Cluster2_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 2",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_wsie_5Cluster2_dinos

plot_wsie_5Cluster3_dinos <- ggplot(wsie_5Cluster3_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE ) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 3",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_wsie_5Cluster3_dinos

plot_wsie_5Cluster4_dinos <- ggplot(wsie_5Cluster4_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 4",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_wsie_5Cluster4_dinos

plot_wsie_5Cluster5_dinos <- ggplot(wsie_5Cluster5_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black", show.legend = FALSE) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 5",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
plot_wsie_5Cluster5_dinos

plot_wsie_5cluster_dinos <- plot_grid(plot_wsie_5Cluster1_dinos, plot_wsie_5Cluster2_dinos, plot_wsie_5Cluster3_dinos, plot_wsie_5Cluster4_dinos, plot_wsie_5Cluster5_dinos,align = "v", ncol = 1)
plot_wsie_5cluster_dinos
ggsave(plot = plot_wsie_5cluster_dinos, filename = "Fig5a_wsie_5clusters_dinos.pdf", width = 50, height = 30, unit = "cm")

# legend plot ----

plot_wsie_5cluster5_dinos <- ggplot(wsie_5Cluster5_long, aes(x = species, y = percentage, fill = facet_label)) + 
  geom_boxplot(col = "black") +
  scale_y_continuous(limits = c(0, 100)) +
  labs(title = "Cluster 5",# here, make sure that it's the right title
       x = "", y = "Median species contribution (%)", fill = "Dinocyst species") +
  theme_bw() +
  theme(axis.text.x = element_blank ())
#plot_wsie_5cluster5_dinos
ggsave(plot = plot_wsie_5cluster5_dinos, filename = "Fig5a_legend_wsie_5clusters_dinos.pdf", width = 50, height = 30, unit = "cm")


# 5.3 dino size plots ----

str(wsie_5Cluster)

wsie_5clustering_map_Sant <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = wsie_cluster, size = Sant_all_prop), pch  = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = expression(italic("S. antarctica"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Sant

wsie_5clustering_map_Imin <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = wsie_cluster, size = Imin_prop), pch = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D")+
  labs(title = expression(italic("I. minutum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Imin

wsie_5clustering_map_Numb <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude,fill = wsie_cluster, size = Numb_prop), pch = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = expression(italic("N. umbiliphora"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Numb

wsie_5clustering_map_Brig <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = wsie_cluster, size = Bspp_prop), pch = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = expression(italic("Brigantedinium spp."))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Brig

wsie_5clustering_map_Cmer <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = wsie_cluster, size = Cmer_prop), pch = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = expression(italic("C. meridianum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Cmer

wsie_5clustering_map_Ipal <- ggplot() +
  geom_polygon(data = so, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data=wsie_5Cluster, aes(x=Longitude, y=Latitude, fill = wsie_cluster, size = Ipal_prop), pch = 21) +
  scale_size(limits = c(0, 1)) + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = expression(italic("I. pallidum"))) + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#wsie_5clustering_map_Ipal

wsie_5clustering_map_dinos <- plot_grid(wsie_5clustering_map_Sant, wsie_5clustering_map_Numb, wsie_5clustering_map_Imin, wsie_5clustering_map_Brig, wsie_5clustering_map_Cmer, wsie_5clustering_map_Ipal,  align = "v", ncol = 3)
wsie_5clustering_map_dinos
ggsave(plot = wsie_5clustering_map_dinos, filename = "wsie_5clusters_maps_dinos.pdf", width = 45, height = 25, unit = "cm")


############################################################################################## ----

# 6. DINOS AND ENVIRONMENTAL SURFACE PARAMETERS IN CLUSTERS ----
# 9 cluster solution 

str(sh21_surface_9Cluster)
# we just consider some main dino species
sh21_surface_9Cluster_shortdinos <- select(sh21_surface_9Cluster, contains("median"), cluster, 
                                           Bspp_prop, Cmer_prop, Dcha_prop, Gymn_prop, Sant_all_prop,
                                           Numb_prop, Imin_prop, Snep_prop, Squa_prop, Ipal_prop,
                                           Iacu_prop, Ipar_prop, Ipat_prop, Isph_prop, Nlab_prop,
                                           Ocen_prop, Pret_prop, Smir_prop, Sram_prop, Sspp_prop)



# prep clusters
sh21_surface_9Cluster_long <- pivot_longer(sh21_surface_9Cluster_shortdinos, cols = ends_with("prop"), names_to = "species", values_to = "percentage" )
str(sh21_surface_9Cluster_long)
sh21_surface_9Cluster_long$percentage <- sh21_surface_9Cluster_long$percentage * 100

# order dinos 
sh21_surface_9Cluster_long$species <- factor(sh21_surface_9Cluster_long$species, 
                                                   levels = c("Sant_all_prop", "Numb_prop", "Imin_prop", "Cmer_prop", "Bspp_prop", 
                                                              "Snep_prop", "Squa_prop", "Gymn_prop", "Ipal_prop", "Nlab_prop",
                                                              "Iacu_prop", "Ipar_prop", "Ipat_prop", "Isph_prop", "Ocen_prop", 
                                                              "Pret_prop", "Dcha_prop", "Smir_prop", "Sram_prop", "Sspp_prop"))

# rename dinos 
sh21_surface_9Cluster_long <- mutate(sh21_surface_9Cluster_long, facet_label = fct_recode(
                                                species, "S. antarctica" = "Sant_all_prop", "N. umbiliphora" = "Numb_prop", "I. minutum" = "Imin_prop", "C. meridianum" = "Cmer_prop", "Brigantedinium spp." = "Bspp_prop",
                                                         "S. nephroides" = "Snep_prop", "S. quanta" = "Squa_prop", "Gymnodinium spp." = "Gymn_prop", "I. pallidum" = "Ipal_prop","N. labyrinthus" = "Nlab_prop",
                                                         "I. aculeatum" = "Iacu_prop", "I. paradoxum" = "Ipar_prop", "I. patulum" = "Ipat_prop", "I. sphaericum" = "Isph_prop", "O. centrocarpum" = "Ocen_prop",
                                                         "P. reticulata" = "Pret_prop", "D. chathamensis" = "Dcha_prop", "S. mirabilis" = "Smir_prop", "S. ramosus " = "Sram_prop", "Spiniferitis spp." = "Sspp_prop"))

sh21_surface_9Cluster_long <- mutate(sh21_surface_9Cluster_long, facet_label_2 = fct_recode(
  cluster, "high-Ocen" = "1","Sant" = "2","high-Brig" = "3","low-Brig" = "4","Imin" = "5","low-Ocen" = "6","Iacu" = "7","Nlab" = "8","Spin" = "9"))

# plot 
str(sh21_surface_9Cluster_long)
# temp 
cross_sh21_9Cluster_temp_median_dinos <- ggplot() + 
  geom_point(data = sh21_surface_9Cluster_long, aes(x = Temp_median, y = percentage, fill = facet_label_2), size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  facet_wrap (~facet_label, nrow = 4, ncol = 5, scales = "free_x") + 
  labs(title = "Sea surface temperature",# here, make sure that it's the right title
       x = "Temp (°C)", y = "Median species contribution (%)", fill = "Cluster") +
  theme_bw() 
#cross_sh21_9Cluster_temp_median_dinos
ggsave(plot = cross_sh21_9Cluster_temp_median_dinos, filename = "cross_sh21_9Cluster_temp_median_dinos.pdf", width = 30, height = 20, unit = "cm")

# ice presence  
cross_sh21_9Cluster_sea_ice_median_dinos <- ggplot() + 
  geom_point(data = sh21_surface_9Cluster_long, aes(x=Ice_presence_median, y=percentage, fill = facet_label_2), size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  facet_wrap (~facet_label, nrow = 4, ncol = 5, scales = "free_x") + 
  labs(title = "Sea Ice presence",# here, make sure that it's the right title
       x = "Sea Ice presence (%)", y = "Median species contribution (%)", fill = "Cluster") +
  theme_bw() 
#cross_sh21_9Cluster_sea_ice_median_dinos

# salinity
cross_sh21_9Cluster_salinity_median_dinos <- ggplot() + 
  geom_point(data = sh21_surface_9Cluster_long, aes(x = Salinity_median, y=percentage, fill = facet_label_2), size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  facet_wrap (~facet_label, nrow = 4, ncol = 5, scales = "free_x") + 
  labs(title = "Salinity",# here, make sure that it's the right title
       x = "Salinity (psu)", y = "Median species contribution (%)", fill = "Cluster") +
  theme_bw() 
#cross_sh21_9Cluster_salinity_median_dinos

# nitrate
cross_sh21_9Cluster_nitrate_median_dinos <- ggplot() + 
  geom_point(data = sh21_surface_9Cluster_long, aes(x = Nitrate_median, y=percentage, fill = facet_label_2), size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  facet_wrap (~facet_label, nrow = 4, ncol = 5, scales = "free_x") + 
  labs(title = "Nitrate",# here, make sure that it's the right title
       x = "Nitrate (mmol/m3)", y = "Median species contribution (%)", fill = "Cluster") +
  theme_bw() 
#cross_sh21_9Cluster_nitrate_median_dinos

# silicate
cross_sh21_9Cluster_silicate_median_dinos <- ggplot() + 
  geom_point(data = sh21_surface_9Cluster_long, aes(x = Silicate_median, y=percentage, fill = facet_label_2), size = 2.5, pch = 21) +
  scale_y_continuous(limits = c(0,100)) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") + 
  facet_wrap (~facet_label, nrow = 4, ncol = 5, scales = "free_x") + 
  labs(title = "Silicate",# here, make sure that it's the right title
       x = "Silicate (mmol/m3)", y = "Median species contribution (%)", fill = "Cluster") +
  theme_bw() 
#cross_sh21_9Cluster_silicate_dinos

cross_sh21_9Cluster_surface_median_dinos <- plot_grid(cross_sh21_9Cluster_temp_median_dinos, cross_sh21_9Cluster_sea_ice_median_dinos, cross_sh21_9Cluster_salinity_median_dinos,
                                                      cross_sh21_9Cluster_nitrate_median_dinos, cross_sh21_9Cluster_silicate_median_dinos, align = "v", ncol = 3)
#cross_sh21_9Cluster_surface_median_dinos
ggsave(plot = cross_sh21_9Cluster_surface_median_dinos, filename = "cross_sh21_9Cluster_surface_median_dinos.pdf", width = 45, height = 25, unit = "cm")


############################################################################################################----

# 7. TRANSPORT MAP----

str(sh21_9Cluster)
str(transport_all)
str(transport_map)

#write.table(transport_map, file = "transport_map.txt", col.names = TRUE)

# order clusters
transport_map$facet = factor(transport_map$cluster, levels = c("2", "5",  "3", "8", "4", "6", "7", "1", "9"))


 

# doublecheck if this is correct
transport_map <- mutate(transport_map, facet = fct_recode(facet, "high-Ocen-cluster" = "1", "Sant-cluster" = "2", "high-Brig-cluster" = "3", 
                                                          "low-Brig-cluster" = "4", "Imin-cluster" = "5","low-Ocen-cluster" = "6",
                                                          "Iacu-cluster" = "7","Nlab-cluster" = "8","Spin-cluster" = "9" ))

# plot 
transport_cluster_map <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data = transport_map, aes(x=Longitude_10m, y=Latitude_10m, col = facet), pch = 21, size = 1, show.legend = FALSE ) +
  scale_color_viridis(discrete = TRUE, option = "plasma", alpha = 0.2) +
  geom_point(data = transport_map, aes(x=Longitude_dinos, y=Latitude_dinos, fill = facet), pch = 21, size = 2) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = "Transport surface locations for sediment surface samples", fill = "Cluster") + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#transport_cluster_map
ggsave(plot = transport_cluster_map, filename = "Fig6_transport_cluster_map.pdf", width = 60, height = 50, unit = "cm")

transport_cluster_map_2 <- ggplot() +
  geom_polygon(data = sh, aes(x=long, y = lat, group = group), fill = "grey3", alpha=0.3) + 
  coord_map("ortho", orientation = c(-90, 0, 0)) + 
  geom_point(data = transport_map, aes(x=Longitude_10m, y=Latitude_10m, col = facet), pch = 21, size = 3, show.legend = FALSE ) +
  scale_color_viridis(discrete = TRUE, option = "plasma", alpha = 0.2) +
  geom_point(data = transport_map, aes(x=Longitude_dinos, y=Latitude_dinos, fill = facet), pch = 21, size = 5) +
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = "Transport surface locations for sediment surface samples", fill = "Cluster") + 
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
#transport_cluster_map_2
ggsave(plot = transport_cluster_map_2, filename = "Fig6_transport_cluster_map_2.pdf", width = 60, height = 50, unit = "cm")


# 8. COMBO SURFACE TRANSPORT: ENVIRONMENTAL PARAMETERS PER CLUSTER ----
str(transport_dinos_9clusters_1)
str(sh21_surface_9Cluster1)

# 8.1 transport data ----

# cl 1 ----
str(transport_dinos_9clusters_1)

test_transport_dinos_9clusters_1 <- select(transport_dinos_9clusters_1, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_1) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_1 <- strrep("t", times = 72) 
str(transport_data_1)
t_transport_dinos_9clusters_1 <- cbind (test_transport_dinos_9clusters_1,transport_data_1)
str(t_transport_dinos_9clusters_1)
names(t_transport_dinos_9clusters_1)[1] <- "Temp"
names(t_transport_dinos_9clusters_1)[2] <- "Ice"
names(t_transport_dinos_9clusters_1)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_1)[4] <- "Silicate"
names(t_transport_dinos_9clusters_1)[5] <- "Salinity"
names(t_transport_dinos_9clusters_1)[6] <- "Datatype"

# cl 2 ----
test_transport_dinos_9clusters_2 <- select(transport_dinos_9clusters_2, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_2) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_2 <- strrep("t", times = 64) 
str(transport_data_2)
t_transport_dinos_9clusters_2 <- cbind (test_transport_dinos_9clusters_2,transport_data_2)
str(t_transport_dinos_9clusters_2)
names(t_transport_dinos_9clusters_2)[1] <- "Temp"
names(t_transport_dinos_9clusters_2)[2] <- "Ice"
names(t_transport_dinos_9clusters_2)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_2)[4] <- "Silicate"
names(t_transport_dinos_9clusters_2)[5] <- "Salinity"
names(t_transport_dinos_9clusters_2)[6] <- "Datatype"

# cl 3 ----
test_transport_dinos_9clusters_3 <- select(transport_dinos_9clusters_3, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_3) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_3 <- strrep("t", times = 86) 
str(transport_data_3)
t_transport_dinos_9clusters_3 <- cbind (test_transport_dinos_9clusters_3,transport_data_3)
str(t_transport_dinos_9clusters_3)
names(t_transport_dinos_9clusters_3)[1] <- "Temp"
names(t_transport_dinos_9clusters_3)[2] <- "Ice"
names(t_transport_dinos_9clusters_3)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_3)[4] <- "Silicate"
names(t_transport_dinos_9clusters_3)[5] <- "Salinity"
names(t_transport_dinos_9clusters_3)[6] <- "Datatype"

# cl 4 ----
test_transport_dinos_9clusters_4 <- select(transport_dinos_9clusters_4, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_4) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_4 <- strrep("t", times = 120) 
str(transport_data_4)
t_transport_dinos_9clusters_4 <- cbind (test_transport_dinos_9clusters_4,transport_data_4)
str(t_transport_dinos_9clusters_4)
names(t_transport_dinos_9clusters_4)[1] <- "Temp"
names(t_transport_dinos_9clusters_4)[2] <- "Ice"
names(t_transport_dinos_9clusters_4)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_4)[4] <- "Silicate"
names(t_transport_dinos_9clusters_4)[5] <- "Salinity"
names(t_transport_dinos_9clusters_4)[6] <- "Datatype"

# cl 5 ----
test_transport_dinos_9clusters_5 <- select(transport_dinos_9clusters_5, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_5) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_5 <- strrep("t", times = 9) 
str(transport_data_5)
t_transport_dinos_9clusters_5 <- cbind (test_transport_dinos_9clusters_5,transport_data_5)
str(t_transport_dinos_9clusters_5)
names(t_transport_dinos_9clusters_5)[1] <- "Temp"
names(t_transport_dinos_9clusters_5)[2] <- "Ice"
names(t_transport_dinos_9clusters_5)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_5)[4] <- "Silicate"
names(t_transport_dinos_9clusters_5)[5] <- "Salinity"
names(t_transport_dinos_9clusters_5)[6] <- "Datatype"

# cl 6 ----
test_transport_dinos_9clusters_6 <- select(transport_dinos_9clusters_6, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_6) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_6 <- strrep("t", times = 115) 
str(transport_data_6)
t_transport_dinos_9clusters_6 <- cbind (test_transport_dinos_9clusters_6,transport_data_6)
str(t_transport_dinos_9clusters_6)
names(t_transport_dinos_9clusters_6)[1] <- "Temp"
names(t_transport_dinos_9clusters_6)[2] <- "Ice"
names(t_transport_dinos_9clusters_6)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_6)[4] <- "Silicate"
names(t_transport_dinos_9clusters_6)[5] <- "Salinity"
names(t_transport_dinos_9clusters_6)[6] <- "Datatype"

# cl 7 ----
test_transport_dinos_9clusters_7 <- select(transport_dinos_9clusters_7, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_7) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_7 <- strrep("t", times = 58) 
str(transport_data_7)
t_transport_dinos_9clusters_7 <- cbind (test_transport_dinos_9clusters_7,transport_data_7)
str(t_transport_dinos_9clusters_7)
names(t_transport_dinos_9clusters_7)[1] <- "Temp"
names(t_transport_dinos_9clusters_7)[2] <- "Ice"
names(t_transport_dinos_9clusters_7)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_7)[4] <- "Silicate"
names(t_transport_dinos_9clusters_7)[5] <- "Salinity"
names(t_transport_dinos_9clusters_7)[6] <- "Datatype"

# cl 8 ----
test_transport_dinos_9clusters_8 <- select(transport_dinos_9clusters_8, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_8) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_8 <- strrep("t", times = 72) 
str(transport_data_8)
t_transport_dinos_9clusters_8 <- cbind (test_transport_dinos_9clusters_8,transport_data_8)
str(t_transport_dinos_9clusters_8)
names(t_transport_dinos_9clusters_8)[1] <- "Temp"
names(t_transport_dinos_9clusters_8)[2] <- "Ice"
names(t_transport_dinos_9clusters_8)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_8)[4] <- "Silicate"
names(t_transport_dinos_9clusters_8)[5] <- "Salinity"
names(t_transport_dinos_9clusters_8)[6] <- "Datatype"

# cl 9 ----
test_transport_dinos_9clusters_9 <- select(transport_dinos_9clusters_9, Transport_Temp_median, Transport_Ice_presence_median, Transport_Nitrate_median, Transport_Silicate_median, Transport_Salinity_median)
str(test_transport_dinos_9clusters_9) # here you have to check how many observations you have and potentially change the times in the next row
transport_data_9 <- strrep("t", times = 59) 
str(transport_data_9)
t_transport_dinos_9clusters_9 <- cbind (test_transport_dinos_9clusters_9,transport_data_9)
str(t_transport_dinos_9clusters_9)
names(t_transport_dinos_9clusters_9)[1] <- "Temp"
names(t_transport_dinos_9clusters_9)[2] <- "Ice"
names(t_transport_dinos_9clusters_9)[3] <- "Nitrate"
names(t_transport_dinos_9clusters_9)[4] <- "Silicate"
names(t_transport_dinos_9clusters_9)[5] <- "Salinity"
names(t_transport_dinos_9clusters_9)[6] <- "Datatype"

# 8.2 surface data ----

# cl 1 ----
str(sh21_surface_9Cluster1)

test_surface_dinos_9clusters_1 <- select(sh21_surface_9Cluster1, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_1) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_1 <- strrep("s", times = 72) 
str(surface_data_1)
t_surface_dinos_9clusters_1 <- cbind (test_surface_dinos_9clusters_1,surface_data_1)
str(t_surface_dinos_9clusters_1)
names(t_surface_dinos_9clusters_1)[1] <- "Temp"
names(t_surface_dinos_9clusters_1)[2] <- "Ice"
names(t_surface_dinos_9clusters_1)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_1)[4] <- "Silicate"
names(t_surface_dinos_9clusters_1)[5] <- "Salinity"
names(t_surface_dinos_9clusters_1)[6] <- "Datatype"

# cl 2 ----
test_surface_dinos_9clusters_2 <- select(sh21_surface_9Cluster2, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_2) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_2 <- strrep("s", times = 64) 
str(surface_data_2)
t_surface_dinos_9clusters_2 <- cbind (test_surface_dinos_9clusters_2,surface_data_2)
str(t_surface_dinos_9clusters_2)
names(t_surface_dinos_9clusters_2)[1] <- "Temp"
names(t_surface_dinos_9clusters_2)[2] <- "Ice"
names(t_surface_dinos_9clusters_2)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_2)[4] <- "Silicate"
names(t_surface_dinos_9clusters_2)[5] <- "Salinity"
names(t_surface_dinos_9clusters_2)[6] <- "Datatype"

# cl 3 ----
test_surface_dinos_9clusters_3 <- select(sh21_surface_9Cluster3, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_3) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_3 <- strrep("s", times = 86) 
str(surface_data_3)
t_surface_dinos_9clusters_3 <- cbind (test_surface_dinos_9clusters_3,surface_data_3)
str(t_surface_dinos_9clusters_3)
names(t_surface_dinos_9clusters_3)[1] <- "Temp"
names(t_surface_dinos_9clusters_3)[2] <- "Ice"
names(t_surface_dinos_9clusters_3)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_3)[4] <- "Silicate"
names(t_surface_dinos_9clusters_3)[5] <- "Salinity"
names(t_surface_dinos_9clusters_3)[6] <- "Datatype"

# cl 4 ----
test_surface_dinos_9clusters_4 <- select(sh21_surface_9Cluster4, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_4) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_4 <- strrep("s", times = 120) 
str(surface_data_4)
t_surface_dinos_9clusters_4 <- cbind (test_surface_dinos_9clusters_4,surface_data_4)
str(t_surface_dinos_9clusters_4)
names(t_surface_dinos_9clusters_4)[1] <- "Temp"
names(t_surface_dinos_9clusters_4)[2] <- "Ice"
names(t_surface_dinos_9clusters_4)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_4)[4] <- "Silicate"
names(t_surface_dinos_9clusters_4)[5] <- "Salinity"
names(t_surface_dinos_9clusters_4)[6] <- "Datatype"

# cl 5 ----
test_surface_dinos_9clusters_5 <- select(sh21_surface_9Cluster5, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_5) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_5 <- strrep("s", times = 9) 
str(surface_data_5)
t_surface_dinos_9clusters_5 <- cbind (test_surface_dinos_9clusters_5,surface_data_5)
str(t_surface_dinos_9clusters_5)
names(t_surface_dinos_9clusters_5)[1] <- "Temp"
names(t_surface_dinos_9clusters_5)[2] <- "Ice"
names(t_surface_dinos_9clusters_5)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_5)[4] <- "Silicate"
names(t_surface_dinos_9clusters_5)[5] <- "Salinity"
names(t_surface_dinos_9clusters_5)[6] <- "Datatype"

# cl 6 ----
test_surface_dinos_9clusters_6 <- select(sh21_surface_9Cluster6, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_6) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_6 <- strrep("s", times = 115) 
str(surface_data_6)
t_surface_dinos_9clusters_6 <- cbind (test_surface_dinos_9clusters_6,surface_data_6)
str(t_surface_dinos_9clusters_6)
names(t_surface_dinos_9clusters_6)[1] <- "Temp"
names(t_surface_dinos_9clusters_6)[2] <- "Ice"
names(t_surface_dinos_9clusters_6)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_6)[4] <- "Silicate"
names(t_surface_dinos_9clusters_6)[5] <- "Salinity"
names(t_surface_dinos_9clusters_6)[6] <- "Datatype"

# cl 7 ----
test_surface_dinos_9clusters_7 <- select(sh21_surface_9Cluster7, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_7) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_7 <- strrep("s", times = 58) 
str(surface_data_7)
t_surface_dinos_9clusters_7 <- cbind (test_surface_dinos_9clusters_7,surface_data_7)
str(t_surface_dinos_9clusters_7)
names(t_surface_dinos_9clusters_7)[1] <- "Temp"
names(t_surface_dinos_9clusters_7)[2] <- "Ice"
names(t_surface_dinos_9clusters_7)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_7)[4] <- "Silicate"
names(t_surface_dinos_9clusters_7)[5] <- "Salinity"
names(t_surface_dinos_9clusters_7)[6] <- "Datatype"

# cl 8 ----
test_surface_dinos_9clusters_8 <- select(sh21_surface_9Cluster8, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_8) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_8 <- strrep("s", times = 72) 
str(surface_data_8)
t_surface_dinos_9clusters_8 <- cbind (test_surface_dinos_9clusters_8,surface_data_8)
str(t_surface_dinos_9clusters_8)
names(t_surface_dinos_9clusters_8)[1] <- "Temp"
names(t_surface_dinos_9clusters_8)[2] <- "Ice"
names(t_surface_dinos_9clusters_8)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_8)[4] <- "Silicate"
names(t_surface_dinos_9clusters_8)[5] <- "Salinity"
names(t_surface_dinos_9clusters_8)[6] <- "Datatype"

# cl 9 ----
test_surface_dinos_9clusters_9 <- select(sh21_surface_9Cluster9, Temp_median, Ice_presence_median, Nitrate_median, Silicate_median, Salinity_median)
str(test_surface_dinos_9clusters_9) # here you have to check how many observations you have and potentially change the times in the next row
surface_data_9 <- strrep("s", times = 59) 
str(surface_data_9)
t_surface_dinos_9clusters_9 <- cbind (test_surface_dinos_9clusters_9,surface_data_9)
str(t_surface_dinos_9clusters_9)
names(t_surface_dinos_9clusters_9)[1] <- "Temp"
names(t_surface_dinos_9clusters_9)[2] <- "Ice"
names(t_surface_dinos_9clusters_9)[3] <- "Nitrate"
names(t_surface_dinos_9clusters_9)[4] <- "Silicate"
names(t_surface_dinos_9clusters_9)[5] <- "Salinity"
names(t_surface_dinos_9clusters_9)[6] <- "Datatype"

# 8.3 COMBINE ----

# cl 1 ----
t_dinos_9clusters_1 <- smartbind (t_transport_dinos_9clusters_1, t_surface_dinos_9clusters_1) 
str(t_dinos_9clusters_1) 
t_dinos_9clusters_1_long <- pivot_longer(t_dinos_9clusters_1, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_1_long$env_parameter <- factor(t_dinos_9clusters_1_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_1_long)

# cl 2 ----
t_dinos_9clusters_2 <- smartbind (t_transport_dinos_9clusters_2, t_surface_dinos_9clusters_2) 
str(t_dinos_9clusters_2) 
t_dinos_9clusters_2_long <- pivot_longer(t_dinos_9clusters_2, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_2_long$env_parameter <- factor(t_dinos_9clusters_2_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_2_long)
# cl 3 ----
t_dinos_9clusters_3 <- smartbind (t_transport_dinos_9clusters_3, t_surface_dinos_9clusters_3) 
str(t_dinos_9clusters_3) 
t_dinos_9clusters_3_long <- pivot_longer(t_dinos_9clusters_3, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_3_long$env_parameter <- factor(t_dinos_9clusters_3_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_3_long)

# cl 4 ----
t_dinos_9clusters_4 <- smartbind (t_transport_dinos_9clusters_4, t_surface_dinos_9clusters_4) 
str(t_dinos_9clusters_4) 
t_dinos_9clusters_4_long <- pivot_longer(t_dinos_9clusters_4, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_4_long$env_parameter <- factor(t_dinos_9clusters_4_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_4_long)

# cl 5 ----
t_dinos_9clusters_5 <- smartbind (t_transport_dinos_9clusters_5, t_surface_dinos_9clusters_5) 
str(t_dinos_9clusters_5) 
t_dinos_9clusters_5_long <- pivot_longer(t_dinos_9clusters_5, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_5_long$env_parameter <- factor(t_dinos_9clusters_5_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_5_long)

# cl 6 ----
t_dinos_9clusters_6 <- smartbind (t_transport_dinos_9clusters_6, t_surface_dinos_9clusters_6) 
str(t_dinos_9clusters_6) 
t_dinos_9clusters_6_long <- pivot_longer(t_dinos_9clusters_6, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_6_long$env_parameter <- factor(t_dinos_9clusters_6_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_6_long)

# cl 7 ----
t_dinos_9clusters_7 <- smartbind (t_transport_dinos_9clusters_7, t_surface_dinos_9clusters_7) 
str(t_dinos_9clusters_7) 
t_dinos_9clusters_7_long <- pivot_longer(t_dinos_9clusters_7, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_7_long$env_parameter <- factor(t_dinos_9clusters_7_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_7_long)

# cl 8 ----
t_dinos_9clusters_8 <- smartbind (t_transport_dinos_9clusters_8, t_surface_dinos_9clusters_8) 
str(t_dinos_9clusters_8) 
t_dinos_9clusters_8_long <- pivot_longer(t_dinos_9clusters_8, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_8_long$env_parameter <- factor(t_dinos_9clusters_8_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_8_long)

# cl 9 ----
t_dinos_9clusters_9 <- smartbind (t_transport_dinos_9clusters_9, t_surface_dinos_9clusters_9) 
str(t_dinos_9clusters_9) 
t_dinos_9clusters_9_long <- pivot_longer(t_dinos_9clusters_9, cols = c("Temp", "Ice", "Nitrate", "Silicate", "Salinity"), names_to = "env_parameter", values_to = "values")

t_dinos_9clusters_9_long$env_parameter <- factor(t_dinos_9clusters_9_long$env_parameter,      # Reordering group factor levels
                                                 levels = c("Temp","Ice", "Salinity", "Nitrate", "Silicate"))
str(t_dinos_9clusters_9_long)

# 8.4 plots ----

# 8.4.1 change plots  ----
str(t_dinos_9clusters_1_long)

t_dinos_9clusters_1_long <- mutate(t_dinos_9clusters_1_long, facet_label = fct_recode(env_parameter,
                                   "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                   "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_2_long <- mutate(t_dinos_9clusters_2_long, facet_label = fct_recode(env_parameter,
                                    "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                    "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_3_long <- mutate(t_dinos_9clusters_3_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_4_long <- mutate(t_dinos_9clusters_4_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_5_long <- mutate(t_dinos_9clusters_5_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_6_long <- mutate(t_dinos_9clusters_6_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_7_long <- mutate(t_dinos_9clusters_7_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_8_long <- mutate(t_dinos_9clusters_8_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))

t_dinos_9clusters_9_long <- mutate(t_dinos_9clusters_9_long, facet_label = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp", "Sea ice presence (%)" = "Ice", "Salinity (psu)" = "Salinity", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate", "Silicate (mmol/m3)" = "Silicate"))
# PLOT NAMES ----
#"Sant" = "2", "Imin" = "5", "high-Brig" = "3", 
#"Nlab" = "8", "low-Brig" = "4", "low-Ocen" = "6", 
#"Iacu" = "7", "high-Ocen" = "1", "Spin" = "9"

# cl 1 ----

combi_t_dinos_9clusters_1 <- ggplot(t_dinos_9clusters_1_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 1",# here, make sure that it's the right title
       x = " ", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
        text = element_text(size = 16))
combi_t_dinos_9clusters_1
#ggsave(plot = combi_t_dinos_9clusters_1, filename = "combi_t_dinos_9clusters_1.pdf", width = 60, height = 30, unit = "cm")

# cl 2 ----
combi_t_dinos_9clusters_2 <- ggplot(t_dinos_9clusters_2_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 2",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
combi_t_dinos_9clusters_2
#ggsave(plot = combi_t_dinos_9clusters_2, filename = "combi_t_dinos_9clusters_2.pdf", width = 60, height = 30, unit = "cm")

# cl 3 ----
combi_t_dinos_9clusters_3 <- ggplot(t_dinos_9clusters_3_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 3",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_3
#ggsave(plot = combi_t_dinos_9clusters_3, filename = "combi_t_dinos_9clusters_3.pdf", width = 60, height = 30, unit = "cm")

# cl 4 ----
combi_t_dinos_9clusters_4 <- ggplot(t_dinos_9clusters_4_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 4",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_4
#ggsave(plot = combi_t_dinos_9clusters_4, filename = "combi_t_dinos_9clusters_4.pdf", width = 60, height = 30, unit = "cm")

# cl 5 ----
combi_t_dinos_9clusters_5 <- ggplot(t_dinos_9clusters_5_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 5",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_5
#ggsave(plot = combi_t_dinos_9clusters_5, filename = "combi_t_dinos_9clusters_5.pdf", width = 60, height = 30, unit = "cm")

# cl 6 ----
combi_t_dinos_9clusters_6 <- ggplot(t_dinos_9clusters_6_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 6",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
combi_t_dinos_9clusters_6
#ggsave(plot = combi_t_dinos_9clusters_6, filename = "combi_t_dinos_9clusters_6.pdf", width = 60, height = 30, unit = "cm")

# cl 7 ----
combi_t_dinos_9clusters_7 <- ggplot(t_dinos_9clusters_7_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 7",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_7
#ggsave(plot = combi_t_dinos_9clusters_7, filename = "combi_t_dinos_9clusters_7.pdf", width = 60, height = 30, unit = "cm")

# cl 8 ----
combi_t_dinos_9clusters_8 <- ggplot(t_dinos_9clusters_8_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 8",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_8
#ggsave(plot = combi_t_dinos_9clusters_8, filename = "combi_t_dinos_9clusters_8.pdf", width = 60, height = 30, unit = "cm")

# cl 9 ----
combi_t_dinos_9clusters_9 <- ggplot(t_dinos_9clusters_9_long, aes(x = facet_label, y = values, fill = Datatype)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 1, scales = "free") + 
  scale_fill_brewer(guide = "none") +
  labs(title = "Cluster 9",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 16))
#combi_t_dinos_9clusters_9
#ggsave(plot = combi_t_dinos_9clusters_9, filename = "combi_t_dinos_9clusters_9.pdf", width = 60, height = 30, unit = "cm")

# all together----
# check if this is the order of plots you want to have

#"Sant" = "2", "Imin" = "5", "high-Brig" = "3", 
#"Nlab" = "8", "low-Brig" = "4", "low-Ocen" = "6", 
#"Iacu" = "7", "high-Ocen" = "1", "Spin" = "9"
combi_s_t_env_parameters <- plot_grid (combi_t_dinos_9clusters_2,combi_t_dinos_9clusters_5,combi_t_dinos_9clusters_3,
                                       combi_t_dinos_9clusters_8,combi_t_dinos_9clusters_4,combi_t_dinos_9clusters_6,
                                       combi_t_dinos_9clusters_7,combi_t_dinos_9clusters_1,combi_t_dinos_9clusters_9,
                                       align = "v", ncol = 3)
combi_s_t_env_parameters
ggsave(plot = combi_s_t_env_parameters, filename = "Fig7_combi_s_t_env_parameters.pdf", width = 70, height = 55, unit = "cm")


# 9. CLUSTER PER ENVIRONMENTAL VARIABLE ----

str(sh21_surface_9Cluster1)
str(sh21_surface_9Cluster)
str(sh21_transport_9Cluster)

prep_sh21_transport_9Cluster <- select(sh21_transport_9Cluster, contains("median"))
prep_sh21_surface_9Cluster <- select(sh21_surface_9Cluster, cluster, contains("median"))

surface_prep_sh21_all_9Cluster_long <- pivot_longer(prep_sh21_surface_9Cluster, cols = c("Temp_median", "Ice_presence_median", "Nitrate_median", "Silicate_median", "Salinity_median"), names_to = "env_parameter", values_to = "values")
transport_prep_sh21_all_9Cluster_long <- pivot_longer(prep_sh21_transport_9Cluster, cols = c("Transport_Temp_median", "Transport_Ice_presence_median", "Transport_Nitrate_median", "Transport_Silicate_median", "Transport_Salinity_median"), names_to = "env_parameter", values_to = "values")
str(surface_prep_sh21_all_9Cluster_long)

# 9.1 surface ----

str(t_surface_dinos_9clusters_1)

surface_prep_sh21_all_9Cluster_long = mutate(surface_prep_sh21_all_9Cluster_long, facet_label_cluster = fct_recode(cluster,
                                                                                                       "Sant" = "2", "Imin" = "5", "high-Brig" = "3", 
                                                                                                       "Nlab" = "8", "low-Brig" = "4", "low-Ocen" = "6", 
                                                                                                       "Iacu" = "7", "high-Ocen" = "1", "Spin" = "9"))



surface_prep_sh21_all_9Cluster_long$facet_label_clusters = factor(surface_prep_sh21_all_9Cluster_long$facet_label_cluster, levels = c("Sant", "Imin",  "high-Brig",
                                                                                                                                 "Nlab", "low-Brig", "low-Ocen", 
                                                                                                                                 "Iacu", "high-Ocen", "Spin"))

surface_prep_sh21_all_9Cluster_long$env_parameter <- factor(surface_prep_sh21_all_9Cluster_long$env_parameter, levels = c("Temp_median", "Ice_presence_median", "Salinity_median", "Nitrate_median", "Silicate_median"))
surface_prep_sh21_all_9Cluster_long <- mutate(surface_prep_sh21_all_9Cluster_long, facet_label_ec = fct_recode(env_parameter,
                                                                                      "SST (°C)" = "Temp_median", "Sea ice presence (%)" = "Ice_presence_median", "Salinity (psu)" = "Salinity_median", 
                                                                                      "Nitrate (mmol/m3)" = "Nitrate_median", "Silicate (mmol/m3)" = "Silicate_median"))


eps_in_9Clusters <- ggplot(surface_prep_sh21_all_9Cluster_long, aes(x = facet_label_ec, y = values, fill = facet_label_clusters)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label_ec, nrow = 2, scales = "free") + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = "Surface Environmental Conditions",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  #theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 20))
eps_in_9Clusters
ggsave(plot = eps_in_9Clusters, filename = "Fig9_eps_in9Clusters.pdf", width =55, height = 25, unit = "cm")


# 9.2 transport ----
transport_prep_sh21_all_9Cluster_long = mutate(transport_prep_sh21_all_9Cluster_long, facet_label_cluster = fct_recode(cluster,
                                                                                                                   "Sant" = "2", "Imin" = "5", "high-Brig" = "3", 
                                                                                                                   "Nlab" = "8", "low-Ocen" = "4", "low-Brig" = "6", 
                                                                                                                   "Iacu" = "7", "high-Ocen" = "1", "Spin" = "9"))

transport_prep_sh21_all_9Cluster_long$facet_label_clusters = factor(transport_prep_sh21_all_9Cluster_long$facet_label_cluster, levels = c("Sant", "Imin",  "high-Brig",
                                                                                                                                      "Nlab", "low-Ocen", "low-Brig", 
                                                                                                                                      "Iacu", "high-Ocen", "Spin"))

transport_prep_sh21_all_9Cluster_long$env_parameter <- factor(transport_prep_sh21_all_9Cluster_long$env_parameter, levels = c("Transport_Temp_median", "Transport_Ice_presence_median", "Transport_Salinity_median", "Transport_Nitrate_median", "Transport_Silicate_median"))
transport_prep_sh21_all_9Cluster_long <- mutate(transport_prep_sh21_all_9Cluster_long, facet_label_ec = fct_recode(env_parameter,
                                                                                                               "SST (°C)" = "Transport_Temp_median", "Sea ice presence (%)" = "Transport_Ice_presence_median", "Salinity (psu)" = "Transport_Salinity_median", 
                                                                                                               "Nitrate (mmol/m3)" = "Transport_Nitrate_median", "Silicate (mmol/m3)" = "Transport_Silicate_median"))


ept_in_9Clusters <- ggplot(transport_prep_sh21_all_9Cluster_long, aes(x = facet_label_ec, y = values, fill = facet_label_clusters)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label_ec, nrow = 2, scales = "free") + 
  scale_fill_viridis(discrete = TRUE, option = "plasma") +
  labs(title = "Transport Environmental Conditions",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  #theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 20))
ept_in_9Clusters
#ggsave(plot = ept_in_9Clusters, filename = "Fig9_ept_in9Clusters.pdf", width =55, height = 25, unit = "cm")


# 10. WSI ENVIRONMENTAL PARAMETERS IN CLUSTERS ----

head(wsie_5Cluster)
str(wsie_5Cluster)

wsie_5Cluster_long <- pivot_longer(wsie_5Cluster, cols = c("Temp_median", "Ice_presence_median", "Salinity_median", "Nitrate_median", "Silicate_median"), names_to = "env_parameter", values_to = "values")
view(wsie_5Cluster_long)

wsie_5Cluster_long <- select(wsie_5Cluster_long, wsie_cluster, env_parameter, values)                         
str(wsie_5Cluster_long)

wsie_5Cluster_long$env_parameter <- factor(wsie_5Cluster_long$env_parameter, levels = c("Temp_median", "Ice_presence_median", "Salinity_median", "Nitrate_median", "Silicate_median"))
wsie_5Cluster_long <- mutate(wsie_5Cluster_long, facet_label = fct_recode(env_parameter,
                                                                          "SST (°C)" = "Temp_median", "Sea ice presence (%)" = "Ice_presence_median", "Salinity (psu)" = "Salinity_median", 
                                                                          "Nitrate (mmol/m3)" = "Nitrate_median", "Silicate (mmol/m3)" = "Silicate_median"))

str(wsie_5Cluster_long)

wsie_5Cluster_eps <- ggplot(wsie_5Cluster_long, aes(x = facet_label, y = values, fill = wsie_cluster)) + 
  geom_boxplot() + 
  facet_wrap (~facet_label, nrow = 5, scales = "free") + 
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Winter Sea Ice Edge - Environmental Conditions",# here, make sure that it's the right title
       x = "", y = " ", fill = "") +
  #theme_bw() +
  theme(#axis.text.x = element_blank (),
    strip.background = element_blank(),
    strip.text = element_blank(),
    text = element_text(size = 20))
wsie_5Cluster_eps
ggsave(plot = wsie_5Cluster_eps, filename = "Fig8_surface_wsi_5Clusters.pdf", width =25, height = 45, unit = "cm")

################################################################################################################## ----
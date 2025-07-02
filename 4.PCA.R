library(vegan)
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(ggvegan)

#### Loading the data ----
# Loading the species dataset
species <- read_csv("data/species_reduced.csv") %>% 
  arrange(LakeID)#Read the file
lakeid <- species$LakeID
species$LakeID = NULL

# Loading the environmental dataset
env_variables <- read_csv("data/NF_lakes_conditions_wide.csv") %>% 
  select(!c(Latitude, Longitude, Cond, Temp)) %>% 
  arrange(LakeID)

env_lakeid <- env_variables$LakeID
env_variables$LakeID <- NULL 

#### PCA on environmental data ----
# Log transformation of the "Area" and "Sal"
env_variables_log <- env_variables %>%
  mutate(log_area = log(Area),
         log_sal = log(Sal),
         log_DTS = log(DistanceToTheSea),
         sqrt_sand = sqrt(SandGrains)) %>%
  select(-c(Area, Sal, SandGrains, DistanceToTheSea))

env_variables_stand <- decostand(env_variables_log, method = "standardize")

env_pca <- rda(env_variables_stand)

# Broken stick model
screeplot(env_pca, bstick = TRUE, col = "darkgreen", bg = "grey40")#two significant axis
env_fort_sites <- fortify(env_pca, axes(1,2), scaling = "sites") # Gives first two significant PCA axes

# Holding the PC1, PC2 coordinates
env_sites_sites <- env_fort_sites[env_fort_sites$score %in% "sites",] # Holds the coordinates PC1, PC1, site scores
env_sp_sites <- env_fort_sites[env_fort_sites$score %in% "species",] # Holds the coordinates PC1, PC2 for species

# Explained variancy
env_ve_prep <- env_pca$CA$eig / env_pca$tot.chi * 100 # Counts the share of explained variancy (eigienvaluea - total.chi.squere*100.
(env_PC1_ve <- round(((env_ve_prep / sum(env_ve_prep))[c(1)]) * 100, digits = 1)) #30% variancy value for the first principal component
(env_PC2_ve <- round(((env_ve_prep / sum(env_ve_prep))[c(2)]) * 100, digits = 1)) #19.9% variancy value for the second principal component

# Fortifying species data into data frame for plotting
env_fort_sp <- fortify(env_pca, axes(1,2), scaling = "species") # fortifying into data frame
env_sites_sp <- env_fort_sites[env_fort_sp$score %in% "sites",] # getting the sites scores
env_sp_sp <- env_fort_sites[env_fort_sp$score %in% "species",] # getting the species scores

#### PCA plot preparation ----
# PCA plot for sites, scaling = sites

variable_names <- colnames(env_sp_sites$label)

env_sp_sites$label
# Variables name change
new_var_names <- list("Depth" = "Depth", 
                      "log_DTS" = "DTTO", 
                      "N" = "TN",
                      "P" = "TP",
                      "SiO2" = "SiO2",
                      "DOC" = "DOC",
                      "ODO" = "ODO",
                      "pH" = "pH",
                      "LOI" = "LOI550",
                      "log_area" = "Area",
                      "log_sal" = "Salinity",
                      "sqrt_sand" = "Sand grain counts")

# Label variable name change
env_sp_sites <- env_sp_sites %>%
  mutate(label = recode(label, !!!new_var_names))

env_sites_sites$combined_score <- sqrt(env_sites_sites$PC1^2 + env_sites_sites$PC2^2)

lakeid <- c("SGL02", "SGL05", "SGL06", "SGL07", "SGL08", "SGL100", "SGL102",
            "SGL103", "SGL104", "SGL105", "SGL106", "SGL107", "SGL108", "SGL109",
            "SGL110", "SGL111", "SGL112", "SGL113", "SGL114", "SGL115", "SGL116",
            "SGL118", "SGL119", "SGL120", "SGL121", "SGL122", "SGL123", "SGL124",
            "SGL125", "SGL126", "SGL127", "SGL128", "SGL129", "SGL13", "SGL14",
            "SGL15", "SGL16", "SGL17", "SGL21", "SGL22", "SGL23", "SGL24",
            "SGL25", "SGL26", "TAYLORSPOND", "TL08", "TL09", "TL101", "TL18",
            "TL27")

# LakeID merge doPCA
env_sites_sites$LakeID <- lakeid

# Categories
category_1 <- c("SGL02", "SGL05", "SGL06", "SGL07", "SGL08", "SGL102",
                "SGL103", "SGL106", "SGL107", "SGL108", "SGL109",
                "SGL110", "SGL111", "SGL112")

category_2 <- c("SGL100", "SGL104", "SGL105", "SGL113", "SGL114", "SGL115", "SGL116", "SGL118", 
                "SGL119", "SGL120", "SGL121", "SGL122", "SGL123", "SGL124","SGL125", "SGL126",
                "SGL127", "SGL128", "SGL129","SGL13", "SGL14","SGL15", "SGL16", "SGL17", "SGL21",
                "SGL22", "SGL23", "SGL24","SGL25", "SGL26", "TAYLORSPOND", "TL08", "TL09", "TL101",
                "TL18", "TL27")

env_sites_sites <- env_sites_sites %>%
  mutate(category = case_when(
    LakeID %in% category_1 ~ "Category 1",
    LakeID %in% category_2 ~ "Category 2",
    TRUE ~ "Unclassified"
  ))

env_sites_sites$category <- factor(env_sites_sites$category)
#### PCA plot ----
env_pca_sites_plot <- ggplot() +
  labs(y = paste("PC2 (", env_PC2_ve, "%)", sep = ""), 
       x = paste("PC1 (", env_PC1_ve, "%)", sep = ""), 
       title = NULL,  
       color = "Ecoregion") +  # Set the legend title to "Ecoregion"
  geom_segment(data = env_sp_sites,
               color = "black", size = 0.4,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = grid::arrow(length = grid::unit(0.25, "cm"))) +
  geom_point(data = env_sites_sites, aes(x = PC1, y = PC2, color = category), 
             size = 1.5) +  
  scale_color_manual(values = c("Category 1" = "darkblue", 
                                "Category 2" = "darkred", 
                                "Unclassified" = "grey"), 
                     labels = c("Category 1" = "Maritime Barren", 
                                "Category 2" = "Eastern Hyper-Oceanic Barren",
                                "Unclassified" = "Unclassified Label")) +
  guides(color = guide_legend(title = "Ecoregion")) +  # Change the legend title to "Ecoregion"
  ggrepel::geom_text_repel(data = env_sites_sites, aes(x = PC1, y = PC2, label = LakeID, color = category), 
                           size = 2.5, segment.alpha = 0) +
  ggrepel::geom_text_repel(data = env_sp_sites, color = "black",
                           size = 2.5, segment.alpha = 0,
                           aes(x = PC1, y = PC2, label = label)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  coord_equal() +
  theme(panel.background = element_rect(fill = "grey99", color = "black"),
        panel.grid.major = element_line(color = "gray50", linewidth = 0.2),
        panel.grid.minor = element_line(color = "gray50", linewidth = 0.2),
        legend.position = "bottom")

print(env_pca_sites_plot)

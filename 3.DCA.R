library(vegan)
library(ggplot2)
library(ggrepel)
library(readxl)
library(tidyverse)

#### Loading the data ----
# Loading the species
species <- read_csv("data/species_counts.csv")
lakeid <- species$LakeID
species$LakeID <- NULL
counting_sums <- rowSums(species)
species_proc <- t(apply(species, 1, function(x) (x / counting_sums) * 100))

# Loading the environmental variables
env_variables <- read_csv("data/NF_lakes_conditions_wide.csv") %>% 
  select(!c(Latitude, Longitude, Cond, Temp)) %>% 
  arrange(LakeID)

env_lakeid <- env_variables$LakeID
env_variables$LakeID <- NULL 

# Log transformation for "DistanceToTheSea", "Area" and "Sal"
env_variables_log <- env_variables %>%
  mutate(log_area = log(Area),
         log_sal = log(Sal),
         log_DTS = log(DistanceToTheSea),
         sqrt_sand = sqrt(SandGrains)) %>%
  select(-c(Area, Sal, SandGrains))

#### CCA $ DCA ----
# CCA
(spe.ca <- cca(species))
summary(spe.ca) #deafault scaling 2
summary(spe.ca, scaling = 1)

# DCA
DCA <- decorana(sqrt(species_proc), iweigh = 1)
summary(DCA)

#### ggplot DCA ----
dca_data <- as.data.frame(scores(DCA, display = "sites"))
dca_data$LakeID <- lakeid #LakeID as columns

# eigenvalues
eigenvalues <- DCA$evals
first_two_axis <- sum(eigenvalues[1:2])
all_axis_sum <- sum(eigenvalues)
first_two_axis_percent <- (sum(eigenvalues[1:2]) / sum(eigenvalues)) * 100

species_data <- as.data.frame(scores(DCA, display = "species"))
species_data$species <- rownames(species_data)

dca_plot <- ggplot() +
  geom_point(data = dca_data, aes(x = DCA1, y = DCA2, label = LakeID), shape = 16, size = 2, color = "gray25") +
  geom_text_repel(data = dca_data, aes(x = DCA1, y = DCA2, label = LakeID), size = 2.5, color = "black", segment.color = "grey50") +
  geom_point(data = species_data, aes(x = DCA1, y = DCA2, label = species), shape = 17, size = 2, color = "grey75") +
  geom_text_repel(data = species_data, aes(x = DCA1, y = DCA2, label = species), size = 2.5, color = "black", segment.color = "grey50", fontface = "italic") +
  labs(x = paste("DCA Axis 1 (Eigenvalue: ", round(eigenvalues[1], 2), ")", sep = ""),
       y = paste("DCA Axis 2 (Eigenvalue: ", round(eigenvalues[2], 2), ")", sep = ""),
       title = "DCA Plot",
       subtitle = "Detrended Correspondence Analysis") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))

print(dca_plot)

# Env. var. vectors
envfit_result <- envfit(DCA, env_variables_log, permutations = 999)
envfit_scores <- as.data.frame(scores(envfit_result, display = "vectors"))

dca_plot_with_vectors <- dca_plot +
  geom_segment(data = envfit_scores, aes(x = 0, xend = DCA1, y = 0, yend = DCA2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "darkred") +
  geom_text(data = envfit_scores, aes(x = DCA1, y = DCA2, label = rownames(envfit_scores)),
            color = "darkred", size = 5, vjust = -1)

print(dca_plot_with_vectors)
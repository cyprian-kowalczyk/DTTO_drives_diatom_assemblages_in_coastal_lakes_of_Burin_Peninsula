library(vegan)
library(tidyverse)
library(ggrepel)
library(ggvegan)
library(ggcorrplot) 
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(tibble)

#### Loading the data ----
#Loading the species dataset
species <- read_csv("data/species_reduced.csv") %>% 
  arrange(LakeID)#Read the file
lakeid <- species$LakeID
species$LakeID = NULL
species_sqrt <- sqrt(species)

#Loading the environmental dataset
env_variables <- read_csv("data/NF_lakes_conditions_wide.csv") %>% 
  select(!c(Latitude, Longitude, Cond, Temp)) %>% 
  arrange(LakeID) #Read the file 
env_lakeid <- env_variables$LakeID
env_variables$LakeID <- NULL 
env_variables_log <- env_variables %>% 
  mutate(log_area = log(Area), log_sal = log(Sal), sqrt_sand = sqrt(SandGrains), log_DTS = log(DistanceToTheSea)) %>% 
  select(!c(Area, Sal, SandGrains, DistanceToTheSea))

#Standarization
env_variables_stand <- decostand(env_variables_log, method = "standardize")

#Correlation matrix
correlation_matrix <- cor(env_variables_stand)
ggcorrplot(correlation_matrix, method ="square", insig = "blank",  
           colors = c("red","blue", brewer.pal(9, "YlGnBu")))

#### RDA ----
spe_rda_all <- rda(species_sqrt ~ ., data = env_variables_stand)
summary(spe_rda_all)
(R2a_all <- RsquareAdj(spe_rda_all)$adj.r.squared) #0.2570973
mod0 <- rda(species_sqrt ~ 1, data = env_variables_stand)

#Monte Carlo permutation Forward selection
set.seed(1000)
step_forward <- ordistep(mod0,
                         scope = formula(spe_rda_all),
                         direction = "forward",
                         permutations = how(nperm = 999) ) #P, DOC, ODO, LOI excluded based on forward selection
(RsquareAdj(step_forward)$adj.r.squared) #0.2373785 NOWE 0.2441508

#### PartialRDA ----------
env_variables_forward_selected <- env_variables_stand %>% 
  select(!c(ODO, DOC, P, LOI)) #insignificant ones

rda_fs <- rda(species_sqrt ~ ., data = env_variables_forward_selected)
(R2a_all_fs <- RsquareAdj(rda_fs)$adj.r.squared) #0.2373785

set.seed(1000)
all_anova_fs <- anova(rda_fs, permutations = 999) 

set.seed(1000)
all_anova_cca <- anova.cca(rda_fs, by = "axis") 

#1Dtts
rda_dtts_fs <- rda(species_sqrt ~ log_DTS + Condition(N, log_sal, Depth, pH, SiO2, SandGrains), data = env_variables_forward_selected)
(R2a_dtts_fs <- RsquareAdj(rda_dtts_fs)$adj.r.squared) #6.5% NOWE 8.85%

set.seed(1000)
dtts_anova_fs <- anova(rda_dtts_fs, permutations = 999) #F Pr(>F) 0.001***

#2N
rda_n_fs <- rda(species_sqrt ~ N + Condition(log_DTS, log_sal, Depth, pH, SiO2, SandGrains), data = env_variables_forward_selected)
(R2a_n_fs <- RsquareAdj(rda_n_fs)$adj.r.squared) #2.9% NOWE 2.3%

set.seed(1000)
n_anova_fs <- anova(rda_n_fs, permutations = 999) #F Pr(>F) 0.002**

#3depth
rda_depth_fs <- rda(species_sqrt ~ Depth + Condition(log_DTS, N, log_sal, pH, SiO2, SandGrains), data = env_variables_forward_selected)
(R2a_depth_fs <- RsquareAdj(rda_depth_fs)$adj.r.squared) #2.0% NOWE 1.8%

set.seed(1000)
depth_anova_fs <- anova(rda_depth_fs, permutations = 999) #F Pr(>F) 0.013* NOWE 0.01**

#4log_sal
rda_sal_fs <- rda(species_sqrt ~ log_sal + Condition(log_DTS, N, Depth, pH, SiO2, SandGrains), data = env_variables_forward_selected)
(R2a_sal_fs <- RsquareAdj(rda_sal_fs)$adj.r.squared) #2.7% NOWE 1.7%

set.seed(1000)
sal_anova_fs <- anova(rda_sal_fs, permutations = 999) #F Pr(>F) 0.003** NOWE 0.01**

#5ph
rda_ph_fs <- rda(species_sqrt ~ pH + Condition(log_DTS, N, log_sal, Depth, SiO2, SandGrains), data = env_variables_forward_selected)
(R2a_ph_fs <- RsquareAdj(rda_ph_fs)$adj.r.squared) #1.6% NOWE 1.7%

set.seed(1000)
ph_anova_fs <- anova(rda_ph_fs, permutations = 999) #F Pr(>F) 0.008** NOWE 0.006**

#6sio2
rda_sio2_fs <- rda(species_sqrt ~ SiO2 + Condition(log_DTS, N, log_sal, Depth, pH, SandGrains), data = env_variables_forward_selected)
(R2a_sio2_fs <- RsquareAdj(rda_sio2_fs)$adj.r.squared) #1.6%

set.seed(1000)
sio2_anova_fs <- anova(rda_sio2_fs, permutations = 999) #F Pr(>F) 0.021* NOWE 0.009**

#7sandgrains
rda_sg_fs <- rda(species_sqrt ~ sqrt_sand + Condition(log_DTS, N, log_sal, Depth, pH, SiO2), data = env_variables_forward_selected)
(R2a_sg_fs <- RsquareAdj(rda_sg_fs)$adj.r.squared) #1.6% NOWE 1.4%

set.seed(1000)
sg_anova_fs <- anova(rda_sg_fs, permutations = 999) #F Pr(>F) 0.018* 0.021*

#8Area
rda_area_fs <- rda(species_sqrt ~ log_area + Condition(log_DTS, N, log_sal, Depth, pH, SandGrains, sio2), data = env_variables_forward_selected)
(R2a_area_fs <- RsquareAdj(rda_area_fs)$adj.r.squared) #1.8% 1.5%

set.seed(1000)
area_anova_fs <- anova(rda_area_fs, permutations = 999) #F Pr(>F) 0.01**

##### Variance partitioning diagram ----
rda_tprep <- tibble("log_DTS" = c(R2a_dtts_fs, dtts_anova_fs$`Pr(>F)`[[1]]),
                    "N" = c(R2a_n_fs, n_anova_fs$`Pr(>F)`[[1]]),
                    "Depth" = c(R2a_depth_fs, depth_anova_fs$`Pr(>F)`[[1]]),
                    "log_sal" = c(R2a_sal_fs, sal_anova_fs$`Pr(>F)`[[1]]),
                    "pH" = c(R2a_ph_fs, ph_anova_fs$`Pr(>F)`[[1]]),
                    "SiO2" = c(R2a_sio2_fs, sio2_anova_fs$`Pr(>F)`[[1]]),
                    "sqrt_sand" = c(R2a_sg_fs, sg_anova_fs$`Pr(>F)`[[1]]),
                    "log_area" = c(R2a_area_fs, sio2_anova_fs$`Pr(>F)`[[1]]),
                    "All" = c(R2a_all_fs, all_anova_fs$`Pr(>F)`[[1]]),
                    "Unexplained" = c(1 - R2a_all_fs, NA))


rda_table <- rda_tprep[1,] %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "proportion_of_v_expl") %>% 
  mutate(Category = factor(Category, 
                           levels = c("log_DTS", "N", "Depth", "log_sal", "pH", "SiO2", "sqrt_sand","log_area", "All", "Unexplained")))

rda_table_sign <- rda_tprep[2,] %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "pval") %>% 
  mutate(category = factor(Category, levels = c("log_DTS", "N", "Depth", "log_sal", "pH", "SiO2", "sqrt_sand","log_area", "All", "Unexplained"))) %>% 
  filter(!category == "Unexplained") %>% 
  mutate(stars = cut(pval, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), label=c("***", "**", "*", "")),
         stars = as.character(stars))

#tibble rda_tprep
rda_tprep <- tibble(
  "log_DTS" = c(R2a_dtts_fs, dtts_anova_fs$`Pr(>F)`[[1]]),
  "N" = c(R2a_n_fs, n_anova_fs$`Pr(>F)`[[1]]),
  "Depth" = c(R2a_depth_fs, depth_anova_fs$`Pr(>F)`[[1]]),
  "log_sal" = c(R2a_sal_fs, sal_anova_fs$`Pr(>F)`[[1]]),
  "pH" = c(R2a_ph_fs, ph_anova_fs$`Pr(>F)`[[1]]),
  "SiO2" = c(R2a_sio2_fs, sio2_anova_fs$`Pr(>F)`[[1]]),
  "sqrt_sand" = c(R2a_sg_fs, sg_anova_fs$`Pr(>F)`[[1]]),
  "log_area" = c(R2a_area_fs, area_anova_fs$`Pr(>F)`[[1]]),
  "All" = c(R2a_all_fs, all_anova_fs$`Pr(>F)`[[1]]),
  "Unexplained" = c(1 - R2a_all_fs, NA)
)

# Renameing the variables
category_names <- c(
  "log_DTS" = "Distance to the ocean",
  "N" = "Total nitrogen",
  "log_sal" = "Salinity",
  "Depth" = "Lake depth",
  "pH" = "pH",
  "SiO2" = "Silica",
  "sqrt_sand" = "Sand grains",
  "log_area" = "Area",
  "All" = "All variables",
  "Unexplained" = "Unexplained"
)

rda_table <- rda_tprep[1,] %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "proportion_of_v_expl") %>% 
  mutate(Category = factor(Category, 
                           levels = names(category_names))) %>%
  mutate(Category = recode(Category, !!!category_names))

# Diagram
rda_prop_var_plot <- ggplot(rda_table, aes(x = Category, y = proportion_of_v_expl)) +
  geom_col(fill = "darkgreen", color = "black") +
  labs(x = "Variable", y = "Proportion of variance explained") +
  annotate("text", x = 1:length(category_names), y = rep(max(rda_table$proportion_of_v_expl) * 0.5, length(category_names)), 
           label = c("***", "**", "**", "**", "**", "**", "*","**", "***", ""), 
           size = 7, color = "black") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(rda_prop_var_plot)

#### Data preparation for biplot -----------
rda_fort_species <- fortify(rda_fs, axes(1,2), scaling = "species") # First and second axes

rda.ve.prep <- rda_fs$CCA$eig / rda_fs$tot.chi*100
(rda1.ve <- round(((rda.ve.prep / sum(rda.ve.prep))[c(1)] * R2a_all_fs) * 100, digits = 1))#11,6% expl. var.
(rda2.ve <- round(((rda.ve.prep / sum(rda.ve.prep))[c(2)] * R2a_all_fs) * 100, digits = 1))#3.7% expl. var.

#Species scores
fort_species_species <- rda_fort_species[rda_fort_species$score == "species",]

# Fixing species names
species_names_corrected <- list(
  "Cocconeisscutelum" = "Cocconeis scutelum",
  "Naviculahanseatica" = "Navicula hanseatica",
  "Naviculameniscus" = "Navicula meniscus",
  "Halamphoracoffeiformis" = "Halamphora coffeiformis",
  "Naviculaperegrina" = "Navicula peregrina",
  "Halamphoraacutiscula" = "Halamphora acutiscula",
  "Amphoraovalis" = "Amphora ovalis",
  "Staurosirellamartyi" = "Staurosirella martyi",
  "Naviculagregaria" = "Navicula gregaria",
  "Planothidiumdelicathulum" = "Planothidium delicathulum",
  "Stephanocyclusmeneghinianus" = "Stephanocyclus meneghinianus",
  "Stephanodiscushantzschii" = "Stephanodiscus hantzschii",
  "Ctenophorapulchella.F." = "Ctenophora pulchella",
  "Navicularadiosa" = "Navicula radiosa",
  "Nitzschiainconspicua" = "Nitzschia inconspicua",
  "Sellaphorabacillum" = "Sellaphora bacillum",
  "Placoneisclementis" = "Placoneis clementis",
  "Cymbopleuranaviculiformis" = "Cymbopleura naviculiformis",
  "Diatomamoniliformis" = "Diatoma moniliformis",
  "Luticolopsisvietnamica" = "Luticolopsis vietnamica",
  "Synedrafamelica" = "Synedra famelica",
  "Amphipleurapellucida" = "Amphipleura pellucida",
  "Pseudostaurosiraelliptica" = "Pseudostaurosira elliptica",
  "Pseudofallaciatenera" = "Pseudofallacia tenera",
  "Diatomatenuis" = "Diatoma tenuis",
  "Pseudostaurosirabrevistriata" = "Pseudostaurosira brevistriata",
  "Brachysirastyriaca" = "Brachysira styriaca",
  "Achnanthidiumlineare" = "Achnanthidium lineare",
  "Nitzschiapalea" = "Nitzschia palea",
  "Encyonopsismicrocephala" = "Encyonopsis microcephala",
  "Naviculacryptotenella" = "Navicula cryptotenella",
  "Staurosiraconstruens" = "Staurosira construens",
  "Pinnulariaborealis" = "Pinnularia borealis",
  "Fragilariagracilis" = "Fragilaria gracilis",
  "Encyonemaminutum" = "Encyonema minutum",
  "Sellaphoralaevissima" = "Sellaphora laevissima",
  "Encyonemasilesiacum" = "Encyonema silesiacum",
  "Halamphorabicapitata" = "Halamphora bicapitata",
  "Pinnulariaviridiformis" = "Pinnularia viridiformis",
  "Lindaviaradiosa" = "Lindavia radiosa",
  "Adlafiaminuscula." = "Adlafia minuscula",
  "Stauroformaexiguiformis" = "Stauroforma exiguiformis",
  "Achnanthidiumminutissimum" = "Achnanthidium minutissimum",
  "Platessaoblongella" = "Platessa oblongella",
  "Tabellariaflocculosa" = "Tabellaria flocculosa",
  "Brachysiravitrea" = "Brachysira vitrea",
  "Achnanthidiumpeterseni" = "Achnanthidium petersenii",
  "Eunotiabilunaris" = "Eunotia bilunaris",
  "Frustuliasaxonica" = "Frustulia saxonica",
  "Tabellariaventricosa" = "Tabellaria ventricosa",
  "Brachysiraserians" = "Brachysira seriens",
  "Eunotiaimplicata" = "Eunotia implicata",
  "Eunotiarhomboidea" = "Eunotia rhomboidea",
  "Brachysiraprocea" = "Brachysira procea",
  "Encyonemaneogracile" = "Encyonema neogracile",
  "Eunotianymanniana" = "Eunotia nymanniana",
  "Frustuliavulgaris" = "Frustulia vulgaris",
  "Fragilariaradians" = "Fragilaria radians",
  "Eunotiatenella" = "Eunotia tenella",
  "Frustuliaerifuga" = "Frustulia erifuga",
  "Encyonopsiscesatii" = "Encyonopsis cesatii",
  "Rossithidiumpusillum" = "Rossithidium pusillum",
  "Eunotiavanheurckii" = "Eunotia vanheurckii",
  "Cocconeisplacentula" = "Cocconeis placentula",
  "Aulacoseiracf.ambigua" = "Aulacoseira cf. ambigua",
  "Staurosirellapinnata" = "Staurosirella pinnata",
  "Fragilariateneravar.Nanana" = "Fragilaria tenera var. nanana",
  "Pinnulariamicrostauron" = "Pinnularia microstauron",
  "Pinnulariakrammeri" = "Pinnularia krammeri",
  "Tabellariafenestrata" = "Tabellaria fenestrata",
  "Gomphonemadrutelingense" = "Gomphonema drutelingense",
  "Stauroneisgracilis" = "Stauroneis gracilis",
  "Naviculacf.cryptocephala" = "Navicula cf. cryptocephala",
  "Hippodontahungarica" = "Hippodonta hungarica",
  "Iconellaamphioxys" = "Iconella amphioxys",
  "Pinnulariabiceps" = "Pinnularia biceps",
  "Nitzschiahantzschiana" = "Nitzschia hantzschiana"
)

# Update the names
fort_species_species <- fort_species_species %>%
  mutate(label = recode(label, !!!species_names_corrected))

# Check if correctly
print(fort_species_species$label)

# Extracting environmental data
fort_env_species <- rda_fort_species[rda_fort_species$score == "biplot",]
env_names <- unique(fort_env_species$label)
# Fixing the names
env_names_corrected <- list(
  "Depth" = "Depth",
  "log_DTS" = "DTS",
  "N" = "TN",
  "SiO2" = "SiO2",
  "pH" = "pH",
  "log_area" = "AR",
  "log_sal" = "SAL",
  "sqrt_sand" = "SGC"
)
fort_env_species <- fort_env_species %>%
  mutate(label = recode(label, !!!env_names_corrected))
print(fort_env_species$label)

# Fortify
fort_species_species <- rda_fort_species[rda_fort_species$score %in% "species",]
fort_env_species <- fort_env_species[fort_env_species$score %in% "biplot",]

# Goodness of fit for diatoms
diatom.tax.good <- goodness(rda_fs)

# Treshold goodness of fit >= 0.2
diatom.tax.selected.sp <- which(diatom.tax.good[, 2] >= 0.19)

# Filtrating the species
diatom.tax.sp.red <- fort_species_species[fort_species_species$label %in% rownames(diatom.tax.good)[diatom.tax.selected.sp], ]
# Names update of filtrated species
diatom.tax.sp.red <- diatom.tax.sp.red %>%
  mutate(label = recode(label, !!!species_names_corrected))

# Extract eigenvalues
eigen_constrained <- spe_rda_all$CCA$eig
total_constrained_inertia <- sum(eigen_constrained)

# Proportion
rda_fort_species <- fortify(rda_fs, axes(1,2), scaling = "species") # First and second axes

rda.ve.prep <- rda_fs$CCA$eig / rda_fs$tot.chi*100
(rda1.ve <- round(((rda.ve.prep / sum(rda.ve.prep))[c(1)] * R2a_all_fs) * 100, digits = 1))#11,6% expl. var.
(rda2.ve <- round(((rda.ve.prep / sum(rda.ve.prep))[c(2)] * R2a_all_fs) * 100, digits = 1))#3.7% expl. var.

rda_axis1_ve <- round(((rda.ve.prep / sum(rda.ve.prep))[c(1)] * R2a_all_fs) * 100, digits = 1) # RDA1
rda_axis2_ve <-  round(((rda.ve.prep / sum(rda.ve.prep))[c(2)] * R2a_all_fs) * 100, digits = 1)  # RDA2

#### RDA biplot. Env.var as vectors + species as vectors ----
rda.plot.filtered <- ggplot() +
  labs(y = paste("RDA2 (", rda2.ve, "%)", sep = ""), 
       x = paste("RDA1 (", rda1.ve, "%)", sep = "")) +
  
  geom_segment(data = diatom.tax.sp.red,
               color = "black", size = 0.4,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = grid::arrow(length = grid::unit(0.20, "cm"))) +
  
  ggrepel::geom_text_repel(data = diatom.tax.sp.red, color = "black",
                           size = 4, segment.alpha = 0,
                           aes(x = RDA1, y = RDA2, label = label, fontface = "italic"),
                           max.overlaps = 30) +
  
  geom_segment(data = fort_env_species,
               color = "darkblue", size = 1,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = grid::arrow(length = grid::unit(0.20, "cm"))) +
  
  ggrepel::geom_text_repel(data = fort_env_species, color = "darkblue",
                           size = 5, segment.alpha = 0,
                           aes(x = RDA1, y = RDA2, label = label)) +
  
  geom_vline(xintercept = 0, color = 'grey65', linewidth = 0.6, linetype = 2) + 
  geom_hline(yintercept = 0, color = 'grey65', linewidth = 0.6, linetype = 2) +
  
  theme_bw() +
  scale_size(range = 2) +
  coord_equal()

print(rda.plot.filtered)

#### RDA plot site scores (DEVIDED INTO ECOREGIONS) + species vectores ----

# Obtaining site scores
site_scores <- scores(spe_rda_all, display = "sites")

# Data frame convert
site_scores_df <- data.frame(site_scores)

# Lake ID matrix
lakeid <- c("SGL02", "SGL05", "SGL06", "SGL07", "SGL08", "SGL100", "SGL102",
            "SGL103", "SGL104", "SGL105", "SGL106", "SGL107", "SGL108", "SGL109",
            "SGL110", "SGL111", "SGL112", "SGL113", "SGL114", "SGL115", "SGL116",
            "SGL118", "SGL119", "SGL120", "SGL121", "SGL122", "SGL123", "SGL124",
            "SGL125", "SGL126", "SGL127", "SGL128", "SGL129", "SGL13", "SGL14",
            "SGL15", "SGL16", "SGL17", "SGL21", "SGL22", "SGL23", "SGL24",
            "SGL25", "SGL26", "TAYLORSPOND", "TL08", "TL09", "TL101", "TL18",
            "TL27")

# Adding Lake ID column
site_scores_df$LakeID <- lakeid

# Lake Categories
category_1 <- c("SGL02", "SGL05", "SGL06", "SGL07", "SGL08", "SGL102",
                "SGL103", "SGL106", "SGL107", "SGL108", "SGL109",
                "SGL110", "SGL111", "SGL112")

category_2 <- c("SGL100", "SGL104", "SGL105", "SGL113", "SGL114", "SGL115", "SGL116", "SGL118", 
                "SGL119", "SGL120", "SGL121", "SGL122", "SGL123", "SGL124","SGL125", "SGL126",
                "SGL127", "SGL128", "SGL129","SGL13", "SGL14","SGL15", "SGL16", "SGL17", "SGL21",
                "SGL22", "SGL23", "SGL24","SGL25", "SGL26", "TAYLORSPOND", "TL08", "TL09", "TL101",
                "TL18", "TL27")

site_scores_df$Category <- ifelse(site_scores_df$LakeID %in% category_1, "Category_1",
                                  ifelse(site_scores_df$LakeID %in% category_2, "Category_2", NA))

# RDA biplot
rda_biplot <- ggplot() +
  
  labs(y = paste0("RDA2 (", rda_axis2_ve, "%)"), 
       x = paste0("RDA1 (", rda_axis1_ve, "%)"),
       title = NULL, 
       color = "Ecoregion") +
  
  geom_segment(data = diatom.tax.sp.red,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               color = "black", size = 0.2,
               arrow = grid::arrow(length = grid::unit(0.25, "cm"))) +
  
  geom_point(data = site_scores_df, 
             aes(x = RDA1, y = RDA2, color = Category), 
             size = 1.5) +
  
  scale_color_manual(values = c("Category_1" = "darkblue", 
                                "Category_2" = "darkred",
                                "Unclassified" = "grey"),
                     labels = c("Category_1" = "Maritime Barren", 
                                "Category_2" = "Easter Hyper-Oceanic Barren",
                                "Unclassified" = "Unclassified")) +
  
  ggrepel::geom_text_repel(data = site_scores_df, 
                           aes(x = RDA1, y = RDA2, label = LakeID, color = Category),
                           max.overlaps = 30,
                           size = 4, segment.alpha = 0) +
  
  ggrepel::geom_text_repel(data = diatom.tax.sp.red, color = "black",
                           aes(x = RDA1, y = RDA2, label = label),
                           max.overlaps = 30,
                           size = 3, fontface = "italic", segment.alpha = 0) +
  
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  
  coord_equal() +
  
  theme(panel.background = element_rect(fill = "grey97", color = "black"),
        panel.grid.major = element_line(color = "gray50", linewidth = 0.2),
        panel.grid.minor = element_line(color = "gray50", linewidth = 0.2),
        legend.position = "bottom")

print(rda_biplot)

#### RDA plot site scores (DEVIDED INTO LAKE CATEGORY) + species vectores ----

# Obtaining site scores
site_scores <- scores(spe_rda_all, display = "sites")

# Data frame convert
site_scores_df <- data.frame(site_scores)

# Lake ID matrix
lakeid <- c("SGL02", "SGL05", "SGL06", "SGL07", "SGL08", "SGL100", "SGL102",
            "SGL103", "SGL104", "SGL105", "SGL106", "SGL107", "SGL108", "SGL109",
            "SGL110", "SGL111", "SGL112", "SGL113", "SGL114", "SGL115", "SGL116",
            "SGL118", "SGL119", "SGL120", "SGL121", "SGL122", "SGL123", "SGL124",
            "SGL125", "SGL126", "SGL127", "SGL128", "SGL129", "SGL13", "SGL14",
            "SGL15", "SGL16", "SGL17", "SGL21", "SGL22", "SGL23", "SGL24",
            "SGL25", "SGL26", "TAYLORSPOND", "TL08", "TL09", "TL101", "TL18",
            "TL27")

# Adding Lake ID column
site_scores_df$LakeID <- lakeid

# Lake Categories
category_1 <- c("SGL24", "TL09", "SGL23", "SGL13", "SGL14", "SGL15", "SGL16", "SGL17", "SGL21", "SGL22", "SGL102", "SGL103", "SGL107", "SGL05", "SGL109", "SGL110", "SGL111", "SGL112", "SGL115", "TL27", "SGL116", "SGL118", "SGL119", "SGL120", "SGL121", "SGL122", "SGL125", "SGL126", "SGL127", "SGL128", "SGL129", "SGL100","TL101", "TAYLORSPOND", "SGL08", "SGL106", "SGL113", "SGL123", "SGL124", "SGL26", "TL08")
category_2 <- c("TL18", "SGL108", "SGL02",  "SGL114", "SGL25")
category_3 <- c("SGL104", "SGL105", "SGL07", "SGL06")

site_scores_df$Category <- ifelse(site_scores_df$LakeID %in% category_1, "Category_1",
                                  ifelse(site_scores_df$LakeID %in% category_2, "Category_2", "Category_3"))

# RDA biplot
rda_biplot <- ggplot() +
  
  labs(y = paste0("RDA2 (", rda_axis2_ve, "%)"), 
       x = paste0("RDA1 (", rda_axis1_ve, "%)"),
       title = NULL, 
       color = "Lake category") +
  
  geom_segment(data = diatom.tax.sp.red,
               aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               color = "black", size = 0.7,
               arrow = grid::arrow(length = grid::unit(0.25, "cm"))) +
  
  geom_point(data = site_scores_df, 
             aes(x = RDA1, y = RDA2, color = Category), 
             size = 1.5) +
  
  scale_color_manual(values = c("Category_1" = "darkgreen", 
                                "Category_2" = "darkred", 
                                "Category_3" = "darkblue", 
                                "Unclassified" = "grey"),
                     labels = c("Category_1" = "Freshwater", 
                                "Category_2" = "Brackish",
                                "Category_3" = "Marine",
                                "Unclassified" = "Unclassified")) +
  
  ggrepel::geom_text_repel(data = site_scores_df, 
                           aes(x = RDA1, y = RDA2, label = LakeID, color = Category),
                           max.overlaps = 30,
                           size = 2.5, segment.alpha = 0) +
  
  ggrepel::geom_text_repel(data = diatom.tax.sp.red, color = "black",
                           aes(x = RDA1, y = RDA2, label = label),
                           max.overlaps = 30,
                           size = 2.5, segment.alpha = 0) +
  
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6, linetype = 2) +
  
  coord_equal() +
  
  theme(panel.background = element_rect(fill = "grey97", color = "black"),
        panel.grid.major = element_line(color = "gray50", linewidth = 0.2),
        panel.grid.minor = element_line(color = "gray50", linewidth = 0.2),
        legend.position = "bottom")

print(rda_biplot)

#### RDA on pH only (3,39% variance explained) ----
rda_fs <- rda(species_sqrt ~ env_variables_log$pH)

summary_rda <- summary(rda_fs)
print(summary_rda)

# Adjusted R-squared
R2a <- RsquareAdj(rda_fs)$adj.r.squared
cat("Adjusted R-squared:", R2a, "\n") #0.01375563

# Environmental data for plotting
fort_env_species <- rda_fort_species[rda_fort_species$score == "biplot", ]

# First plot (just to see in case)
plot(rda_fs)
points(rda_fs, display = "sites", pch = 21, col = "blue", bg = "lightblue", cex = 1.5)
text(diatom.tax.sp.red$RDA1, diatom.tax.sp.red$RDA2, labels = diatom.tax.sp.red$label, pos = 4, cex = 0.8)

# Extract eigenvalues and variance explained
eigenvalues <- rda_fs$CA$eig
total_variance <- sum(eigenvalues)
explained_variance <- eigenvalues / total_variance * 100  # Percent variance explained

# Proportion of variance explained
print(data.frame(
  Axis = 1:length(eigenvalues),
  Eigenvalue = eigenvalues,
  Proportion = explained_variance
))
R2a <- RsquareAdj(rda_fs)$adj.r.squared
cat("Adjusted R-squared:", R2a, "\n")
anova_rda <- anova(rda_fs, perm = 999)  # 999 permutations for significance testing
print(anova_rda)
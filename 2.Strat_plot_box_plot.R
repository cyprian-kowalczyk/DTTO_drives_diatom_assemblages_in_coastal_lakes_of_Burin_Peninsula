library(vegan)
library(readr)
library(ggplot2)
library(tidyverse)
library(rioja)
library(dplyr)
source("stratplot2.R")

##### Data preparation for plot ----
# Reduced data for plot
species_reduced <- read_csv("data/species_reduced_LakeID.csv")

# Unique species names
unique_species_reduced <- unique(colnames(species_reduced)[-1])

# Proper names list
species_names_corrected <- list(
  "Cocconeisscutelum" = "Cocconeis scutellum",
  "Naviculahanseatica" = "Navicula hanseatica",
  "Naviculameniscus" = "Navicula meniscus",
  "Halamphoracoffeiformis" = "Halamphora coffeiformis",
  "Naviculaperegrina" = "Navicula peregrina",
  "Halamphoraacutiscula" = "Halamphora acutiscula",
  "Amphoraovalis" = "Amphora ovalis",
  "Staurosirellamartyi" = "Staurosirella martyi",
  "Naviculagregaria" = "Navicula gregaria",
  "Planothidiumdelicathulum" = "Planothidium delicatulum",
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
  "Adlafiaminuscula.F." = "Adlafia minuscula",
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

# Species names change in species_reduced
species_reduced_corrected <- species_reduced %>%
  rename_with(~ recode(., !!!species_names_corrected), -1)  # Zakładam, że pierwsza kolumna to ID lub Site

# Defining the groups
group1 <- c("SGL24", "TL09", "SGL23", "SGL13", "SGL14", "SGL15", "SGL16", "SGL17", "SGL21", "SGL22", 
            "SGL102", "SGL103", "SGL107", "SGL05", "SGL109", "SGL110", "SGL111", "SGL112", "SGL115", 
            "TL27", "SGL116", "SGL118", "SGL119", "SGL120", "SGL121", "SGL122", "SGL125", "SGL126", 
            "SGL127", "SGL128", "SGL129", "SGL100", "TL08","TL101", "TAYLORSBAY", "SGL08", "SGL106", "SGL113","SGL123", "SGL124","SGL26")

group2 <- c("TL18", "SGL108", "SGL02", "SGL114", "SGL25")

group3 <- c("SGL104", "SGL105", "SGL07", "SGL06")

# Creating categorization data frame
lake_categories <- tibble(
  LakeID = c(group1, group2, group3),
  Category = c(rep("Group1", length(group1)),
               rep("Group2", length(group2)),
               rep("Group3", length(group3)))
)
# Merging the main data frame with categories
species_with_categories <- species_reduced_corrected %>%
  left_join(lake_categories, by = "LakeID")

# Finding lakes without a category
missing_categories <- species_reduced_corrected %>%
  anti_join(lake_categories, by = "LakeID")

species_xnames <- species_reduced_corrected[ ,-1] # deletes the first column (LakeID)

lake_id <- species_reduced_corrected$LakeID
y.override <- lake_id
yvar <- 1:50
species_reduced2_prep <- colSums(species_reduced_corrected[,-1])
species_reduced2_prep2 <- species_reduced_corrected[,-1]

species_reduced2 <- 
  as.data.frame(species_reduced2_prep2[,which(species_reduced2_prep > 35)])
species_reduced2 <- as.data.frame(
  species_reduced2_prep2[, which(species_reduced2_prep > 35 | 
                                   names(species_reduced2_prep) == "Navicula hanseatica")]
)

#Full data for richness and Inverse Simpson Index
(species <- read_csv("data/species_counts.csv"))

unique_species <- unique(colnames(species)[-1])
species_all_names <- list(
  "Trachyneisaspera" = "Trachyneis aspera",
  "Nitzschiatubicola" = "Nitzschia tubicola",
  "Pinnulariasemiinflata" = "Pinnularia semiinflata",
  "Surirellasubsalsa" = "Surirella subsalsa",
  "Tabulariawaernii" = "Tabularia waernii",
  "Naviculabipustulata" = "Navicula bipustulata",
  "Alveusmarinus" = "Alveus marinus",
  "Nitzschiasubconstricta" = "Nitzschia subconstricta",
  "Cocconeisscutelum" = "Cocconeis scutellum",
  "Actinocycluscf.octonarius" = "Actinocyclus cf. octonarius",
  "Actinocycluscf.normaii" = "Actinocyclus cf. normaii",
  "Nitzschiafusiformis" = "Nitzschia fusiformis",
  "Tabulariafasciculata" = "Tabularia fasciculata",
  "Naviculaveneta.B." = "Navicula veneta B.",
  "Nitzschiasigma" = "Nitzschia sigma",
  "Parlibelluaplicatus" = "Parlibellus plicatus",
  "Rhopalodiamusculus" = "Rhopalodia musculus",
  "Naviculahanseatica" = "Navicula hanseatica",
  "Naviculameniscus" = "Navicula meniscus",
  "Naviculaarenaria" = "Navicula arenaria",
  "Halamphoracoffeiformis" = "Halamphora coffeiformis",
  "Naviculaperegrina" = "Navicula peregrina",
  "Halamphoraacutiscula" = "Halamphora acutiscula",
  "Naviculaphylleoptosoma" = "Navicula phylleoptosoma",
  "Mastogloiasmithii." = "Mastogloia smithii",
  "Diploneissmithii" = "Diploneis smithii",
  "Pinnunaviselegans" = "Pinnunavis elegans",
  "Nitzschiadubia" = "Nitzschia dubia",
  "Surirellaovalis" = "Surirella ovalis",
  "Diploneisdidymus" = "Diploneis didymus",
  "Parlibelluscrucicula" = "Parlibellus crucicula",
  "Cocconeiscalifornica" = "Cocconeis californica",
  "Amphoraovalis" = "Amphora ovalis",
  "Navicularhynchotella" = "Navicula rhynchotella",
  "Staurosirellamartyi" = "Staurosirella martyi",
  "Naviculagregaria" = "Navicula gregaria",
  "Planothidiumdelicathulum" = "Planothidium delicatulum",
  "Stephanocyclusmeneghinianus" = "Stephanocyclus meneghinianus",
  "Stephanodiscushantzschii" = "Stephanodiscus hantzschii",
  "Diploneiselliptica" = "Diploneis elliptica",
  "Achnanthesadnata" = "Achnanthes adnata",
  "Prestauroneiscrucicula" = "Stauroneis crucicula",
  "Ctenophorapulchella.F." = "Ctenophora pulchella",
  "Navicularadiosa" = "Navicula radiosa",
  "Nitzschiainconspicua" = "Nitzschia inconspicua",
  "Sellaphorabacillum" = "Sellaphora bacillum",
  "Placoneisclementis" = "Placoneis clementis",
  "Cymbopleuranaviculiformis" = "Cymbopleura naviculiformis",
  "Diatomamoniliformis" = "Diatoma moniliformis",
  "Luticolopsisvietnamica" = "Luticolopsis vietnamica",
  "Synedrafamelica" = "Synedra famelica",
  "Fragilariacapucina" = "Fragilaria capucina",
  "Nitzschiasigmoidea" = "Nitzschia sigmoidea",
  "Amphipleurapellucida" = "Amphipleura pellucida",
  "Nitzschianana" = "Nitzschia nana",
  "Pseudostaurosiraelliptica" = "Pseudostaurosira elliptica",
  "Pseudofallaciatenera" = "Pseudofallacia tenera",
  "Craticulahalophilioides" = "Craticula halophilioides",
  "Brebissonialanceolata" = "Brebissonium lanceolatum",
  "Fallaciapygmaea" = "Fallacia pygmaea",
  "Stauroneisseparanda" = "Stauroneis separanda",
  "Diatomatenuis" = "Diatoma tenuis",
  "Nitzschiadissipata" = "Nitzschia dissipata",
  "Pseudostaurosirabrevistriata" = "Pseudostaurosira brevistriata",
  "Brachysirastyriaca" = "Brachysira styriaca",
  "Achnanthidiumlineare" = "Achnanthidium lineare",
  "Nitzschiapalea" = "Nitzschia palea",
  "Encyonopsismicrocephala" = "Encyonopsis microcephala",
  "Naviculacryptotenella" = "Navicula cryptotenella",
  "Staurosiraconstruens" = "Staurosira construens",
  "Pinnulariaborealis" = "Pinnularia borealis",
  "Fragilariagracilis" = "Fragilaria gracilis",
  "Pinnulariaviridis" = "Pinnularia viridis",
  "Fallaciatenera" = "Fallacia tenera",
  "Aulacoseiracanadensis" = "Aulacoseira canadensis",
  "Cymbopleurainaequalis" = "Cymbopleura inaequalis",
  "Iconellacurvula" = "Iconella curvula",
  "Caloneissilicula" = "Caloneis silicula",
  "Caloneistenuis" = "Caloneis tenuis",
  "Encyonemaminutum" = "Encyonema minutum",
  "Gomphonemaacuminatum" = "Gomphonema acuminatum",
  "Cymbellacymbiformis" = "Cymbella cymbiformis",
  "Sellaphoralaevissima" = "Sellaphora laevissima",
  "Encyonemasilesiacum" = "Encyonema silesiacum",
  "Halamphorabicapitata" = "Halamphora bicapitata",
  "Pinnulariaviridiformis" = "Pinnularia viridiformis",
  "Lindaviaradiosa" = "Lindavia radiosa",
  "Adlafiaminuscula." = "Adlafia minuscula",
  "Gomphonemaminusculum" = "Gomphonema minusculum",
  "Encyonemaobscurum" = "Encyonema obscurum",
  "Lindaviabalatonis" = "Lindavia balatonis",
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
  "Gomphonemaparvulius" = "Gomphonema parvulius",
  "Staurosirellapinnata" = "Staurosirella pinnata",
  "Psammothidiumsubatomoides" = "Psammothidium subatomoides",
  "Fragilariateneravar.Nanana" = "Fragilaria tenera var. nanana",
  "Pinnulariamicrostauron" = "Pinnularia microstauron",
  "Gyrosigmaacuminatum" = "Gyrosigma acuminatum",
  "Pinnulariakrammeri" = "Pinnularia krammeri",
  "Frustuliabahlsii" = "Frustulia bahlsii",
  "Tabellariafenestrata" = "Tabellaria fenestrata",
  "Gomphonemadrutelingense" = "Gomphonema drutelingense",
  "Stauroneiskriegeri" = "Stauroneis kriegeri",
  "Stauroneisgracilis" = "Stauroneis gracilis",
  "Naviculacf.cryptocephala" = "Navicula cf. cryptocephala",
  "Eunotiaarcus" = "Eunotia arcus",
  "Hippodontahungarica" = "Hippodonta hungarica",
  "Iconellaamphioxys" = "Iconella amphioxys",
  "Fragilariatenera" = "Fragilaria tenera",
  "Pinnulariabiceps" = "Pinnularia biceps",
  "Nitzschiahantzschiana" = "Nitzschia hantzschiana",
  "Neidiumaffine" = "Neidium affine",
  "Fragilariformabicapitata" = "Fragilaria formabicapitata",
  "Placoneiselginensis" = "Placoneis elginensis",
  "Stauroneisphoenicentron" = "Stauroneis phoenicentron",
  "Eunotiapaludosa" = "Eunotia paludosa",
  "Staurosiraventer" = "Staurosira venter",
  "Gomphonemaparvulum" = "Gomphonema parvulum",
  "Naviculatripunctata" = "Navicula tripunctata",
  "Diploneisfinnica" = "Diploneis finnica",
  "Eucocconeisflexella" = "Eucocconeis flexella",
  "Eunotiameisteri" = "Eunotia meisteri",
  "Eunotiasubherkiniensis" = "Eunotia subherkiniensis",
  "Stauroneissylvahassiaca" = "Stauroneis sylvahassiaca",
  "Diploneisboldtiana" = "Diploneis boldtiana",
  "Eunotiaglacialis" = "Eunotia glacialis",
  "Stauroneisacidoclinata" = "Stauroneis acidoclinata",
  "Nitzschiahomburgensis" = "Nitzschia homburgensis",
  "Pinnulariacomplexa" = "Pinnularia complexa",
  "Fragilariaexigua" = "Fragilaria exigua",
  "Meridioncirculare" = "Meridion circulare",
  "Ulnariadelicatissimavar.angustissima" = "Ulnaria delicatissima var. angustissima",
  "Actinellapunctata" = "Actinella punctata",
  "Fragilariarhabdosoma" = "Fragilaria rhabdosoma",
  "Pinnulariacf.stomatophora" = "Pinnularia cf. stomatophora",
  "Neidiumbisulcatum" = "Neidium bisulcatum",
  "Pinnulariamacilenta" = "Pinnularia macilenta",
  "Pinnulariapulchra" = "Pinnularia pulchra",
  "Eunotiatriodon" = "Eunotia triodon",
  "Fragilariformavirescensvar.subsalina" = "Fragilaria formavirescens var. subsalina",
  "Neidiumampilatum" = "Neidiumampilatum",
  "Neidiumlongiceps" = "Neidium longiceps",
  "Pinnulariaturfosiphila" = "Pinnularia turfosiphila",
  "Caloneisschumanniana" = "Caloneis schumanniana",
  "Eunotiaboreoalpina" = "Eunotia boreoalpina",
  "Eunotiatetraedron" = "Eunotia tetraedron",
  "Cavinulapseudoscutiformis" = "Cavinula pseudoscutiformis",
  "Diploneiskrammeri" = "Diploneis krammeri",
  "Gomphonemacoronatum" = "Gomphonema coronatum",
  "Pinnulariasubgibba" = "Pinnularia subgibba",
  "Pinnulariasubnodosa" = "Pinnularia subnodosa",
  "Ulnariaulna" = "Ulnaria ulna",
  "Gomphonemapala" = "Gomphonema pala",
  "Gomphonemapumilum" = "Gomphonema pumilum",
  "Planothidiumpotapovae" = "Planothidium potapovae",
  "Staurosirabidonis" = "Staurosira bidonis",
  "Diatomasp." = "Diatoma sp.",
  "Aulacoseirasp." = "Aulacoseira sp.",
  "Pinnulariasp." = "Pinnularia sp.",
  "Sellaphorasp." = "Sellaphora sp.",
  "Achnanthessp." = "Achnanthes sp.",
  "Stauroneiscapitata" = "Stauroneis capitata",
  "Nitzschiasp." = "Nitzschia sp.",
  "Tabellariasp." = "Tabellaria sp."
)
species_all_names <- species %>%
  rename_with(~ recode(., !!!species_all_names), -1)
print(species_all_names)

LakeID <- species_all_names$LakeID
species_all_names$LakeID <- NULL

counting_sums <- rowSums(species_all_names) # Counting sums
species_proc <- t(apply(species_all_names, 1, function(x) (x / counting_sums) * 100)) # Changing counts into percentage

#Species richness
richness <- specnumber(species_proc)
richness_combined <- data.frame(LakeID = lake_id, SpeciesRichness = richness)

#Inverse Simpson Index
species_matrix <- as.matrix(species_proc)
invsimpson_index <- diversity(species_matrix, index = "invsimpson")
mean(richness)
median(richness)
median(invsimpson_index)

#Diatom diagram
richness_combined <- richness_combined %>%
  left_join(species_with_categories %>% select(LakeID, Category), by = "LakeID")

# Prepare inverse Simpson index data
invsimpson_combined <- data.frame(
  LakeID = lake_id,
  InverseSimpsonIndex = invsimpson_index
) %>%
  left_join(species_with_categories %>% select(LakeID, Category), by = "LakeID")
richness <- specnumber(species_reduced_corrected[,-1]) # Species richness per site
simpson_index <- diversity(species_reduced_corrected[,-1], index = "simpson")
inv_simpson_index <- 1 / simpson_index

# Combine richness and inverse Simpson index
combined_data <- richness_combined %>%
  left_join(invsimpson_combined, by = "LakeID", suffix = c("_richness", "_invsimpson"))
combined_data <- combined_data %>%
  mutate(LakeID = ifelse(LakeID == "TAYLORSBAY", "TAYLORSPOND", LakeID))
# Define category colors
category_colors <- c("Group1" = "darkgreen", "Group2" = "darkred", "Group3" = "darkblue")

# Merge richness and inverse Simpson index with categories
richness_combined <- richness_combined %>%
  left_join(species_with_categories %>% select(LakeID, Category), by = "LakeID")

# Prepare inverse Simpson index data
invsimpson_combined <- data.frame(
  LakeID = lake_id,
  InverseSimpsonIndex = invsimpson_index
) %>%
  left_join(species_with_categories %>% select(LakeID, Category), by = "LakeID")

# Combine richness and inverse Simpson index
combined_data <- richness_combined %>%
  left_join(invsimpson_combined, by = "LakeID", suffix = c("_richness", "_invsimpson"))

combined_data <- combined_data %>%
  rename(Category = Category_richness)  # Renaming to ensure consistency

##### Scatter plot of Species Richness vs. Inverse Simpson Index ----

ggplot(combined_data, aes(x = SpeciesRichness, y = InverseSimpsonIndex, color = Category)) +
  geom_point(size = 3) +
  geom_text(aes(label = LakeID), vjust = -1, hjust = 0.5, size = 3, check_overlap = TRUE) +
  labs(
    title = "Species Richness vs. Inverse Simpson Index",
    x = "Species Richness",
    y = "Inverse Simpson Index",
    color = "Group"
  ) +
  scale_color_manual(
    values = c("Group1" = "darkgreen", "Group2" = "darkred", "Group3" = "darkblue"),
    labels = c("Group1" = "Absent marine influence", "Group2" = "Intermittent marine influence", "Group3" = "Semi-constant marine influence")
  ) +
  theme_minimal()

#### Data preparation for strat plot ----

# Define colors for bars and additional metrics
num_species_bars <- ncol(species_reduced2)
colors <- c(rep("black", num_species_bars), "darkred", "darkblue")

# Update LakeID names everywhere
rownames(species_reduced2)[rownames(species_reduced2) == "TAYLORSBAY"] <- "TAYLORSPOND"
names(richness)[names(richness) == "TAYLORSBAY"] <- "TAYLORSPOND"
names(invsimpson_index)[names(invsimpson_index) == "TAYLORSBAY"] <- "TAYLORSPOND"
species_reduced_corrected$LakeID[species_reduced_corrected$LakeID == "TAYLORSBAY"] <- "TAYLORSPOND"

rownames(species_reduced2) <- species_reduced_corrected$LakeID
names(richness) <- species_reduced_corrected$LakeID
names(invsimpson_index) <- species_reduced_corrected$LakeID

ordered_ids <- c(
  "SGL126", "SGL121", "SGL16", "SGL17", "SGL14", "SGL15", "SGL110", "SGL120", "SGL102",
  "SGL13", "SGL129", "SGL111", "SGL22", "SGL112", "SGL23", "SGL118", "SGL119", "SGL05",
  "SGL21", "SGL125", "SGL122", "SGL116", "SGL103", "SGL105", "SGL127", "SGL24", "TL08",
  "TL09", "SGL128", "SGL109", "SGL124", "SGL104", "TAYLORSPOND", "SGL08", "SGL123",
  "TL27", "SGL25", "TL18", "SGL100", "SGL114", "TL101", "SGL107", "SGL115", "SGL26",
  "SGL106", "SGL108", "SGL113", "SGL02", "SGL06", "SGL07"
)

# Change the order of the objects
species_reduced2 <- species_reduced2[ordered_ids, ]
richness <- richness[ordered_ids]
invsimpson_index <- invsimpson_index[ordered_ids]
scaled_invsimpson <- (invsimpson_index / max(invsimpson_index)) * 100
scaled_richness <- (richness / max(richness)) * 100

plot_matrix <- cbind(species_reduced2, 
                     "InverseSimpson" = scaled_invsimpson,
                     "Richness" = scaled_richness)
species_reduced_corrected$LakeID

# Sorting species_reduced_corrected and species_reduced2 according to ordered_ids
species_reduced_corrected <- species_reduced_corrected %>%
  slice(match(ordered_ids, LakeID))

species_reduced2 <- species_reduced2[match(ordered_ids, rownames(species_reduced2)), ]

# Update yvar, y.override
yvar <- 1:length(ordered_ids)
y.override <- ordered_ids

# Data sorting
species_reduced_corrected <- species_reduced_corrected %>%
  slice(match(ordered_ids, LakeID))
species_reduced2 <- species_reduced2[match(ordered_ids, rownames(species_reduced2)), ]
richness <- richness[match(ordered_ids, species_reduced_corrected$LakeID)]
invsimpson_index <- invsimpson_index[match(ordered_ids, species_reduced_corrected$LakeID)]

# Axe Y
yvar <- 1:length(ordered_ids)
y.override <- ordered_ids

##### Strat plot ----

strat.plot2(
  cbind(species_reduced2, invsimpson_index, richness),
  yvar = yvar,
  y.tks = yvar,
  y.labels = y.override,
  cex.yaxis = 0.5,
  par(family = "Times New Roman"),
  plot.line = FALSE,
  plot.poly = FALSE,
  plot.bar = TRUE,
  lwd.bar = 8,
  scale.percent = TRUE,
  x.pc.lab = TRUE,
  x.pc.omit0 = TRUE,
  fontface = "italic",
  col.bar = colors,
  col.xlabel = "black",
  y.label = "Lake ID",
  srt.xlabel = 45
)
# Add horizontal lines for better visualization
for (i in 1:length(yvar)) {
  abline(h = i - 0.4, col = "gray50", lty = 2)
}

##### Box Plot ----

NF_lakes_conditions_long <- read_csv("data/NF_lakes_conditions_long.csv") %>%
  filter(!Param %in% c("Latitude", "Longitude", "Cond", "Temp"))

NF_lakes_conditions_long <- NF_lakes_conditions_long

print(rownames(NF_lakes_conditions_long))

# Param names change
param_names_corrected <- list(
  "Depth" = "Lake depth [m]",
  "Area" = "Area [ha]",
  "DistanceToTheSea" = "Distance to the ocean [m]",
  "N" = "Total nitrogen [mg/L]",
  "P" = "Total phosphorus [mg/L]",
  "SiO2" = "SiO2 [mg/L]",
  "DOC" = "Dissolved organic carbon [mg/L]",
  "ODO" = "Optical dissolved oxygen [%]",
  "pH" = "pH [units]",
  "Sal" = "Salinity [PSU]",
  "LOI" = "Loss on ignition [%]",
  "SandGrains" = "Sand Grains [counts]"
)

NF_lakes_conditions_long <- NF_lakes_conditions_long %>%
  mutate(Param = recode(Param, !!!param_names_corrected))

# Box plot
ggplot(NF_lakes_conditions_long, aes(y = Value)) +
  geom_boxplot(fill = "green4", color = "black") +
  facet_wrap(~Param, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(), # Usuwa wartości osi x
    axis.ticks.x = element_blank(), # Usuwa znaczniki osi x
    strip.background = element_rect(fill = "yellow4", color = "black"),
    strip.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.grid.minor = element_line(color = "grey")
  )

#### Obtaining the values ----
env_variables <- read_csv("data/NF_lakes_conditions_wide.csv") %>% 
  select(!c(Latitude, Longitude, Cond, Temp)) %>% 
  arrange(LakeID)

env_lakeid <- env_variables$LakeID
env_variables$LakeID <- NULL 

tp <- env_variables$P #TP
min(tp)
max(tp)
mean(tp)
median(tp)

tn <- env_variables$N #TN
min(tn)
max(tn)
mean(tn)
median(tn)

sio2 <- env_variables$SiO2 #SiO2
min(sio2)
max(sio2)
mean(sio2)
median(sio2)

doc <- env_variables$DOC #DOc
min(doc)
max(doc)
mean(doc)
median(doc)

depth <- env_variables$Depth #Depth
min(depth)
max(depth)
mean(depth)
median(depth)

area <- env_variables$Area #Area
min(area)
max(area)
mean(area)
median(area)

dtts <- env_variables$DistanceToTheSea #Distance to the sea
min(dtts)
max(dtts)
mean(dtts)
median(dtts)

odo <- env_variables$ODO #ODO
min(odo)
max(odo)
mean(odo)
median(odo)

ph <- env_variables$pH #pH
min(ph)
max(ph)
mean(ph)
median(ph)

sal <- env_variables$Sal #Salinity
min(sal)
max(sal)
mean(sal)
median(sal)

loi <- env_variables$LOI #LOI
min(loi)
max(loi)
mean(loi)
median(loi)

sand <- env_variables$SandGrains #sand
min(sand)
max(sand)
mean(sand)
median(sand)

min(richness)
max(richness)
mean(richness)
median(richness)

View(env_variables)
env_lakeid

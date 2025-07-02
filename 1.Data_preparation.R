library(tidyverse)
library(readxl)
library(vegan)
library(ade4)
library(ggvegan)

#### Load species count data ----
counts <- read_csv("data/species_counts.csv")

# Correct taxon names
species_names_corrected <- c(
  "Trachyneisaspera" = "Trachyneis aspera",
  "Nitzschiatubicola" = "Nitzschia tubicola",
  "Pinnulariasemiinflata" = "Pinnularia semiinflata",
  "Surirellasubsalsa" = "Surirella subsalsa",
  "Tabulariawaernii" = "Tabularia waernii",
  "Naviculabipustulata" = "Navicula bipustulata",
  "Alveusmarinus" = "Alveus marinus",
  "Nitzschiasubconstricta" = "Nitzschia subconstricta",
  "Cocconeisscutelum" = "Cocconeis scutelum",
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
  "Planothidiumdelicathulum" = "Planothidium delicathulum",
  "Stephanocyclusmeneghinianus" = "Stephanocyclus meneghinianus",
  "Stephanodiscushantzschii" = "Stephanodiscus hantzschii",
  "Diploneiselliptica" = "Diploneis elliptica",
  "Achnanthesadnata" = "Achnanthes adnata",
  "Prestauroneiscrucicula" = "Pre Stauroneis crucicula",
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

counts <- counts %>%
  rename_with(~ recode(., !!!species_names_corrected), -1)

#### Calculate diversity metrics ----
counts <- counts %>%
  mutate(
    Species_richness = rowSums(select(., -1) > 0),
    Inverse_Simpson = 1 / rowSums((select(., -1) / rowSums(select(., -1)))^2)
  )

#### Convert to relative abundances (percentages) ----
species_only <- select(counts, -1, -Species_richness, -Inverse_Simpson)
counts_sum <- rowSums(species_only)

species_percent <- sweep(species_only, 1, counts_sum, "/") * 100

#### Filter species: ≥2% in ≥2 sites ----
condition <- colSums(species_percent >= 2) >= 2
species_filtered <- species_percent[, condition]

# Combine with diversity metrics
species_final <- cbind(species_filtered, counts[, c("Species_richness", "Inverse_Simpson")])

# Add LakeID back
species_final <- bind_cols(
  Site = counts[[1]],
  as_tibble(species_final)
)

# Save
write_csv(species_final, "data/species_reduced2.csv")

#### Prepare environmental data ----
env_long <- read_excel("data/NF_Lakes_conditions.xlsx") %>%
  filter(
    !Param %in% c("SurficialGeology", "Comment", "Units", "DominatingVegetationType", "CoastlineType", "SecchiDepth", "ORP"),
    !(Param == "Temp" & SamplingPosition == "BOT")
  ) %>%
  mutate(Value = as.numeric(Value)) %>%
  select(LakeID, Param, Value)

# Reshape to wide format
env_wide <- env_long %>%
  pivot_wider(names_from = Param, values_from = Value)

# Save environmental data
write_csv(env_long,  "data/NF_lakes_conditions_long.csv")
write_csv(env_wide,  "data/NF_lakes_conditions_wide.csv")

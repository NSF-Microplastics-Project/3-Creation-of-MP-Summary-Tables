
library(tidyverse)
library(sf) # Package used for dealing with shapefiles and simple features
library(ggplot2)
library(gh)



############### Load files ###############

Particles_sf <- st_read("Input/SFEI.ID.particles.byRiskRegion Shapefiles/SFEI.ID.particles.byRiskRegion.shp") # read in Risk Region shapefile from Input folder

Particles <- as.data.frame(Particles_sf)
#################

### Cut Data

# Both Plastics and TWP 
IncludeList_plastic_TWP <- c("Acrylic","Anthropogenic (synthetic)","Cellulose acetate","Ethylene/vinyl acetate copolymer","Nylon","Polyester","Polyethylene",
                             "Polyethylene co-acrylic acid", "Polyethylene/polypropylene copolymer", "Polypropylene", "Polystyrene", "Polyethylene terephthalate",
                             "Polyurethane", "Polyvinyl chloride", "Rubber", "Styrene copolymer", "Unknown Potentially Rubber", "Polyvinyl butyral",
                             "Methyl vinyl ether copolymer","Polyvinyl alcohol","Polystyrene/acrylic copolymer", "Acrylonitrile butadiene styrene", "Polycarbonate", 
                             "Polyvinyl acetate", "Rubber", "Unknown Potentially Rubber")
particles_plastic_TWP <- subset(Particles, Particles$PlstcTy %in% IncludeList_plastic_TWP)

# Only Plastics
IncludeList_plastic <- c("Acrylic","Anthropogenic (synthetic)","Cellulose acetate","Ethylene/vinyl acetate copolymer","Nylon","Polyester","Polyethylene",
                         "Polyethylene co-acrylic acid", "Polyethylene/polypropylene copolymer", "Polypropylene", "Polystyrene", "Polyethylene terephthalate",
                         "Polyurethane", "Polyvinyl chloride", "Styrene copolymer", "Polyvinyl butyral","Methyl vinyl ether copolymer","Polyvinyl alcohol",
                         "Polystyrene/acrylic copolymer", "Acrylonitrile butadiene styrene", "Polycarbonate", "Polyvinyl acetate")
particles_plastic <- subset(Particles, Particles$PlstcTy %in% IncludeList_plastic)

# Potential tire particles only (e.i. rubber and Unknown Potentially Rubber)
IncludeList_TWP <- c("Rubber", "Unknown Potentially Rubber")
particles_TWP <- subset(Particles, Particles$PlstcTy %in% IncludeList_TWP)

# Separating Rubber and Potentially Rubber 
IncludeList_Rubber <- c("Rubber")
IncludeList_Potent_Rubber <- c("Unknown Potentially Rubber")
Rubber<- subset(Particles, Particles$PlstcTy %in% IncludeList_Rubber)
Potent_Rubber <- subset(Particles, Particles$PlstcTy %in% IncludeList_Potent_Rubber)

###############

# Subsetting so only data for effluent samples is available
eff_Rubber <- subset(Rubber, Rubber$SmplMtr %in% "eff")
eff_Potent_Rubber <- subset(Potent_Rubber, Potent_Rubber$SmplMtr %in% "eff")
eff_TWP_plastic<- subset(particles_plastic_TWP, particles_plastic_TWP$SmplMtr %in% "eff")
eff_TWP<- subset(particles_TWP, particles_TWP$SmplMtr %in% "eff")
eff_plastic<- subset(particles_plastic, particles_plastic$SmplMtr %in% "eff")

# Subsetting so only data for sediment samples is avaialbe
sed_Rubber <- subset(Rubber, Rubber$SmplMtr %in% "sed")
sed_Potent_Rubber <- subset(Potent_Rubber, Potent_Rubber$SmplMtr %in% "sed")
sed_TWP_plastic<- subset(particles_plastic_TWP, particles_plastic_TWP$SmplMtr %in% "sed")
sed_TWP<- subset(particles_TWP, particles_TWP$SmplMtr %in% "sed")
sed_plastic<- subset(particles_plastic, particles_plastic$SmplMtr %in% "sed")

# Subsetting so only data for manta trawls samples is available
manta_Rubber <- subset(Rubber, Rubber$SmplMtr %in% "manta")
manta_Potent_Rubber <- subset(Potent_Rubber, Potent_Rubber$SmplMtr %in% "manta")
manta_TWP_plastic<- subset(particles_plastic_TWP, particles_plastic_TWP$SmplMtr %in% "manta")
manta_TWP<- subset(particles_TWP, particles_TWP$SmplMtr %in% "manta")
manta_plastic<- subset(particles_plastic, particles_plastic$SmplMtr %in% "manta")


###############


### Sediment Summary Tables for making Sample ID table

sed_Rubber_table <- sed_Rubber %>%
  filter(!is.na(name)) %>%# Filter out NA
  group_by(SamplID, SamplDt, StatnCd) %>% # Added SampleMatrix here to group by sample type as well as plastic type
  #filter(PlasticType %in% PlasticType.filterlist) %>% # Filters for only plastic types within a list
  tally()

sed_Potent_Rubber_table <- sed_Potent_Rubber %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

sed_plastic_TWP_table <- sed_TWP_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()


sed_TWP_table <- sed_TWP %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

sed_plastic_table <- sed_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

#######################

### Effluent Summary Tables for making Sample ID table

eff_Rubber_table <- eff_Rubber %>%
  filter(!is.na(name)) %>%# Filter out NA
  group_by(SamplID, SamplDt, StatnCd) %>% # Added SampleMatrix here to group by sample type as well as plastic type
  #filter(PlasticType %in% PlasticType.filterlist) %>% # Filters for only plastic types within a list
  tally()

eff_Potent_Rubber_table <- eff_Potent_Rubber %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

eff_plastic_TWP_table <- eff_TWP_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()


eff_TWP_table <- eff_TWP %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

eff_plastic_table <- eff_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

#########################

### Manta Trawl Summary Tables for making Sample ID table

manta_Rubber_table <- manta_Rubber %>%
  filter(!is.na(name)) %>%# Filter out NA
  group_by(SamplID, SamplDt, StatnCd) %>% # Added SampleMatrix here to group by sample type as well as plastic type
  #filter(PlasticType %in% PlasticType.filterlist) %>% # Filters for only plastic types within a list
  tally()

manta_Potent_Rubber_table <- manta_Potent_Rubber %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

manta_plastic_TWP_table <- manta_TWP_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()


manta_TWP_table <- manta_TWP %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()

manta_plastic_table <- manta_plastic %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd) %>% 
  tally()



######################

### Polypropylene
Polypropylene <- subset(particles_plastic, particles_plastic$PlstcTy %in% "Polypropylene")

Polypropylene.manta <- subset(Polypropylene, Polypropylene$SmplMtr %in% "manta")
Polypropylene.eff <- subset(Polypropylene, Polypropylene$SmplMtr %in% "eff")
Polypropylene.sed <- subset(Polypropylene, Polypropylene$SmplMtr %in% "sed")
Polypropylene.storm <- subset(Polypropylene, Polypropylene$SmplMtr %in% "sw")
Polypropylene.fish <- subset(Polypropylene, Polypropylene$SmplMtr %in% "fish")


Polypropylene.manta.table <- Polypropylene.manta %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()

Polypropylene.eff.table <- Polypropylene.eff %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()

Polypropylene.sed.table <- Polypropylene.sed %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()

Polypropylene.storm.table <- Polypropylene.storm %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()

Polypropylene.fish.table <- Polypropylene.fish %>%
  filter(!is.na(name)) %>%
  group_by(SamplID, SamplDt, StatnCd, name) %>% 
  tally()

# Need to pull datasheet for Manta IDs over 
manta_SampleID <- read_csv("manta.plastic_TWP.table.csv")
Polypropylene.manta.table <- Polypropylene.manta.table %>% rename(SampleID = SamplID)
new.manta <- merge(manta_SampleID, Polypropylene.manta.table, by.y ="SampleID")
new.manta <- new.manta[c(1,14)]




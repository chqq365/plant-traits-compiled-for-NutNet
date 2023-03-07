## compiling trait data from TRY (version 6), AusTraits(2.1.2), BIEN(version 1.2.6)
## and Nutnet (leaf traits)
## add root traits too from Groot (did not include due to lack of standardized species names)

## for TRY, data (Height, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass, SLA) were downloaded 
## February 13 2023 (Request No: 24964)

## focus on Height, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass, SLA
rm(list=ls())
## package needed 
library(data.table)
library(tidyverse)
library(GGally)
library(FactoMineR)
library(factoextra)
library(FD)
library(BIEN)
# https://traitecoevo.github.io/austraits/
# install.packages("remotes")
library(remotes)
# remotes::install_github("traitecoevo/austraits", build = F, force = TRUE)
library(austraits)

library(rworldmap) 
library(sp)

# set the working directory 
dir.data<-"H:/plant traits for NutNet/raw data/"
dir.graphs<-"H:/plant traits for NutNet/graphs/"
setwd(dir.graphs)

###################################################################################################
########################load and sort the trait data from the TRY##################################
###################################################################################################
tr1<-fread(paste0(dir.data, "24964.txt"))
unique(tr1$TraitName)
colnames(tr1)
# add location information; find data for latitude, longitude and altitude under DataIDs 59, 60 and 61 as suggested by Jens Kattge
check.names<-unique(tr1$DataName)

tr2.location<-tr1%>%select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue)%>%
  filter(DataID%in%c(59, 60, 61))%>%distinct()
table(tr2.location$DataName)
unique(tr2.location$OriglName)
tr2.location1<-tr2.location%>%mutate(OriglName=NULL, DataID=NULL, OrigValueStr=NULL)%>%distinct()%>%
  pivot_wider(names_from = "DataName", values_from = StdValue)%>%distinct()

## select colnames interested 
## change the consolidated species names to match the data from nutrient network 
## capitalize all the species names 
## delete trait names that are empty 
tr2<-tr1%>%select("DatasetID", "ObservationID", "AccSpeciesName" ,"TraitName" ,  "OrigValueStr", "OrigUnitStr", "ValueKindName","OrigUncertaintyStr", "UncertaintyName",  "StdValue", "UnitName", "ErrorRisk")%>%
  dplyr::rename(standard_taxon1=AccSpeciesName)%>%mutate(standard_taxon=toupper(standard_taxon1), standard_taxon1=NULL)%>%filter(!TraitName=="")%>%
  merge(tr2.location1, by=c("ObservationID"), all.x=T)

###################################################################################################
########################## check trait data from the TRY (units) ##################################
###################################################################################################
## check leaf nutrients based on per leaf area and per leaf dry mass
tr.area<-tr2[grep("per leaf area", tr2$TraitName),]
unique(tr.area$UnitName)
tr.mass<-tr2[grep("per leaf dry mass", tr2$TraitName),]
## more data for per leaf dry mass, better focus on this 

## call all records including leaf area as leaf area, similarly, all records including specific leaf area as SLA
## regardless of petiole excluded or included 

## check Plant height generative 
che.generative<-tr2%>%filter(TraitName=="Plant height generative")
unique(che.generative$UnitName) # "m" 

table(che.generative$ValueKindName)## measured in many ways, but single is the commonest way
che.vege<-tr2%>%filter(TraitName=="Plant height vegetative")
unique(che.vege$UnitName)##  "m" 
table(che.vege$ValueKindName)# mainly measured by Single
## include both generative and vegetative height
## delete trait names that we are not interested 
tr3<-tr2[-grep("content per leaf area", tr2$TraitName),]
unique(tr3$TraitName)

## adjust the names for traits 
tr3$TraitName1<-tr3$TraitName
tr3$TraitName1[grep("Leaf nitrogen", tr3$TraitName)]<-"Leaf N"
tr3$TraitName1[grep("Leaf phosphorus", tr3$TraitName)]<-"Leaf P"
tr3$TraitName1[grep("Leaf potassium", tr3$TraitName)]<-"Leaf K"
tr3$TraitName1[grep("Leaf carbon", tr3$TraitName)]<-"Leaf C"
tr3$TraitName1[grep("Leaf area per leaf dry mass", tr3$TraitName)]<-"SLA"
tr3$TraitName1[grep("compound leaves", tr3$TraitName)]<-"LA"
tr3$TraitName1[grep("height", tr3$TraitName)]<-"Height"
tr3$TraitName1[grep("Plant biomass", tr3$TraitName)]<-"Aboveground biomass"
unique(tr3$TraitName1)

##delete Error risk > 4  
##delete data without unit names 
##delete NAs 
# delete repeats
tr4<-tr3%>%filter(ErrorRisk<=4)%>%filter(!UnitName=="")%>%filter(!is.na(StdValue))%>%unique()
## check trait name and unitname
trait.unit<-tr4%>%select(TraitName1, UnitName)%>%unique()
## indicate database name 
tr5<-tr4%>%mutate(database="TRY")
unique(tr5$TraitName1)

# check data set that require authorship 
author.req<-readxl::read_excel(paste0(dir.data, "authorship required from TRY.xlsx"))
check.restrition<-tr5%>%filter(DatasetID %in%author.req$DatasetID)
nrow(check.restrition)/nrow(tr5) # 10% 
table(check.restrition$DatasetID)
table(check.restrition$TraitName1)
check.species.restrition<- unique(check.restrition$standard_taxon)

# delete datasetID that require authorships 
tr5.authorship.free<-tr5%>%filter(!DatasetID %in% author.req$DatasetID)
check.species.free<- unique(tr5.authorship.free$standard_taxon)
check.unique.species.not.free<-check.species.restrition[!check.species.restrition%in%check.species.free]
length(check.unique.species.not.free)/length(check.species.free)
# very few unique species are added from those restricted data
# therefore better to exclude those data sets that require authorships

###################################################################################################
################################## add traits from BIEN  ##########################################
###################################################################################################

BIEN_trait_list()
tr.bien<- BIEN_trait_trait(trait= c("maximum whole plant height", "whole plant height",
                                    "leaf area",
                                    "leaf area per leaf dry mass",
                                    "leaf carbon content per leaf dry mass",
                                    "leaf nitrogen content per leaf dry mass",
                                    "leaf phosphorus content per leaf dry mass",
                                    "seed mass", "root dry mass", "stem dry mass"))
unique(tr.bien$trait_name)
unique(tr.bien$method)
colnames(tr.bien)
# select variables of interesting including lat and long data 
# delete repeats (a lot of repeats)
tr.bien1<-tr.bien%>%select("scrubbed_species_binomial", "trait_name",  "trait_value", "unit", "latitude","longitude", "elevation_m")%>%                
  dplyr::rename(standard_taxon1=scrubbed_species_binomial)%>%
  mutate(standard_taxon=toupper(standard_taxon1), standard_taxon1=NULL, TraitName1=trait_name, UnitName=unit, StdValue=as.numeric(trait_value))%>%
  mutate(database="BIEN")%>%unique()
unique(tr.bien1$UnitName)

###################################################################################################
################################### traits from  Australia ########################################
###################################################################################################
austraits <- load_austraits(version="4.0.0")
summary(austraits)
austraits$taxonomic_updates

# Extracting all traits data and assigning it to an object
au<- austraits$traits
colnames(au)
## check trait records for each trait
che.rec<-au%>%group_by(trait_name)%>%dplyr::summarise(N.rec=length(value), N.spp=length(unique(taxon_name)))
che.rec1<-che.rec[order(che.rec$N.rec, decreasing = TRUE), ]
che.rec2<-che.rec1[1:30,]
che.rec2$trait_name
length(unique(au$taxon_name))
traits <- au$trait_name %>% unique()  #All possible traits 
(leaf_traits <- traits[str_which(traits, "leaf")]) # Extracting data where "leaf" occurs in the trait_name
(leaf_area<- leaf_traits[str_which(leaf_traits, "area")])
(height_traits <- traits[str_which(traits, "height")]) # Extracting data where "height" occurs in the trait_name
(biomass_shoot_traits <- traits[str_which(traits, "shoot")]) # Extracting data where "shoot" occurs in the trait_name
(biomass_traits <- traits[str_which(traits, "mass")]) # Extracting data where "biomass" occurs in the trait_name
(seed<-biomass_traits[str_which(traits, "seed")])
(root_traits <- traits[str_which(traits, "root")]) # Extracting data where "root" occurs in the trait_name

# add location 
unique(austraits$locations$location_property)
loc<-austraits$locations
che.loc<-loc%>%group_by(location_property)%>%dplyr::summarise(N=length(value))
che.loc1<-che.loc[order(che.loc$N, decreasing = TRUE), ]
che.loc2<-che.loc1[1:30,]

site.inf<-austraits$locations%>%filter(grepl("lat|long|elev", location_property))%>%filter(!is.na(value))%>%
  # unique(site.inf$location_property)
  # table(site.inf$location_property) # a few weird names, but majority of them were named latitude (deg) and longitude (deg)
  filter(location_property %in% c("latitude (deg)", "longitude (deg)", "elevation (m)"))%>%
  mutate(site.pro=ifelse(location_property=="longitude (deg)", "longitude", ifelse(location_property=="latitude (deg)", "latitude", "altitude")), location_property=NULL)%>%
  pivot_wider(names_from ="site.pro", values_from = "value")
colnames(site.inf)  

## select traits that interesting
traits.list<-c("leaf_area", "leaf_N_per_dry_mass", "leaf_mass_per_area", "seed_dry_mass", "plant_height", 
               "leaf_P_per_dry_mass", "leaf_C_per_dry_mass", 
               "leaf_K_per_dry_mass", "root_N_per_dry_mass", "root_dry_matter_content", "root_C_per_dry_mass", "root_specific_root_area")

## get species with continuous traits listed above
colnames(au)
au1<-au%>%filter(trait_name%in%traits.list)%>%merge(site.inf, by=c("dataset_id", "location_id"), all.x=T)
table(au1$trait_name)

## check whether each trait name has only one unit 
(che.unit<-au%>%filter(trait_name%in%traits.list)%>%select(trait_name, unit)%>%distinct())
# attention, leaf_mass_per_area is 1/SLA
## add the units and indicate that trait data were collected from Australia
# delete repeats
au2<-au1%>%mutate(database="Aus")%>%unique()

###################################################################################################
#############################leaf trait data from NutNet ##########################################
###################################################################################################
# for leaf nutrients
l.nutnet<-read.csv(paste0(dir.data, "foliar_cover_updated_3.csv"))
str(l.nutnet)
colnames(l.nutnet)[1]<-"site_code"
## convert to long version 
colnames(l.nutnet)
l.nutnet1<-l.nutnet%>%
  select("continent",   "country", "region",  "site_code", "block",  "plot", "trt", "year", "latitude",   "longitude", "Taxon", "leaf_pct_N", "leaf_pct_C", "leaf_pct_P",  "leaf_pct_K", "SLA_v2" )%>%
  pivot_longer(cols = leaf_pct_N:SLA_v2)%>%
  mutate(units=ifelse(name %in% c("SLA_v2"), "mm^2 / g", "%"))%>%filter(!is.na(value))%>%mutate(database="NutNet")%>%unique()
# also add root traits 
l.nut.root<-read.csv(paste0(dir.data, "root-biomass-Cleand-et-al-2019.csv"))
# seems like root biomass data at the community level, this does not help. 

###################################################################################################
#############################root trait data from Groot ##########################################
###################################################################################################
# (Guerrero-Ram?rez et al., 2021)
# Global root traits (GRooT) database. Global Ecology and Biogeography, 30(1), 25-37. https://doi.org/10.1111/geb.13179
# We standardized original species names using the Taxonomic Name Resolution Service v.4.0
# (i.e., TNRS; http://tnrs.iplan tcoll abora tive.org/; accessed September 2019; Boyle et al., 2013), 
groots<-read.csv(paste0(dir.data, "GRooTFullVersion.csv"))
colnames(groots)
groots1<-groots%>%select("family", "genus", "species", "familyTNRS", "genusTNRS", "speciesTNRS", "traitName",  "traitValue", "errorRiskEntries", "errorRisk", "location", "decimalLatitude", "decimalLongitud", "elevation")                      
# check species 
check.species<-groots1%>%select("family", "genus", "species", "familyTNRS", "genusTNRS", "speciesTNRS")%>%distinct()
# really weird species names  (also do not understand what is speciesTNRS)
###################################################################################################
################################## combine all traits together#####################################
###################################################################################################
## unify species name, trait name, and trait unit
colnames(tr5.authorship.free)
(uni.try.nu<-tr5.authorship.free%>%group_by(TraitName1, UnitName)%>%select(TraitName1, UnitName)%>%distinct())
tr6<-tr5.authorship.free%>%select(database, Latitude, Longitude, Altitude, standard_taxon, TraitName1, UnitName, StdValue)
###################################################################################################
## for data from Australia 
colnames(au2)
(uni.au.nu<-au2%>%group_by(trait_name, unit)%>%select(trait_name, unit)%>%distinct())
## units were NOT the same as TRY
# 1/SLA   mg/mm2 (TRY) = 1 kg/m2 = 1000 g/m2 (aus)
## simplify the trait name 
au2$trait_name1<-au2$trait_name
au2$trait_name1[au2$trait_name=="leaf_N_per_dry_mass"]<-"Leaf N"
au2$trait_name1[au2$trait_name=="leaf_C_per_dry_mass"]<-"Leaf C"
au2$trait_name1[au2$trait_name=="leaf_P_per_dry_mass"]<-"Leaf P"
au2$trait_name1[au2$trait_name=="leaf_K_per_dry_mass"]<-"Leaf K"
au2$trait_name1[au2$trait_name=="leaf_area"]<-"LA"
au2$trait_name1[au2$trait_name=="plant_height"]<-"Height"
au2$trait_name1[au2$trait_name=="leaf_mass_per_area"]<-"SLA"
au2$trait_name1[au2$trait_name=="seed_dry_mass"]<-"Seed dry mass"
unique(au2$trait_name1)
## capitalize the species names 
colnames(au2)
au3<-au2%>%mutate(standard_taxon=toupper(taxon_name), value0=as.numeric(value))%>%mutate(value1=ifelse(trait_name1=="SLA", 1/(value0/1000), value0))%>%
  select(database, latitude, longitude, altitude, standard_taxon, trait_name1, unit, value1)
names(au3)<-c("database", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue")
check.sla<-au3%>%filter(TraitName1=="SLA")
range(check.sla$StdValue)
# compare with try 
range(tr6[tr6$TraitName1=="SLA",]$StdValue)
###################################################################################################
## for data from BIEN
colnames(tr.bien1)
(uni.us.nu<-tr.bien1%>%group_by("TraitName1", "UnitName")%>%select("TraitName1", "UnitName")%>%distinct())
tr.bien2<-tr.bien1%>%mutate(StdValue1=ifelse(TraitName1=="stem dry mass", StdValue*1000, StdValue))
## simplify the trait name 
unique(au2$trait_name1)
tr.bien2$TraitName11<-tr.bien2$TraitName1
tr.bien2$TraitName11[tr.bien2$TraitName1=="leaf nitrogen content per leaf dry mass"]<-"Leaf N"
tr.bien2$TraitName11[tr.bien2$TraitName1=="leaf carbon content per leaf dry mass"]<-"Leaf C"
tr.bien2$TraitName11[tr.bien2$TraitName1=="leaf phosphorus content per leaf dry mass"]<-"Leaf P"
tr.bien2$TraitName11[tr.bien2$TraitName1=="leaf area"]<-"LA"
tr.bien2$TraitName11[tr.bien2$TraitName1=="leaf area per leaf dry mass"]<-"SLA"
tr.bien2$TraitName11[tr.bien2$TraitName1=="seed mass"]<-"Seed dry mass"
tr.bien2$TraitName11[tr.bien2$TraitName1=="stem dry mass"]<-"Aboveground biomass"
tr.bien2$TraitName11[tr.bien2$TraitName1=="maximum whole plant height"|tr.bien2$TraitName1=="whole plant height"]<-"Height"
unique(tr.bien2$TraitName11)
colnames(tr.bien2)
tr.bien3<-tr.bien2%>%ungroup()%>%select(database, latitude, longitude, elevation_m, standard_taxon, TraitName11, UnitName, StdValue1)
names(tr.bien3)<-c("database", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue")

###################################################################################################
## for nutnet data 
colnames(l.nutnet1)
(uni.nutnet.nu<-l.nutnet1%>%group_by(name, units)%>%select(name, units)%>%distinct())
# mg/g=(%)*10
# mm2 mg-1=(mm^2 / g)/1000
l.nutnet2<-l.nutnet1%>%dplyr::rename(TraitName=name)%>%mutate(StdValue1=ifelse(TraitName=="SLA_v2", value/1000, value*10), UnitName=ifelse(TraitName=="SLA_v2", "mm^2 / g", "mg/g"))
range(subset(l.nutnet2, TraitName=="leaf_pct_C")$StdValue1); range(subset(tr6, TraitName1=="Leaf N")$StdValue)

## simplify the trait name 
l.nutnet2$TraitName1<-l.nutnet2$TraitName
l.nutnet2$TraitName1[l.nutnet2$TraitName=="leaf_pct_N"]<-"Leaf N"
l.nutnet2$TraitName1[l.nutnet2$TraitName=="leaf_pct_C"]<-"Leaf C"
l.nutnet2$TraitName1[l.nutnet2$TraitName=="leaf_pct_P"]<-"Leaf P"
l.nutnet2$TraitName1[l.nutnet2$TraitName=="leaf_pct_K"]<-"Leaf K"
l.nutnet2$TraitName1[l.nutnet2$TraitName=="SLA_v2"]<-"SLA"
colnames(l.nutnet2)
l.nutnet3<-l.nutnet2%>%mutate(elevation_m=NA)%>%select(database, latitude, longitude, elevation_m, Taxon, TraitName1, UnitName, StdValue1)
names(l.nutnet3)<-c("database", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue")

###################################################################################################
################################### add all traits together########################################
###################################################################################################
# add traits from all sources together 
colnames(tr6)
colnames(au3)
str(tr6)
str(au3)
au3$Latitude<-as.numeric(au3$Latitude)
au3$Longitude<-as.numeric(au3$Longitude)
au3$Altitude<-as.numeric(au3$Altitude)

all.traits<-tr6%>%bind_rows(y=au3)%>%bind_rows(y=tr.bien3)%>%bind_rows(y=l.nutnet3)%>%unique()  

# add continent information to the data using the following function
# https://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  c(indices$REGION,   # returns the continent (7 continent model)
    indices$ADMIN)  #returns country name
}
points <-all.traits%>%select(Longitude, Latitude)%>%unique()%>%filter(!is.na(Longitude))%>%filter(!is.na(Latitude))
temp.data<-coords2continent(points)
length(temp.data)
points$continent<-temp.data[1:(length(temp.data)/2)]
unique(points$continent)
points$country<-temp.data[((length(temp.data)/2)+1):length(temp.data)]
# add continents and countries 
# adjust unit name for each trait
all.traits1<-all.traits%>%merge(y=points, by=c("Longitude", "Latitude"))%>%
  mutate(UnitName1=case_when(TraitName1 %in% c("Leaf C", "Leaf N", "Leaf P", "Leaf K")~"mg/g",
                             TraitName1 %in% c("Height")~"m",
                             TraitName1 %in% c("SLA")~"mm2/mg",
                             TraitName1 %in% c("LA")~"mm2",
                             TraitName1 %in% c("Seed dry mass")~"mg",
                             TraitName1 %in% c("Aboveground biomass")~"g",
                             TRUE~UnitName), UnitName=NULL)
# save this data set 
# check trait value distribution
ggplot(all.traits1)+geom_histogram(aes(x=StdValue))+facet_wrap(~TraitName1, scales="free")
# fwrite(all.traits1, file ="combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv" )


###################################################################################################
###################### check trait records for species occur at NutNet sites#######################
###################################################################################################

# read the cover data and adjust the taxa for NutNet sites 
d.cov<-read.csv(paste0(dir.data, "full-cover-2022-11-15.csv"), header = T, sep=",")
taxa.adj<-read.csv(paste0(dir.data, "site-taxonomy-2022-07-13.csv"))
taxa.adj1<-taxa.adj%>%mutate(Taxon=toupper(local_name), standard_taxon_temp=toupper(standard_taxon))
# check whether we need to adjust taxa names
check.taxa1<-d.cov%>%filter(Taxon%in%taxa.adj1$Taxon)%>%unique()

# focus on live plants; ensure maximum cover is 100%; adjust taxa 
d.cov1<-d.cov%>%filter(live==1)%>%mutate(max_cover1 = ifelse(max_cover > 100, 100, max_cover))%>%
  merge(taxa.adj1[,c("site_code", "Family", "Taxon", "standard_taxon_temp")], by=c("site_code", "Taxon"), all.x=T)%>%
  mutate(standard_taxon=ifelse(is.na(standard_taxon_temp), Taxon, standard_taxon_temp), Family=ifelse(is.na(Family.y), Family.x, Family.y))
check.taxa1<-d.cov1%>%filter(is.na(standard_taxon_temp))
# Drop mosses, lichens, fungi
d7 <- d.cov1[! d.cov1$functional_group %in% c("BRYOPHYTE", "LICHEN",  "LIVERWORT", "NON-LIVE") , ]
#some families not consistently identified to functional group
d7 <- d7[! d7$Family %in% c( "Phallales") , ]
all.spp<-unique(d7$standard_taxon) # species list 

table(all.traits1$TraitName1)
table(all.traits1$continent)
table(all.traits1$database)


all.traits.for.nutnet<-all.traits1%>%filter(standard_taxon%in%d7$standard_taxon)%>%
  # filter(!is.na(continent))%>% # reduce quite records, maybe no need to filter out data without geolocation
  arrange(continent, standard_taxon, TraitName1)
table(all.traits.for.nutnet$continent)

# average over continents 
all.traits.for.nutnet.avg<-all.traits.for.nutnet%>%group_by(standard_taxon, TraitName1)%>%summarise(avg.value=mean(StdValue))
table(all.traits.for.nutnet.avg$TraitName1) # very few records for root traits and Aboveground biomass  
all.traits.for.nutnet.avg.1<-all.traits.for.nutnet.avg[-grep("root|Aboveground", all.traits.for.nutnet.avg$TraitName1),]
table(all.traits.for.nutnet.avg.1$TraitName1) # root traits and Aboveground biomass were excluded 

# get species level trait data
sp.level.trait.nutnet<-all.traits.for.nutnet.avg.1%>%filter(standard_taxon%in%all.spp)


# fill NAs for species for each trait without species-level trait value
all.traits1a<-all.traits1[-grep("root|Aboveground", all.traits1$TraitName1),]# excluded root traits  
all.traits2<-all.traits1a%>%mutate(genus=gsub( " .*$", "", standard_taxon)) # add genus data
unique(all.traits2$TraitName1)

all.traits.fill<-c()
for(tr in unique(all.traits2$TraitName1)){
  # tr<-"Leaf N"
  sp.trait<-sp.level.trait.nutnet%>%filter(TraitName1==tr)
  fill.trait<-d7%>%select(standard_taxon)%>%distinct()%>%filter(!standard_taxon%in%sp.trait$standard_taxon)%>%
    mutate(genus=gsub( " .*$", "", standard_taxon))
  for(g in unique(fill.trait$genus)){
    # g<-"POA"
    # first summarize mean trait value for each species, then summarize mean trait value for each genus 
    original.traits<-all.traits2%>%filter(TraitName1==tr & genus==g)%>%group_by(TraitName1, genus, standard_taxon)%>%
      dplyr::summarise(trait_value_avg_species=mean(StdValue))%>%
      group_by(genus)%>%dplyr::summarise(trait_value_avg=mean(trait_value_avg_species))
    fill.trait1<-fill.trait%>%filter(genus==g)%>%mutate(avg.value=ifelse(nrow(original.traits)==0, NA,  original.traits$trait_value_avg), data.type="average.values.at.genus.level")
    fill.trait1$TraitName1<-tr
    all.traits.fill<-rbind(all.traits.fill, fill.trait1)
  }
}
all.traits.fill1<-all.traits.fill%>%filter(!is.na(avg.value))%>%select(standard_taxon, TraitName1, avg.value, data.type)

# add this data frame to the one with trait data at species level
sp.and.genus.level.traits.nutnet<-sp.level.trait.nutnet%>%mutate(data.type="traits.at.species.level")%>%bind_rows(all.traits.fill1)
## check records for each species 
table(sp.and.genus.level.traits.nutnet$TraitName1)

# save the data 
# fwrite(sp.and.genus.level.traits.nutnet, file="species and genus level traits from TRY, Aus, BIEN, and NutNet for NutNet species.csv")

###################################################################################################
######################check trait values, distributions, and correlations##########################
###################################################################################################
sp.nutnet1<-sp.and.genus.level.traits.nutnet%>%filter(!is.na(TraitName1))
colnames(sp.nutnet1)
table(sp.nutnet1$TraitName1)
# check data distribution 
ggplot(sp.nutnet1)+geom_histogram(aes(x=avg.value))+facet_wrap(~TraitName1, scale="free_x")

# get traits for these species, delete extreme values by using 5-95% quantile
sp.nutnet2<-sp.nutnet1%>%group_by(TraitName1)%>%filter(avg.value < quantile(avg.value, 0.95))%>%
  filter(avg.value > quantile(avg.value, 0.05))%>%mutate(extreme.values="No")
table(sp.nutnet2$TraitName1)
# check trait value distribution including and excluding extreme values 
sp.nutnet_12<-sp.nutnet1%>%mutate(extreme.values="Yes")%>%bind_rows(sp.nutnet2)
sp.nutnet_12$extreme.values<-factor(sp.nutnet_12$extreme.values, levels = c("Yes", "No"))
(pp.distribution<-ggplot(sp.nutnet_12)+theme_bw(base_size = 20)+
    geom_density(aes(x=avg.value, color=extreme.values, fill=extreme.values))+
    facet_wrap(~TraitName1, scales="free"))
ggsave(pp.distribution, width=13.3,height=6.64, file="distribution of trait values including and excluding extreme values.png")


# change data to wide version, focus on traits with high coverage for species (leaf K was deleted)
sp.nutnet.w<-sp.nutnet2%>%mutate(data.type=NULL, extreme.values=NULL)%>%filter(TraitName1 %in%c("Height", "Seed dry mass", "SLA", "LA", "Leaf N", "Leaf C", "Leaf P"))%>%
  pivot_wider(names_from = TraitName1, values_from = avg.value)

colnames(sp.nutnet.w)<-c("standard_taxon", "Leaf N", "Height",  "LA",  "Leaf P", "Seed mass",  "SLA",  "Leaf C")
(pp1<-ggpairs(sp.nutnet.w[,c("Height", "Seed mass", "LA", "Leaf C", "Leaf N", "Leaf P",  "SLA")])+theme_bw(base_size = 20)+
    labs(title=paste0("pairwise correlation based on ", nrow(sp.nutnet.w), " NutNet species (in total ", length(all.spp), " species occurred)")))
ggsave(pp1, width=13.3,height=6.64, file="correlation among traits.png")

## check PCA plot using 6 traits with all traits 
sp.nutnet.w.na.omit<-sp.nutnet.w %>% filter_at(vars("Height", "Seed mass", "LA", "Leaf C", "Leaf N", "Leaf P", "SLA"),all_vars(!is.na(.)))
# 
pca.r<-PCA(sp.nutnet.w.na.omit[,c("Height", "LA", "Leaf C", "Leaf N", "Leaf P", "Seed mass", "SLA")], scale=T, graph=F)
summary(pca.r)## 
# visualize 
fviz_pca_var(pca.r, label = "var", alpha.ind =0.5, pointsize=2, title = NULL)+theme_bw(base_size=18)
(pp2<-fviz_pca_biplot(pca.r, label = "var", alpha.ind =0.5, pointsize=2, title = paste0("PCA based on ", nrow(sp.nutnet.w.na.omit), " species having all 7 traits (in total ", length(all.spp), " species occurred)"))+theme_bw(base_size=18))
ggsave(pp2, width=13.3,height=6.64, file="pca among traits.png")

# there are two sets of traits, related to size and leaf traits 
# including all these traits reduces number of species strongly. 
# maybe focus on individual trait, such as height and seed mass for size, leaf N and SLA for leaf. 

# sort the trait information in a table 
trait.inf1<-sp.nutnet2%>%group_by(TraitName1)%>%summarise(percent.species.with.traits=length(avg.value)/length(all.spp))
trait.inf2<-sp.nutnet2%>%group_by(TraitName1, data.type)%>%summarise(percent.species.with.traits=length(avg.value)/length(all.spp))%>%
  pivot_wider(names_from = data.type, values_from = percent.species.with.traits)
trait.inf3<-all.traits1%>%filter(standard_taxon%in%d7$standard_taxon)%>%select(database, standard_taxon, TraitName1, StdValue)%>%distinct()%>%
  group_by(database, standard_taxon, TraitName1)%>%summarise(avg.value=mean(StdValue))%>%
  group_by(TraitName1, database)%>%summarise(percent.species.with.traits=length(avg.value)/length(all.spp))%>%
  pivot_wider(names_from = database, values_from = percent.species.with.traits)
trait.inf4<-trait.inf1%>%merge(y=trait.inf2, by=c("TraitName1"))%>%merge(y=trait.inf3, by=c("TraitName1"))%>%
  arrange(-percent.species.with.traits)
trait.inf4[,2:8]<-lapply(trait.inf4[,2:8], function(x){x*100} )
trait.inf4[,2:8]<-round(trait.inf4[,2:8], 0)
colnames(trait.inf4)<-c("TraitName", "percent species with traits",                                
                        "percent species with traits at genue level",                  "percent species with traits at species level",               
                        "percent species with traits at species level from TRY",       "percent species with traits at species level from AusTrait" ,
                        "percent species with traits at species level from BEIN data",  "percent species with traits at species level from NutNet" )  


# fwrite(trait.inf4, file="summary of trait information.csv")

# the end 
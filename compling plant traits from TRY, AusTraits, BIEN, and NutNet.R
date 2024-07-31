## compiling trait data from TRY (version 6), AusTraits(2.1.2), BIEN(version 1.2.6)
## and Nutnet (leaf traits)
## add root traits too from Groot (did not include due to lack of standardized species names)

## for TRY, data (Height, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass, SLA) were downloaded 
## February 13 2023 (Request No: 24964)
## LDMC was downloaded on March 04 2024 (Request No: 31973)
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
#library(remotes)
# remotes::install_github("traitecoevo/austraits", build = F, force = TRUE)
library(austraits)

library(rworldmap) 
library(sp)

# set the working directory 
dir.data<-"C:/Users/chqq3/work/plant traits compiled for NutNet/raw data/"
dir.graphs<-"C:/Users/chqq3/work/plant traits compiled for NutNet/"
setwd(dir.graphs)

###################################################################################################
########################load and sort the trait data from the TRY##################################
###################################################################################################
tr1<-fread(paste0(dir.data, "24964.txt"))
unique(tr1$TraitName)
tr1.ldmc<-fread(paste0(dir.data, "31973.txt"))
unique(tr1.ldmc$TraitName)

colnames(tr1); colnames(tr1.ldmc)
# add location information; find data for latitude, longitude and altitude under DataIDs 59, 60 and 61 as suggested by Jens Kattge
check.names<-unique(tr1$DataName)

tr2.location<-tr1%>%select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue)%>%
  filter(DataID%in%c(59, 60, 61))%>%distinct()
table(tr2.location$DataName)
unique(tr2.location$OriglName)
tr2.ldmc.location<-tr1.ldmc%>%select(ObservationID, DataID, DataName, OriglName, OrigValueStr, StdValue)%>%
  filter(DataID%in%c(59, 60, 61))%>%distinct()

tr2.location1<-tr2.location%>%rbind(tr2.ldmc.location)%>%mutate(OriglName=NULL, DataID=NULL, OrigValueStr=NULL)%>%distinct()%>%
  pivot_wider(names_from = "DataName", values_from = StdValue)%>%distinct()

## select variable names interested 
## change the consolidated species names to match the data from nutrient network 
## capitalize all the species names 
## delete trait names that are empty 
tr2<-tr1%>%rbind(tr1.ldmc)%>%select("DatasetID", "ObservationID", "AccSpeciesName" ,"TraitName" ,  "OrigValueStr", "OrigUnitStr", "ValueKindName","OrigUncertaintyStr", "UncertaintyName",  "StdValue", "UnitName", "ErrorRisk")%>%
  dplyr::rename(standard_taxon1=AccSpeciesName)%>%mutate(standard_taxon=toupper(standard_taxon1), standard_taxon1=NULL)%>%filter(!TraitName=="")%>%
  merge(tr2.location1, by=c("ObservationID"), all.x=T)
unique(tr2$TraitName)
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
che.ldmc<-tr2%>%filter(TraitName=="Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)" )
unique(che.ldmc$UnitName) # "g/g"

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
tr3$TraitName1[grep("LDMC", tr3$TraitName)]<-"LDMC"
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
nrow(check.restrition)/nrow(tr5) # 
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
sessionInfo("BIEN")
bien.traits<-BIEN_trait_list()

tr.bien<- BIEN_trait_trait(trait= c("maximum whole plant height", "whole plant height",
                                    "leaf dry mass per leaf fresh mass", 
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
tr.bien1<-tr.bien%>%select("scrubbed_species_binomial", "unit", "latitude", "longitude",  "elevation_m", "url_source", "project_pi",  "project_pi_contact",  "access",  "trait_name","trait_value")%>%                
  mutate(trait_value=as.numeric(trait_value))%>%        
  mutate(standard_taxon=toupper(scrubbed_species_binomial), scrubbed_species_binomial=NULL)%>%
  dplyr::rename(TraitName1=trait_name, UnitName=unit, StdValue=trait_value)%>% mutate(database="BIEN")%>%unique()
unique(tr.bien1$UnitName)

###################################################################################################
################################### traits from  Australia ########################################
###################################################################################################
#sessionInfo("austraits")
#get_versions("austraits")
austraits <- load_austraits(version="5.0.0")
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
(leaf_ldmc<- leaf_traits[str_which(leaf_traits, "fresh")])
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

# calculate LDMC
au%>%filter(trait_name%in% c("leaf_dry_mass", "leaf_fresh_mass"))%>%select(unit)%>%distinct() # same units 
aus.ldmc<-au%>%filter(trait_name%in% c("leaf_dry_mass", "leaf_fresh_mass"))%>%pivot_wider(names_from ="trait_name", values_from = "value" )%>%
  mutate(LDMC=as.numeric(leaf_dry_mass) /as.numeric(leaf_fresh_mass ))%>%filter(! is.na(LDMC))%>%mutate(leaf_dry_mass=NULL, leaf_fresh_mass=NULL)%>%
  pivot_longer(cols = c("LDMC"), names_to ="trait_name", values_to = "value" )%>%relocate(c("trait_name", "value" ), .before=unit)
colnames(aus.ldmc); colnames(au)
## select traits that interesting
traits.list<-c("leaf_area", "leaf_N_per_dry_mass", "leaf_mass_per_area", "seed_dry_mass", "plant_height", 
               "leaf_P_per_dry_mass", "leaf_C_per_dry_mass", 
               "leaf_K_per_dry_mass", "root_N_per_dry_mass", "root_dry_matter_content", "root_C_per_dry_mass", "root_specific_root_area")

## get species with continuous traits listed above
au1<-au%>%filter(trait_name%in%traits.list)%>%rbind(aus.ldmc)%>% merge(site.inf, by=c("dataset_id", "location_id"), all.x=T)
unique(au1$unit)
table(au1$trait_name)

## check whether each trait name has only one unit 
(che.unit<-au1%>%select(trait_name, unit)%>%distinct())
# attention, leaf_mass_per_area is 1/SLA; also the unit for LDMC is mg/mg, which is equal to g/g
## add the units and indicate that trait data were collected from Australia
# delete repeats
au2<-au1%>%mutate(database="Aus")%>%unique()

###################################################################################################
#############################leaf trait data from NutNet ##########################################
###################################################################################################
# for leaf nutrients
l.nutnet<-read.csv(paste0(dir.data, "foliar_cover_updated_3.csv"))
str(l.nutnet)
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
#combine all traits together
# first average within data contributors, then average within databases, then across databases  
###################################################################################################

options(scipen=999, digits = 3)
## unify species name, trait name, and trait unit
colnames(tr5.authorship.free)
(uni.try.nu<-tr5.authorship.free%>%group_by(TraitName1, UnitName)%>%select(TraitName1, UnitName)%>%distinct())
tr6<-tr5.authorship.free%>%group_by(database, DatasetID, Latitude, Longitude, Altitude, standard_taxon, TraitName1, UnitName)%>%mutate(StdValue_avg_within_contributors=mean(StdValue))%>%
  group_by(database, Latitude, Longitude, Altitude, standard_taxon, TraitName1, UnitName)%>%mutate(StdValue_avg_across_contributors=mean(StdValue_avg_within_contributors))%>%
   select(database, DatasetID, ObservationID, Latitude, Longitude, Altitude, standard_taxon, TraitName1, UnitName, StdValue, StdValue_avg_within_contributors, StdValue_avg_across_contributors)%>%arrange(standard_taxon, TraitName1)
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
  group_by(database, dataset_id, latitude, longitude, altitude, standard_taxon, trait_name1, unit)%>%mutate(StdValue_avg_within_contributors=mean(value1))%>%
  group_by(database, latitude, longitude, altitude, standard_taxon, trait_name1, unit)%>%mutate(StdValue_avg_across_contributors=mean(StdValue_avg_within_contributors))%>%
   select(database, dataset_id, observation_id, latitude, longitude, altitude, standard_taxon, trait_name1, unit, value1, StdValue_avg_within_contributors, StdValue_avg_across_contributors)%>%arrange(standard_taxon, trait_name1)
names(au3)<-c("database", "DatasetID", "ObservationID", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue", "StdValue_avg_within_contributors", "StdValue_avg_across_contributors")
check.sla<-au3%>%filter(TraitName1=="SLA")
range(check.sla$StdValue)
# compare with try 
range(tr6[tr6$TraitName1=="SLA",]$StdValue)
###################################################################################################
## for data from BIEN
colnames(tr.bien1)
(uni.us.nu<-tr.bien1%>%group_by("TraitName1", "UnitName")%>%select("TraitName1", "UnitName")%>%distinct())
# units are Not the same, need adjustment 
# leaf dry mass per leaf fresh mass         mg.g-1 
# stem dry mass                             kg   
## simplify the trait name 
unique(tr.bien1$TraitName1)
tr.bien1$TraitName11<-tr.bien1$TraitName1
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf nitrogen content per leaf dry mass"]<-"Leaf N"
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf carbon content per leaf dry mass"]<-"Leaf C"
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf phosphorus content per leaf dry mass"]<-"Leaf P"
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf area"]<-"LA"
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf area per leaf dry mass"]<-"SLA"
tr.bien1$TraitName11[tr.bien1$TraitName1=="leaf dry mass per leaf fresh mass"]<-"LDMC"
tr.bien1$TraitName11[tr.bien1$TraitName1=="seed mass"]<-"Seed dry mass"
tr.bien1$TraitName11[tr.bien1$TraitName1=="stem dry mass"]<-"Aboveground biomass"
tr.bien1$TraitName11[tr.bien1$TraitName1=="maximum whole plant height"|tr.bien1$TraitName1=="whole plant height"]<-"Height"
unique(tr.bien1$TraitName11)
colnames(tr.bien1)

tr.bien3<-tr.bien1%>%mutate(StdValue1=ifelse(TraitName1=="stem dry mass", StdValue*1000, StdValue))%>%
  mutate(StdValue2=ifelse(TraitName1=="leaf dry mass per leaf fresh mass", StdValue1/1000, StdValue1))%>%ungroup()%>%
  group_by(database, project_pi, latitude, longitude, elevation_m, standard_taxon, TraitName11, UnitName)%>%mutate(StdValue_avg_within_contributors=mean(StdValue2))%>%
  group_by(database, latitude, longitude, elevation_m, standard_taxon, TraitName11, UnitName)%>%mutate(StdValue_avg_across_contributors=mean(StdValue_avg_within_contributors))%>%
  select(database, project_pi, url_source, latitude, longitude, elevation_m, standard_taxon, TraitName11, UnitName, StdValue2, StdValue_avg_within_contributors, StdValue_avg_across_contributors)%>%arrange(standard_taxon, TraitName11)
names(tr.bien3)<-c("database","DatasetID", "ObservationID", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue", "StdValue_avg_within_contributors", "StdValue_avg_across_contributors")

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
l.nutnet3<-l.nutnet2%>%mutate(elevation_m=NA)%>%
  group_by(database, site_code, latitude, longitude, elevation_m, Taxon, TraitName1, UnitName)%>%mutate(StdValue_avg_within_contributors=mean(StdValue1))%>%
  group_by(database, latitude, longitude, elevation_m, Taxon, TraitName1, UnitName)%>%mutate(StdValue_avg_across_contributors=mean(StdValue_avg_within_contributors))%>%
  select(database,site_code, trt, latitude, longitude, elevation_m, Taxon, TraitName1, UnitName, StdValue1, StdValue_avg_within_contributors, StdValue_avg_across_contributors)%>%arrange(Taxon, TraitName1)
names(l.nutnet3)<-c("database","DatasetID", "ObservationID", "Latitude", "Longitude", "Altitude", "standard_taxon", "TraitName1", "UnitName", "StdValue", "StdValue_avg_within_contributors", "StdValue_avg_across_contributors")

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

all.traits<-tr6%>%mutate(DatasetID=as.character(DatasetID), ObservationID=as.character(ObservationID), Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude), Altitude=as.numeric(Altitude))%>%
  bind_rows(y=au3%>%mutate(DatasetID=as.character(DatasetID), ObservationID=as.character(ObservationID), Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude), Altitude=as.numeric(Altitude)))%>%
  bind_rows(y=tr.bien3%>%mutate(DatasetID=as.character(DatasetID), ObservationID=as.character(ObservationID), Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude), Altitude=as.numeric(Altitude)))%>%
  bind_rows(y=l.nutnet3%>%mutate(DatasetID=as.character(DatasetID), ObservationID=as.character(ObservationID), Latitude=as.numeric(Latitude), Longitude=as.numeric(Longitude), Altitude=as.numeric(Altitude)))%>%
  unique()  

# adjust unit name for each trait
colnames(all.traits)
unique(all.traits$TraitName1)

all.traits1<-all.traits%>%ungroup()%>%filter(!grepl("root", TraitName1))%>%
  mutate(UnitName1=case_when(TraitName1 %in% c("Leaf C", "Leaf N", "Leaf P", "Leaf K")~"mg/g",
                             TraitName1 %in% c("Height")~"m",
                             TraitName1 %in% c("SLA")~"mm2/mg",
                             TraitName1 %in% c("LA")~"mm2",
                             TraitName1 %in% c("Seed dry mass")~"mg",
                             TraitName1 %in% c("Aboveground biomass")~"g",
                             TraitName1 %in% c("LDMC")~"%"), UnitName=NULL)
check.traits.and.units<-all.traits1%>%ungroup()%>%select(TraitName1, UnitName1)%>%distinct()

# check trait value distribution
ggplot(all.traits1)+geom_histogram(aes(x=StdValue))+facet_grid(database~TraitName1, scales="free_x")
# check data range 
check.range<-all.traits1%>%group_by(database, TraitName1)%>%summarise(min.v=min(StdValue), max.v=max(StdValue))
# LDMC should not be more than 1, but this is observed in AusTraits and BIEN
# save this data set 
# fwrite(all.traits1, file ="combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv" )

###################################################################################################
###################### check trait records for species occur at NutNet sites#######################
###################################################################################################

# read the cover data and adjust the taxa for NutNet sites 
d.cov<-read.csv(paste0(dir.data, "full-cover_2023-11-07.csv"), header = T, sep=",")
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
table(all.traits1$database)

all.traits1<- read.csv("combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv")

all.traits.for.nutnet<-all.traits1%>%filter(standard_taxon%in%d7$standard_taxon)%>%
  # filter(!is.na(continent))%>% # reduce quite records, maybe no need to filter out data without geolocation
  arrange(standard_taxon, TraitName1)

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
sp.nutnet.w<-sp.nutnet2%>%mutate(data.type=NULL, extreme.values=NULL)%>%filter(TraitName1 %in%c("Height", "Seed dry mass", "LDMC", "SLA", "LA", "Leaf N", "Leaf C", "Leaf P"))%>%
  pivot_wider(names_from = TraitName1, values_from = avg.value)

colnames(sp.nutnet.w)<-c("standard_taxon", "Height", "Seed mass", "Leaf N", "LA",  "Leaf P", "SLA", "LDMC", "Leaf C")
(pp1<-ggpairs(sp.nutnet.w[,c("Height", "Seed mass", "LDMC",  "SLA", "LA", "Leaf C", "Leaf N", "Leaf P")])+theme_bw(base_size = 15)+
    labs(title=paste0("pairwise correlation based on ", nrow(sp.nutnet.w), " NutNet species (in total ", length(all.spp), " species occurred)")))
ggsave(pp1, width=13.3,height=6.64, file="correlation among traits.png")

## check PCA plot using 6 traits with all traits 
sp.nutnet.w.na.omit<-sp.nutnet.w %>% filter_at(vars("Height", "Seed mass", "LDMC",  "SLA", "LA", "Leaf C", "Leaf N", "Leaf P"),all_vars(!is.na(.)))
# 
pca.r<-PCA(sp.nutnet.w.na.omit[,c("Height", "Seed mass", "LDMC",  "SLA", "LA", "Leaf C", "Leaf N", "Leaf P")], scale=T, graph=F)
summary(pca.r)## 
# visualize 
fviz_pca_var(pca.r, label = "var", alpha.ind =0.5, pointsize=2, title = NULL)+theme_bw(base_size=15)
(pp2<-fviz_pca_biplot(pca.r, label = "var", alpha.ind =0.5, pointsize=2, title = paste0("PCA based on ", nrow(sp.nutnet.w.na.omit), " species having all 8 traits (in total ", length(all.spp), " species occurred)"))+theme_bw(base_size=18))
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
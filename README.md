# Functional traits compilied from global databases for NutNet species 
compiling trait data from TRY (version 6), AusTraits(2.1.2), BIEN(version 1.2.6)

# Trait data as well as the output files can be found in [https://www.dropbox.com/scl/fo/sj6kwbrfppdsiqx2mzk75/h?dl=0&rlkey=f6idfy0nvevnuv6yy3sgw9a4i]

# this R file generates 2 important output files 
# file 1 "combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv", 
 this file includes all species from these trait databases (one species has mulitple trait values), geolocation information is included 

# file 2, "species and genus level traits from TRY, Aus, BIEN, and NutNet for NutNet species.csv"
 all traits (Height,LDMC, SLA, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass) are averaged for each species
 regardless of their geolocation. Missing traits are filled using the average trait value from its genus level, which is based on other 
 species in the same genus with trait values. I first average traits for each species, then use the average  of trait value from the genus so that filled trait values do not biased towards species with a higher
 number of records.

LDMC: leaf dry matter content; SLA:specific leaf area; LA:leaf area; Leaf C, Leaf N, Leaf P: leaf carbon, nitrogen, phosphorus




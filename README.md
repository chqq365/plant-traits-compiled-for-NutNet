# Functional traits compilied from global databases for NutNet species 

# all data needed can be downloanded from here (as well as output files) https://drive.google.com/drive/folders/1w-I_P7i_RrtchmRAgZ3E0orZHcJfOQ8f?usp=sharing


# this R file generate 3 important output files 
# file 1 "combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv", 
# this file include all species from these three trait databases, geolocation information is inlcuded 

# file 2, "species and genus level traits from TRY, Aus, and BIEN for NutNet species.csv"
# all traits (Height, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass, SLA) are averaged for each species
# regardless of their geolocation, 
# Missing traits are filled using the average trait value from its genus level, which is based on other 
# species in the same genus with trait values. I first average traits for each species, then use the average # of trait value from the genus so that filled trait values do not biased towards species with a higher
# number of records.

# file 3, "functional dispersion and community weighted mean at plot level.csv"
# functional dispersion and community weighted mean are calculated for all subplots for all years  
# (year_trt>0)

# importantly, authorships are required from TRY (7 researchers)
# around 30% of the unique species would be deleted if we do not include those datasets 






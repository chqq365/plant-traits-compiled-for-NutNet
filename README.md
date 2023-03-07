# Functional traits compilied from global databases for NutNet species 

# all data needed can be downloanded from here (as well as output files) https://drive.google.com/drive/folders/1w-I_P7i_RrtchmRAgZ3E0orZHcJfOQ8f?usp=sharing


# this R file generates 2 important output files 
# file 1 "combining traits at individual level from TRY, BIEN, Aus, and NutNet.csv", 
 this file includes all species from these trait databases (one species has mulitple trait values), geolocation information is included 

# file 2, "species and genus level traits from TRY, Aus, BIEN, and NutNet for NutNet species.csv"
 all traits (Height, LA, Leaf C, Leaf N, Leaf P,  Seed dry mass, SLA) are averaged for each species
 regardless of their geolocation. Missing traits are filled using the average trait value from its genus level, which is based on other 
 species in the same genus with trait values. I first average traits for each species, then use the average  of trait value from the genus so that filled trait values do not biased towards species with a higher
 number of records.






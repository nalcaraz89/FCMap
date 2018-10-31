#Big Bend Priority Mapping Analysis
#Nick Alcaraz and Luke McEachron
#FL Fish and Wildlife Research Institute
#nick.alcaraz@myfwc.com   luke.mceachron@myfwc.com

require(rgdal)
require(dplyr)
require(plyr)

# The input file geodatabase
fgdb <- "G:/Groups/GIS/FCMaP/Final_Priority_Mapping_Data/Final_Merged_Datasets/Big_Bend_All_Respondents.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="All_Priorities_Coded")

# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)

#Change Justifications and Products from integers to factors
fc$Justification1 <- as.factor(fc$Justification1)
fc$Justification2 <- as.factor(fc$Justification2)
fc$Justification3 <- as.factor(fc$Justification3)
fc$Products1 <- as.factor(fc$Products1)
fc$Products2 <- as.factor(fc$Products2)
fc$Products3 <- as.factor(fc$Products3)

#Recode Values
fc$Justification1 <- revalue(fc$Justification1, c("-1"="None", "1"="General knowledge gap", "2"="Habitat mapping and coastal geomorphology", "3"="Resource management (sediment, minerals, coastal restoration and resilience)", "4"="Fishing and fisheries (commercial/recreational fishing)", "5"="Recreation (diving, sailing, non-fishing activities)", "6"="Navigation/safety/marine infrastructure", "7"="Scientific research and education (biological, geological)", "8"="Cultural/historical resources (shipwrecks, debris fields)"))
fc$Justification2 <- revalue(fc$Justification2, c("-1"="None", "1"="General knowledge gap", "2"="Habitat mapping and coastal geomorphology", "3"="Resource management (sediment, minerals, coastal restoration and resilience)", "4"="Fishing and fisheries (commercial/recreational fishing)", "5"="Recreation (diving, sailing, non-fishing activities)", "6"="Navigation/safety/marine infrastructure", "7"="Scientific research and education (biological, geological)", "8"="Cultural/historical resources (shipwrecks, debris fields)"))
fc$Justification3 <- revalue(fc$Justification3, c("-1"="None", "1"="General knowledge gap", "2"="Habitat mapping and coastal geomorphology", "3"="Resource management (sediment, minerals, coastal restoration and resilience)", "4"="Fishing and fisheries (commercial/recreational fishing)", "5"="Recreation (diving, sailing, non-fishing activities)", "6"="Navigation/safety/marine infrastructure", "7"="Scientific research and education (biological, geological)", "8"="Cultural/historical resources (shipwrecks, debris fields)"))
fc$Products1 <- revalue(fc$Products1, c("-1"="None", "1"="Bottom type - hardness/smoothness - Side-scan sonar", "2"="Bottom type - hardness/smoothness - Multibeam backscatter", "3"="Sub-bottom geology from a profiler", "4"= "Ferrous objects from a magnetometer", "5"="Ground data such as imagery/grabs or in situ spectrometry", "6"="Seafloor color from remotely collected imaging sensor"))
fc$Products2 <- revalue(fc$Products2, c("-1"="None", "1"="Bottom type - hardness/smoothness - Side-scan sonar", "2"="Bottom type - hardness/smoothness - Multibeam backscatter", "3"="Sub-bottom geology from a profiler", "4"= "Ferrous objects from a magnetometer", "5"="Ground data such as imagery/grabs or in situ spectrometry", "6"="Seafloor color from remotely collected imaging sensor"))
fc$Products3 <- revalue(fc$Products3, c("-1"="None", "1"="Bottom type - hardness/smoothness - Side-scan sonar", "2"="Bottom type - hardness/smoothness - Multibeam backscatter", "3"="Sub-bottom geology from a profiler", "4"= "Ferrous objects from a magnetometer", "5"="Ground data such as imagery/grabs or in situ spectrometry", "6"="Seafloor color from remotely collected imaging sensor"))

##Summary Statistics Section##

#Number of Coins Used - All Respondents
coin_total <- sum(fc$Coins)
print(coin_total)

potential_coins <- (nlevels(fc$Respondents)) * round((nlevels(fc$USNG)*0.2))
print(potential_coins)

#Justification 1 - All Respondents
j1_list <- fc$Justification1[fc$Coins>=1]
j1_total <- as.data.frame(table(j1_list))
barplot(j1_total$Freq)
barplot(j1_total$Freq[!j1_total$j1_list=="None"])

#Justification 2 - All Respondents
j2_list <- fc$Justification2[fc$Coins>=1]
j2_total <- as.data.frame(table(j2_list))
barplot(j2_total$Freq)
barplot(j2_total$Freq[!j2_total$j2_list=="None"])

#Justification 3 - All Respondents
j3_list <- fc$Justification3[fc$Coins>=1]
j3_total <- as.data.frame(table(j3_list))
barplot(j3_total$Freq)
barplot(j3_total$Freq[!j3_total$j3_list=="None"])

#Product 1 - All Respondents
p1_list <- fc$Products1[fc$Coins>=1]
p1_total <- as.data.frame(table(p1_list))
barplot(p1_total$Freq)
barplot(p1_total$Freq[!p1_total$p1_list=="None"])

#Product 2 - All Respondents
p2_list <- fc$Products2[fc$Coins>=1]
p2_total <- as.data.frame(table(p2_list))
barplot(p2_total$Freq)
barplot(p2_total$Freq[!p2_total$p2_list=="None"])

#Product 3 - All Respondents
p3_list <- fc$Products3[fc$Coins>=1]
p3_total <- as.data.frame(table(p3_list))
barplot(p3_total$Freq)
barplot(p3_total$Freq[!p3_total$p3_list==-"None"])

#USNG Total Coins - All Respondents
usng_coin_totalsum <- aggregate(fc$Coins, by=list(USNGNumber=fc$USNG), FUN=sum)
summary(usng_coin_total)
print(usng_coin_total[usng_coin_total$x>=1,])

usng_coin_totalmin <- aggregate(fc$Coins, by=list(USNGNumber=fc$USNG), FUN=min)
print(usng_coin_totalmin)

usng_coin_totalmax <- aggregate(fc$Coins, by=list(USNGNumber=fc$USNG), FUN=max)
print(usng_coin_totalmax)

usng_coin_totalave <- aggregate(fc$Coins, by=list(USNGNumber=fc$USNG), FUN=ave)
print(usng_coin_totalave)

usng_coin_totalsd <- aggregate(fc$Coins, by=list(USNGNumber=fc$USNG), FUN=sd)
print(usng_coin_totalsd)

#USNG Justification Types and Counts - All Respondents
#########May need to review and fix code
usng_j1 <- with(fc, as.data.frame(table(fc$USNG[fc$Coins>=1], fc$Justification1[fc$Coins>=1])))

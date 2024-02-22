# Miniproject Code: Primate Roadkill

# clear environment 
rm(list=ls())

# set working directory 
setwd("C:/Users/User/OneDrive - Imperial College London/Documents/Masters/Modules/Miniproject")

# load required packages
library(dplyr)
library(lme4)
library(lmtest)
library(ggplot2)
library(devtools)
library(glmmADMB)
library(sjPlot)
library(MuMIn)
library(maps)
library(MASS)
library(effects)
library(ggplot2)
library(ggeffects)

# load roadkill data 
primate_data<-read.csv("global_primate_roadkill_database.csv",header=T)

# subset so only columns wanted remain 
primate_data <- dplyr::select(primate_data, ID, Genus, `Genus.species`, `IUCN.status`, Year, Month, Country, Age, Sex)

# remove blanks in age, sex, year and month
primate_data <- primate_data[primate_data$Sex != "", ] 
primate_data <- primate_data[primate_data$Age != "", ] 
primate_data <- primate_data[primate_data$Month != "", ]
primate_data <- primate_data[primate_data$Year != "", ] 
primate_data <- primate_data[primate_data$Country != "", ] 
primate_data <- primate_data[primate_data$IUCN.status != "", ] 

# view first few rows
head(primate_data)

# view structure of data 
str(primate_data)

# check unique entries in sex  
unique(primate_data$Sex)

# remove unknown, 2m 1f and hermaphrodite 
primate_data <- primate_data[!(primate_data$Sex %in% c(" ", "Unknown", "Hermaphrodite?")), ]

# check this worked 
unique(primate_data$Sex)

# check unique entries in age 
unique(primate_data$Age)

# remove unknown
primate_data <- primate_data[!primate_data$Age %in% c("Unknown"), ]

# check this worked 
unique(primate_data$Age)

# change all to lower case 
primate_data$Age <- tolower(primate_data$Age)

# check this worked 
unique(primate_data$Age)

# remove spaces after inputs
primate_data$Age[primate_data$Age == "adult "] <- "adult"

# check this worked
unique(primate_data$Age)

# how many adult and offspring inputs 
total_adult_and_offspring <- sum(primate_data$Age == "adult and infant") + sum(primate_data$Age == "2 adult, 1 juvenile")
total_adult_and_offspring

# only 2, so won't be able to test this versus those not with an infant, so remove 
primate_data <- primate_data[!(primate_data$Age %in% c("adult and infant", "2 adult, 1 juvenile")), ]

# check unique entries in month
unique(primate_data$Month)

# fix january so only one version
primate_data$Month <- gsub("\\s+", "", primate_data$Month)

# one month is april-october - check how many
table(primate_data$Month)

# 3 in total - remove these as not meaningful
primate_data <- primate_data[primate_data$Month != "April-October", ]

# create a bar plot of dead primates by age
ggplot(primate_data, aes(x = Age)) +
  geom_bar(fill = "skyblue", color = "black")

# loading traits data 

# MASS DATA
mass_data <- read.csv('BodyMass_.csv', header=T)

# change species column format and name so it matches roadkill data 
mass_data$Species <- gsub("_", " ", mass_data$Species)
mass_data$Genus.species <- mass_data$Species

# view unique species entries in primate_data to subset mass_data to just contain these species 
unique(primate_data$Genus.species)

# create logical vector to subset by 
species_to_subset <- c("Sapajus nigritus", "Alouatta guariba", "Callithrix penicillata","Papio hamadryas", "Pan troglodytes","Chlorocebus pygerythrus", "Papio ursinus","Trachypithecus phayrei", "Alouatta caraya", "Presbytis femoralis","Chlorocebus pygerythrus", "Macaca maura", "Pan troglodytes","Macaca fascicularis", "Trachypithecus geei", "Trachypithecus obscurus","Cheirogaleus major", "Otolemur crassicaudatus", "Cercopithecus mitis","Loris lydekkerianus", "Loris lydekkerianus lydekkerianus", "Cercopithecus mitis", "Chlorocebus pygerythrus","Colobus angolensis", "Papio cynocephalus","Otolemur garnettii", "Paragalago cocos", "Macaca mulatta")

# subset mass_data by these species         
mass_data <- mass_data[mass_data$Genus.species %in% species_to_subset, ]

# missing data on males and females so will just have to use average for species 

# check if all primate species are present in mass_data
# extract unique species from primate_data
primate_species <- unique(primate_data$Genus.species)

# find species from primate_data not present in mass_data
missing_species <- primate_species[!(primate_species %in% mass_data$Species)]

# print the missing species
print(missing_species)

# all these are subspecies, so will change them to just their species
# fefine a function to remove the third word from a string
remove_third_word <- function(string) {
  words <- strsplit(string, " ")[[1]]
  paste(words[1:2], collapse = " ")
}

# update the missing species in primate_data so those with three words have their third word removed 
primate_data$Genus.species <- ifelse(primate_data$Genus.species %in% missing_species,
                                    sapply(primate_data$Genus.species[primate_data$Genus.species %in% missing_species],
                                           remove_third_word),
                                    primate_data$Genus.species)

# cross check again
# find species from primate_data not present in mass_data
primate_species <- unique(primate_data$Genus.species)
missing_species <- primate_species[!(primate_species %in% mass_data$Species)]

# print the missing species
print(missing_species)

# paragalago cocos missing as it has no mass data so remove it from primate_data
primate_data <- subset(primate_data, Genus.species != "Paragalago cocos")

# none, so now need to cross check the iucn status of the subspecies and species 
# two not matching so change it to that of the species for them
# update IUCN status for Cercopithecus mitis
primate_data$IUCN.status[primate_data$Genus.species == "Cercopithecus mitis"] <- "LC"

# update IUCN status for Loris lydekkerianus
primate_data$IUCN.status[primate_data$Genus.species == "Loris lydekkerianus"] <- "LC"

# check if the updates were applied correctly for Cercopithecus mitis
subset_1 <- primate_data[primate_data$Genus.species == "Cercopithecus mitis", ]
print(subset_1$IUCN)

# check if the updates were applied correctly for Loris lydekkerianus
subset_2 <- primate_data[primate_data$Genus.species == "Loris lydekkerianus", ]
print(subset_2$IUCN)

# now view first few rows of mass_data to prepare for merging
head(mass_data)

# remove unwanted columns
mass_data <- dplyr::select(mass_data, Genus.species, BodyMass_kg)

# this section is not run because otherwise merging gets errors - it had to be run to determine where additional observations were coming from and then removed
# merge primate_data and mass_data by Genus.species
# merged_data <- merge(primate_data, mass_data, by = "Genus.species")

# view the merged dataset
# print(head(merged_data))

# somehow gained observations - find out where
# table(merged_data$Genus.species)
# table(primate_data$Genus.species)

# gained observations in Chlorocebus pygerythrus - remove the problem row 

# looking at this and the mass_data, there is a duplicate in Chlorocebus pygerythrus - rows 164 and 165
print(mass_data[0:10,])

# row 165 lacks data for males and females so better to keep row 164 - remove row 8 
mass_data <- mass_data[-8, ]

#  other problem - Cheirogaleus major
print(mass_data[0:10,])

# row 139 lacks the data for males and females so it is better to keep row 138
# remove row number 139
mass_data <- mass_data[-6, ]

# check again
# table(merged_data$Genus.species)
# table(primate_data$Genus.species)

# do the merge again
# merge primate_data and mass_data by Genus.species
merged_data <- merge(primate_data, mass_data, by = "Genus.species")

# DIEL ACTIVITY
# load data 
diel_data <- read.csv('DielActivity.csv', header=T)

# change species names to match format in primate data
diel_data$Species <- gsub("_", " ", diel_data$Species)
diel_data$Genus.species <- diel_data$Species

# find species from primate_data not present in diel_data
missing_species <- primate_species[!(primate_species %in% diel_data$Species)]

# print the missing species
print(missing_species)

# paragalago cocos still coming up but that is because there is data in diel_data for it so ignore

# now view head data to prepare for merging
head(diel_data)

# remove unwanted columns
diel_data <- dplyr::select(diel_data, Genus.species, DielActivity)

# merge merged_data and diel by Genus.species
merged_data <- merge(merged_data, diel_data, by = "Genus.species")

# view the merged dataset
print(head(merged_data))

# correct amount of observations so maybe problem with duplicate was just in mass data but will check for all anyways

# HABITAT
# load data 
habitat_data <- read.csv('Habitat.csv', header=T)

# change species names to match format in primate data
habitat_data$Species <- gsub("_", " ", habitat_data$Species)
habitat_data$Genus.species <- habitat_data$Species

# now view head data to perpare for merging
head(habitat_data)

# remove unwanted columns
habitat_data <- dplyr::select(habitat_data, Genus.species, Habitat_Forest:Habitat_Desert)

# merge merged_data and habitat by Genus.species
merged_data <- merge(merged_data, habitat_data, by = "Genus.species")

# View the merged dataset
print(head(merged_data))

# HOME RANGE
# load data
home_range_data <- read.csv('HomeRange.csv', header=T)

# change species names to match format in primate data
home_range_data$Species <- gsub("_", " ", home_range_data$Species)
home_range_data$Genus.species <- home_range_data$Species

# subset home range data to include only the species in primate_data
home_range_data <- home_range_data[home_range_data$Genus.species %in% species_to_subset, ]

# subset to remove unwanted columns 
home_range_data <- dplyr::select(home_range_data, Genus.species, HomeRange_ha)

# view which are duplicated
table(home_range_data$Genus.species)

# same as above - had to run separately as otherwise would give errors in merge 
# merged_data <- merge(merged_data, home_range_data, by = "Genus.species")

# gained observations so need to determine where these came from

# identify species with multiple home range values
multiple_values_species <- unique(home_range_data$Genus.species[duplicated(home_range_data$Genus.species)])

# for species with multiple home range values, merge and calculate the average
for (Genus.species in multiple_values_species) {
  # subset the data for the species
  species_data <- home_range_data[home_range_data$Genus.species == Genus.species, "HomeRange_ha"]
  
  # calculate the average
  average_home_range <- mean(as.numeric(species_data))
  
  # update the original dataframe with the average value
  home_range_data <- home_range_data[home_range_data$Genus.species != Genus.species, ]
  home_range_data <- rbind(home_range_data, data.frame(Genus.species = Genus.species, HomeRange_ha = average_home_range))
}

# print updated home_range_data
print(home_range_data)
table(home_range_data$Genus.species)

# merge 
merged_data <- merge(merged_data, home_range_data, by = "Genus.species")


# LOCOMOTION
# load data
locomotion_data <- read.csv('Locomotion.csv', header = TRUE)

# change species names to match format in primate data
locomotion_data$Species <- gsub("_", " ", locomotion_data$Species)
locomotion_data$Genus.species <- locomotion_data$Species

# view species in both
unique(primate_data$Genus.species)
unique(locomotion_data$Genus.species)

# subset locomotion_data to include only the species in primate_data
locomotion_data <- locomotion_data[locomotion_data$Genus.species %in% species_to_subset, ]

# remove unwanted columns
locomotion_data <- dplyr::select(locomotion_data, Genus.species, Locomotion)

# Check if all species in primate_data are present in locomotion_data
all_species_present <- all(primate_data$Genus.species %in% locomotion_data$Genus.species)

if(all_species_present) {
  print("All species in primate_data are present in locomotion_data.")
} else {
  print("Not all species in primate_data are present in locomotion_data.")
}

# fix these now
# L. lydekkerianus
print(locomotion_data[1:20,])

# find the row where species is L. lydekkerianus and locomotion is both
row_index <- which(locomotion_data$Genus.species == "Loris lydekkerianus" & locomotion_data$Locomotion == "BOTH")

# create loop to tell if the row was found, and if so that it was dropped 
if (length(row_index) > 0) {
  locomotion_data <- locomotion_data[-row_index, , drop = FALSE]  # Drop = FALSE ensures that the result remains a data frame
  print("Row removed where locomotion is 'both' for Loris lydekkerianus.")
} else {
  print("No row found where locomotion is 'both' for Loris lydekkerianus.")
}

# M. fasicularis
print(locomotion_data[0:25,])

# it is both aroboreal and terrestrial 
# find the row index where "Macaca fascicularis" has locomotion recorded as "AR"
row_index <- which(locomotion_data$Genus.species == "Macaca fascicularis" & locomotion_data$Locomotion == "AR")

# remove the row if found 
if (length(row_index) > 0) {
  locomotion_data <- locomotion_data[-row_index, , drop = FALSE]  # Drop = FALSE ensures that the result remains a data frame
  print("Row removed")
} else {
  print("No row found")
}

# again not run in code here because causes errors in merged_data 
# merge merged_data and locomotion by Genus.species
# merged_data <- merge(merged_data, locomotion_data, by = "Genus.species")

# gained a few 
# check where
# table(primate_data$Genus.species)
# table(merged_data$Genus.species)

# gained in l. lydekkerianus, m. fasicularis, p. cynocephalus - find problem rows

# p. cynocephalus 
print(locomotion_data[0:24,])

# it is both so fix this 
# find the row index where "Papio cynocephalus" has locomotion recorded as "T"
row_index <- which(locomotion_data$Genus.species == "Papio cynocephalus" & locomotion_data$Locomotion == "T")

# remove the row if found
if (length(row_index) > 0) {
  locomotion_data <- locomotion_data[-row_index, , drop = FALSE]  # Drop = FALSE ensures that the result remains a data frame
  print("Row removed")
} else {
  print("No row found")
}

# find the row index where species is "Papio cynocephalus"
papio_row_index <- which(locomotion_data$Genus.species == "Papio cynocephalus")

# print the row index
print(papio_row_index)

# now change the other row locomotion to both

# find the row index where "Papio cynocephalus" is located
row_index <- which(locomotion_data$Genus.species == "Papio cynocephalus")

# if the row exists, change the locomotion data to "BOTH"
if (length(row_index) > 0) {
  locomotion_data[row_index, "Locomotion"] <- "BOTH"
  print("Changed locomotion data to 'BOTH' for Papio cynocephalus.")
} else {
  print("Papio cynocephalus not found in locomotion_data.")
}

# merge
merged_data <- merge(merged_data, locomotion_data, by = "Genus.species")

# TROPHIC GUILD
# load data
trophic_data <- read.csv('TrophicGuild.csv', header = TRUE)

# change species names to match format in primate data
trophic_data$Species <- gsub("_", " ", trophic_data$Species)
trophic_data$Genus.species <- trophic_data$Species

# view species in both
unique(primate_data$Genus.species)
unique(trophic_data$Genus.species)

# subset locomotion_data to include only the specified species
trophic_data <- trophic_data[trophic_data$Genus.species %in% species_to_subset, ]

# remove unwanted columns
trophic_data <- dplyr::select(trophic_data, Genus.species, TrophicGuild)

# check where extra observations are coming from
table(trophic_data$Genus.species)

# 2 recordings of m. mulatta - fix this 
print(trophic_data[0:24,])

# they are folivore_frugivore so remove row with just frugivore 
# find the row index
row_index <- which(trophic_data$Genus.species == "Macaca mulatta" & trophic_data$TrophicGuild == "Frugivore")

# remove the row if found
if (length(row_index) > 0) {
  trophic_data <- trophic_data[-row_index, , drop = FALSE]  # Drop = FALSE ensures that the result remains a data frame
  print("Row removed")
} else {
  print("No row found")
}

# now o. garnetti
print(trophic_data[0:24,])

# remove row 8
trophic_data <- trophic_data[-13, , drop = FALSE]

# merge 
merged_data <- merge(merged_data, trophic_data, by = "Genus.species")

# Check for missing values in the entire dataset
missing_values <- is.na(merged_data)

# summarize the number of missing values in each column
missing_counts <- colSums(missing_values)

# print the counts of missing values
print(missing_counts)

# unique to look for any mistakes or outliers 
unique(merged_data$IUCN.status)
unique(merged_data$Year)
unique(merged_data$Month)
unique(merged_data$Country)
unique(merged_data$Age)
unique(merged_data$Sex)
unique(merged_data$BodyMass_kg)
unique(merged_data$DielActivity)
unique(merged_data$HomeRange_ha) # a few seem v big - check them - all correct 
unique(merged_data$Locomotion)
unique(merged_data$Habitat_Forest)
unique(merged_data$Habitat_Savanna)
unique(merged_data$Habitat_Shrubland)
unique(merged_data$Habitat_Grassland)
unique(merged_data$Habitat_Wetlands)
unique(merged_data$Habitat_Rocky.areas)
unique(merged_data$Habitat_Desert)
unique(merged_data$TrophicGuild)

# all are ok - need to decide about habitat ones because not sure if will keep them all 
table(merged_data$Habitat_Desert) 
table(merged_data$Habitat_Rocky.areas)
table(merged_data$Habitat_Grassland)
table(merged_data$Habitat_Savanna) 
table(merged_data$Habitat_Forest) 
table(merged_data$Habitat_Wetlands) 
table(merged_data$Habitat_Shrubland)

# all except savanna are dominated by either 0 or 1

# check str
str(merged_data)

# change variable types 
merged_data$IUCN.status <- as.factor(merged_data$IUCN.status)
merged_data$Age <- as.factor(merged_data$Age)
merged_data$Sex <- as.factor(merged_data$Sex)
merged_data$DielActivity <- as.factor(merged_data$DielActivity)
merged_data$Locomotion <- as.factor(merged_data$Locomotion)
merged_data$TrophicGuild <- as.factor(merged_data$TrophicGuild)
merged_data$Habitat_Forest <- as.logical(merged_data$Habitat_Forest)
merged_data$Habitat_Savanna <- as.logical(merged_data$Habitat_Savanna)
merged_data$Habitat_Shrubland <- as.logical(merged_data$Habitat_Shrubland)
merged_data$Habitat_Grassland <- as.logical(merged_data$Habitat_Grassland)
merged_data$Habitat_Wetlands<- as.logical(merged_data$Habitat_Wetlands)
merged_data$Habitat_Rocky.areas <- as.logical(merged_data$Habitat_Rocky.areas)
merged_data$Habitat_Desert <- as.logical(merged_data$Habitat_Desert) # ALL - 1 true 0 false

# check
str(merged_data)

# create a bar plots of primate death counts (y) and different variables
ggplot(merged_data, aes(x = Genus.species)) + 
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = Age)) +
  geom_bar(fill = "skyblue", color = "black") 

ggplot(merged_data, aes(x = BodyMass_kg)) +
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = DielActivity)) +
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = Habitat_Savanna)) +
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = Habitat_Shrubland))+
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = HomeRange_ha)) +
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = Locomotion)) +
  geom_bar(fill = "skyblue", color = "black")

ggplot(merged_data, aes(x = TrophicGuild)) +
  geom_bar(fill = "skyblue", color = "black") 

# due to lack of data available for habitat, remove some
merged_data <- subset(merged_data, select=-c(Habitat_Grassland, Habitat_Wetlands, Habitat_Desert, Habitat_Rocky.areas))

# group merged_data by species and month to create a count, which is the response variable 
# include means of numerical variables - these will remain the same as in merged_data - because taking the mean e.g. 4 recordings of the same number
# because body mass and home range are averages for the species (not taking age into consideration)
response_data <- merged_data %>%
  group_by(Genus.species, Age) %>%
  summarise(Count = n(),
            Mean_BodyMass = mean(BodyMass_kg, na.rm = TRUE),  
            Mean_HomeRange = mean(HomeRange_ha, na.rm = TRUE))

# merge with the datasets again - am sure there is smoother way to do this than remerging but this works 
response_data<- merge(response_data, diel_data, by="Genus.species")
response_data<- merge(response_data, locomotion_data, by="Genus.species")
response_data<- merge(response_data, home_range_data, by="Genus.species")
response_data<- merge(response_data, trophic_data, by="Genus.species")
response_data<- merge(response_data, habitat_data, by="Genus.species")

# remove data on habitats other than savanna 
response_data <- subset(response_data, select=-c(Habitat_Grassland, Habitat_Wetlands, Habitat_Desert, Habitat_Rocky.areas, Habitat_Forest, Habitat_Shrubland))

# view first few rows 
head(response_data)

# view structure 
str(response_data)

# fix variable types 
response_data$Genus.species <- as.factor(response_data$Genus.species)
response_data$DielActivity <- as.factor(response_data$DielActivity)
response_data$Locomotion <- as.factor(response_data$Locomotion)
response_data$Habitat_Savanna <- as.logical(response_data$Habitat_Savanna)
response_data$TrophicGuild <- as.factor(response_data$TrophicGuild)

# view ranges of numerical variables 
range(response_data$Mean_BodyMass)
range(response_data$Mean_HomeRange)

# reorder data so it goes from infant to adult - ease of plotting later
response_data$Age <- factor(response_data$Age, levels = c("infant", "juvenile", "subadult", "adult"))

# Check the new factor level names
print(levels(response_data$Age))

# reorder the data frame based on the 'Age' variable
response_data <- response_data[order(response_data$Age), ]

# capitalise them for plotting later
levels(response_data$Age)[levels(response_data$Age) == "infant"] <- "Infant"
levels(response_data$Age)[levels(response_data$Age) == "juvenile"] <- "Juvenile"
levels(response_data$Age)[levels(response_data$Age) == "subadult"] <- "Subadult"
levels(response_data$Age)[levels(response_data$Age) == "adult"] <- "Adult"

# construct generalised linear mixed models with species as a random effect and other variables as fixed effects 
# family = "poisson" for count data

glmer1 <- glmer(Count ~  Age + scale(Mean_BodyMass) + scale(Mean_HomeRange) + Locomotion + TrophicGuild + DielActivity + Habitat_Savanna +(1|Genus.species), 
              family="poisson",
              data = response_data)

# view output 
summary(glmer1)

# remove diel activity - highest p value
glmer2 <- glmer(Count ~  Age + scale(Mean_BodyMass) + scale(Mean_HomeRange) + Locomotion + TrophicGuild + Habitat_Savanna + (1|Genus.species), 
                family="poisson",
                data = response_data)

# view output 
summary(glmer2)

# likelihood ratio test
lrtest(glmer1,glmer2)

# p > 0.05 so simpler model better - glmer2

# remove body mass - highest p value 
glmer3 <- glmer(Count ~  Age  + scale(Mean_HomeRange) + Locomotion + TrophicGuild + Habitat_Savanna + (1|Genus.species), 
                family="poisson",
                data = response_data)

# view output 
summary(glmer3)

# lrtest
lrtest(glmer2,glmer3)

# p > 0.05 so simpler model better - glmer3

# remove savanna - highest p value 
glmer4 <- glmer(Count ~  Age  + scale(Mean_HomeRange) + Locomotion + TrophicGuild + (1|Genus.species), 
                family="poisson",
                data = response_data)

# view output 
summary(glmer4)

# lrtest
lrtest(glmer3,glmer4)

# p > 0.05, glm4 better
# remove locomotion - highest p value 
glmer5 <- glmer(Count ~  Age  + scale(Mean_HomeRange) + TrophicGuild + (1|Genus.species), 
                family="poisson",
                data = response_data)

# view output 
summary(glmer5)

# lrtest
lrtest(glmer4,glmer5)

# p > 0.05, glmer5 better 

# remove trophic guild - highest p value
glmer6 <- glmer(Count ~  Age  + scale(Mean_HomeRange) + (1|Genus.species), 
                family="poisson",
                data = response_data)

# view output 
summary(glmer6)

# lrtest
lrtest(glmer5,glmer6)

# glmer6 better 
# remove home range 
glmer7 <- glmer(Count ~  Age + (1|Genus.species), 
                family="poisson",
                data = response_data,
                control = glmerControl(optimizer = "bobyqa"))

# view output
summary(glmer7)

# lrtest
lrtest(glmer6,glmer7)

# glmer7 better - final model 

# test for overdispersion
E1 <- resid(glmer7, type="pearson")
N <- nrow(response_data)
p <- length(fixef(glmer7)+1)
overdispersion <-sum(E1^2)/(N-p)
overdispersion

# underdispersed

# glmer7 plotting assumptions
F1 <- fitted(glmer7)
par(mfrow=c(2,2), mar = c(5,5,2,2))
plot(x=F1, y = E1, xlab="Fitted values", ylab="Pearson residuals") # observations clustered around fitted values 1 
abline(h=0,lty=2)
plot(x=response_data$Age,
     y=E1,
     xlab="Age",
     ylab="Pearson residuals")
boxplot(E1~Genus.species,
        data=response_data,
        xlab="Species",
        ylab="Pearson residuals")
abline(h=0,lty=2)

# null model 
glmer7null <- glmer(Count ~  1+(1|Genus.species), 
                family="poisson",
                data = response_data,
                control = glmerControl(optimizer = "bobyqa"))

summary(glmer7null)

# maximal model
summary(glmer1)

# plot the fixed effects (marginal effects)
dev.off()
plot_model(glmer7, type = "eff", terms = c("Age"))

# plot the model summary
plot_model(glmer7, type = "std")

# model minus genus 
glmer9 <- glm(Count ~ Age , family="poisson",  data = response_data)

summary(glmer9)
summary(glmer7)

# Likelihood Ratio Test
lr_test <- anova(glmer7, glmer9, test = "LRT")
print(lr_test)

AIC(glmer7)
AIC(glmer9)

# calculate variance explained by random effect species 
# extract variance of random intercept
var_random_intercept <- 2.954

# calculate residual variance
residual_variance <- 241.8 / 38

# calculate ICC
ICC <- var_random_intercept / (var_random_intercept + residual_variance)

# print ICC
ICC

# print r squared values
conditional_r_squared <- r.squaredGLMM(glmer7)
print(conditional_r_squared)

# plotting
# create prediction object
effect_glmer7 <- effect("Age", glmer7)

# plot the predictions
dev.off()
plot1 <- plot(effect_glmer7, type = "response", ylim = c(0, 12), main="", ylab="Predicted Roadkill Count", xlab="Age Group")
plot1

# create prediction object
effect_glmer7b <- ggpredict(glmer7, terms = "Age")

# plot the predictions
plot2 <- ggplot(effect_glmer7b, aes(x = x, y = predicted)) +
  geom_point(aes(y = predicted), size = 2) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2,
    position = position_dodge(0.2)
  ) +
  geom_line(aes(group = group), size=0.5,color = "red") +
  
  labs(
    x = "Age Group",
    y = "Predicted Roadkill Count"
  ) +
  theme_minimal()+
  coord_cartesian(ylim = c(0, 11))+
  theme(
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),  # Adjust the margin
    axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))   # Adjust the margin
  )

print(plot2) # this is best plot 

# create map of roadkill observations
# list of countries to be colored
target_countries <- c(
  "Argentina", "Paraguay", "Brazil",
  "Kenya", "Uganda", "South Africa",
  "Democratic Republic of the Congo", "Madagascar",
  "India", "Malaysia", "Indonesia", "Sri Lanka",
  "Saudi Arabia", "Singapore", "India",
  "Bangladesh"
)

# load world map data
world_map <- map_data("world")

# filter map data for the target countries 
target_map_data <- subset(world_map, tolower(region) %in% tolower(target_countries))

# create the map
ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  geom_polygon(data = target_map_data, aes(x = long, y = lat, group = group), fill = "red", color = "white") +
  theme_void()

# view r version
R.Version()
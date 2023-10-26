
library(magrittr)
library(uuid)
library(tidyverse)
library(readxl)
library(mdJSONdictio)

# Validate entry file
setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2023/Eielson/data")
entry.file<- read.csv("2023_Eielson_Resights.csv")

setwd(
  "C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Archived_Projects/Eielson/metadata/data_dictionaries"
)
dict.file <-
  read_excel(list.files(pattern = "Resight_Dictionary"))

input.dict <- dict.file

input.data <- entry.file

all.warnings <-
  validate.table(input.dict,
                 input.data)


# Merge data entry sheet with full

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2023/Eielson/data")
entry<-read.csv("2023_Eielson_Resights.csv")

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Archived_Projects/Eielson/data/final_data")
full<-read.csv("Eielson_Resight.csv")

## Confirm column names in entry

colnames<-paste0(colnames(full))

colnames(entry)[!colnames(entry) %in% colnames(full)]

### Consider adding missing columns (may need to fill these in later)

colnames(full)[!colnames(full) %in% colnames(entry)]


# entry<- entry %>%
#   plyr::rename(replace = c("BandNumber" = "BandNumberOut",
#                      "UL" = "ULOut",
#                      "LL" = "LLOut",
#                      "UR" = "UROut",
#                      "LR" = "LROut",
#                      "FlagColor" = "FlagColorOut",
#                      "FlagCode" = "FlagCodeOut",
#                      "Sex" = "FieldSex",
#                      "FeatherType" = "FeatherType1",
#                      "TagType" =  "TagType1",
#                      "TagID" = "TagID1",
#                      "TagAttachment" = "TagAttachment1",
#                      "TagStatus" = "TagStatus1"))
# 
# colnames(entry)[!colnames(entry) %in% colnames(full)]
# 
# entry$Filename<- "Anchorage_Bands_2023.csv"

## Combine tables
new<- entry %>%
  merge(full, all = TRUE)

## Reorder columns
new <- new[, colnames]

## Add UUID

# new<- cbind(OccurenceID = NA, full)

for(a in 1:nrow(new)) {
  if (is.na(new$OccurenceID[a]) == TRUE || new$OccurenceID[a] == ""){
    new$OccurenceID[a] = UUIDgenerate(use.time = FALSE)
  }
}


# Validate new file

setwd(
  "C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Archived_Projects/Eielson/metadata/data_dictionaries"
)
dict.file <-
  read_excel(list.files(pattern = "Resight_Dictionary"))

input.dict <- dict.file

input.data <- new

all.warnings <-
  validate.table(input.dict,
                 input.data)


## Export new sheet with new version number

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Archived_Projects/Eielson/data/final_data")

write.csv(new,"Eielson_Resight_v2.csv",na="",row.names = FALSE)



# # Add locations to ALMS survey
# 
# setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_prep/ALMS/GPS")
# locs<-read.csv("ALMS_points_reference_updated.csv")
# 
# setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2023/NWR_ALMS/data")
# survey<-read.csv("ALMS_surveyed_points_locs_2023.csv")
# 
# 
# for(a in 1:nrow(locs)) {
#   for (b in 1:nrow(survey)) {
#     if (survey$Identity[b] == locs$LocID[a] &
#         is.na(survey$Latitude_dec[b]) == TRUE) {
#       survey$Latitude_dec[b] = locs$Latitude[a]
#     }
#     if (survey$Identity[b] == locs$LocID[a] &
#         is.na(survey$Longitude_dec[b]) == TRUE) {
#       survey$Longitude_dec[b] = locs$Longitude[a]
#     }
#   }
# }
# 
# 
# ## Export new sheet
# setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2023/NWR_ALMS/data")
# write.csv(survey,"ALMS_surveyed_points_locs_2023_updated.csv",na="",row.names = FALSE)


# Transcribe banding data to biosamples inventory

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Reformatted_data/Biological_Samples")
biosamples<-read.csv("Biosamples_v16.csv")

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2022/Migratory_Tracking/data/")
banding<-read.csv("Migratory_Tracking_Banding_2022.csv")

biosamples[biosamples == ""] <- NA
banding[banding == ""] <- NA

banding$BandNumberOut<- as.character(banding$BandNumberOut)

## remove banding data already inventoried
banding<- banding %>%
  drop_na(BandNumberOut, Year)

biosamples<- biosamples %>%
  drop_na(BandNumber, YearCollected)

banding$Delete<- NA

for(a in 1:nrow(banding)) {
  for (b in 1:nrow(biosamples)) {
    if (banding$BandNumberOut[a] == biosamples$BandNumber[b] &
        banding$Year[a] == biosamples$YearCollected[b]) {
      banding$Delete[a] <- 1
    }
  }
}

banding <- banding %>%
  filter(is.na(Delete))

# banding <- banding %>%
#   filter(Year == c(2022),
#          !City == c("Nome"))


setdiff(colnames(biosamples),colnames(banding))

banding<- banding %>%
  plyr::rename(replace = c("FeatherType" = "FeatherType1",
                     "BandNumberOut" = "BandNumber",
                     "FlagCodeOut" = "FlagCode",
                     "Year" = "YearCollected",
                     "Month" = "MonthCollected",
                     "Day" = "DayCollected"))

setdiff(colnames(biosamples),colnames(banding))

sampcol <- c(
  "Buccal",
  "AISwabO",
  "AISwabC",
  "Fecal",
  "Nails",
  "BloodFilter",
  "BloodSmear",
  "BloodGenetics",
  "BloodIsotope",
  "BloodHg",
  "Feather",
  "Downy"
)

sampcol[sampcol %in% colnames(banding)]

banding<- banding %>% pivot_longer(cols = sampcol[sampcol %in% colnames(banding)], names_to = "SampleType", values_to = "SampleTaken") 

unique(banding$SampleTaken)

banding<- banding %>%
  filter(SampleTaken== "Y")

banding$SampleTissue = NA
banding$SampleForm = NA
banding$SamplePurpose = NA
banding$SampleBuffer = NA
banding$StorageMedium = NA

if(!"FeatherType1" %in% colnames(banding)){
  banding$FeatherType1 = NA
}

for (a in 1:nrow(banding)) {
  if (banding$SampleType[a] == "Nails") {
    banding$SampleTissue[a] = "claw"
    banding$SampleForm[a] = NA
    banding$SamplePurpose[a] = NA
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "coin envelope"
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "Feather") {
    banding$SampleTissue[a] = "feather"
    banding$SampleForm[a] = NA
    banding$SamplePurpose[a] = NA
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "coin envelope"
  }
  if (banding$SampleType[a] == "BloodFilter") {
    banding$SampleTissue[a] = "blood"
    banding$SampleForm[a] = "filter"
    banding$SamplePurpose[a] = NA
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "coin envelope"
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "BloodGenetics") {
    banding$SampleTissue[a] = "blood"
    banding$SampleForm[a] = "capillary tube"
    banding$SamplePurpose[a] = "DNA"
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "vacuette tube"
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "BloodHg") {
    banding$SampleTissue[a] = "blood"
    banding$SampleForm[a] = "heparinized capillary tube"
    banding$SamplePurpose[a] = "Hg contamination"
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "vacuette tube"
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "Downy") {
    banding$SampleTissue[a] = "feather"
    banding$SampleForm[a] = NA
    banding$SamplePurpose[a] = NA
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = "coin envelope"
    banding$FeatherType1[a] = "down"
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "Fecal") {
    banding$SampleTissue[a] = "feces"
    banding$SampleForm[a] = NA
    banding$SamplePurpose[a] = NA
    banding$SampleBuffer[a] = "ETOH"
    banding$StorageMedium[a] = "vial"
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "AISwabO") {
    banding$SampleTissue[a] = "buccal"
    banding$SampleForm[a] = "swab"
    banding$SamplePurpose[a] = "AI"
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = NA
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
  if (banding$SampleType[a] == "AISwabC") {
    banding$SampleTissue[a] = "cloacal"
    banding$SampleForm[a] = "swab"
    banding$SamplePurpose[a] = "AI"
    banding$SampleBuffer[a] = NA
    banding$StorageMedium[a] = NA
    banding$FeatherType1[a] = NA
    banding$FeatherType2[a] = NA
  }
}


# "Buccal"
# "BloodSmear"
# "BloodIsotope"

biosamples_add <-
  subset(banding, select = names(banding)[(names(banding) %in% colnames(biosamples))])

biosamples_add$Country<- "USA"
biosamples_add$StateProvince<- "Washington"
biosamples_add$StorageMethod<- "frozen"
biosamples_add$StorageLocation<- "Alaska MBM"
biosamples_add$SampleStatus<- NA
biosamples_add$Collector<- NA
biosamples_add$Contact<- NA
biosamples_add$Notes<- NA


new<- biosamples_add %>%
  merge(biosamples, all = TRUE)

new <- new[, colnames(biosamples)]


for(a in 1:nrow(new)) {
  if (is.na(new$OccurenceID[a]) == TRUE || new$OccurenceID[a] == ""){
    new$OccurenceID[a] = UUIDgenerate(use.time = FALSE)
  }
}

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Reformatted_data/Biological_Samples")
write.csv(new,"Biosamples_v17.csv",na="",row.names = FALSE)

# # Add test status
# setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Reformatted_data/Banding")
# banding<-read.csv("Banding_all_v17.csv")
# 
# setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Reformatted_data/Biological_Samples")
# biosamples<-read.csv("Biosamples_v9.csv")
# 
# 
# for(a in 1:nrow(biosamples)){
#   for(b in 1:nrow(banding)){
#     if(biosamples$BandNumber[a]==banding$BandNumberOut[b] & banding$CHDSex[b]!="" & biosamples$SampleForm[a]=="filter"){
#       biosamples$SampleStatus[a] = "analysis complete"
#     }
#   }
# }
# 
# write.csv(biosamples,"biosamples_v10.csv",na="",row.names = FALSE)


# Summarize samples
setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Fieldwork_wrap/2023/NWR_ALMS/data")
AOU_codes <- read.csv("IBP-AOS-LIST23.csv")

setwd("C:/Users/hvincelette/OneDrive - DOI/Documents/Data_management/landbirds_project_maintenance/Reformatted_data/Biological_Samples")
biosamples<-read.csv("Biosamples_v12.csv")

biosamples_sum_yr <- biosamples %>%
  dplyr::filter(
    SampleForm == "filter",
    !SampleStatus %in% c("analysis complete", "analysis in progress"),
    !StorageLocation %in% c("USGS Alaska Science Center","ADFG"),
    # YearCollected %in% c(2021, 2022, 2023),
    SpeciesCode %in% c("GRYE", "REKN", "LEYE", "SBDO", "SOSA", "SPSA", "UPSA")
  ) %>%
  group_by(SpeciesCode, SampleTissue, SampleForm, YearCollected) %>%
  summarise(NumSamples = n())

for (i in 1:nrow(biosamples_sum_yr)) {
  for (l in 1:nrow(AOU_codes)) {
    if (biosamples_sum_yr$SpeciesCode[i] == AOU_codes$SPEC[l]) {
      biosamples_sum_yr$SpeciesCode[i] = AOU_codes$COMMONNAME[l]
    }
  }
}

biosamples_sum_yr<- dplyr::rename(biosamples_sum_yr, "Species" = SpeciesCode)

biosamples_sum_all <- biosamples_sum_yr %>%
  group_by(Species, SampleTissue, SampleForm) %>%
  summarise(count = sum(NumSamples))



write.csv(biosamples_sum_yr,"biosamples_sum_yr.csv",na="",row.names = FALSE)



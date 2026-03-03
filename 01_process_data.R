options(scipen=999)
#### SETUPS ####

# List all setup files
tmp = list.files(path = "..", pattern = "setup.R", recursive = T, full.names = T)
tmp <- tmp[grepl("TimeMotion", tmp)]

# Source code
source(file = tmp)

library(xlsx)

# -------------------------------------------------------------------
# Read in data 
# -------------------------------------------------------------------

# declare file name and directory
file_name <- "TimeCaT export _02-26-2026_15 observations.xlsx"
dir_data_in <- "C:/Users/stefanie.meliss/OneDrive - Ambition Institute/research_projects/2025_Q3_Lift_TimeandMotion/Data/Time and motion all schools data"

# OBSERVATION INDEX #

# read in observation index data
obs_index <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 1)

# only use real observations
obs_index <- obs_index[obs_index$Type == "real", ]
gc()

# change column names
names(obs_index)[names(obs_index) == "NA."] <- "Pseudonym"
names(obs_index)[names(obs_index) == "NA..1"] <- "Respondent"

# save TimeCaT ids
obs_ids <- obs_index$Obs_Id

# save pseudonyms
pseudo <- obs_index$Pseudonym

# INDIVIDUAL DATA #

# read in observation index data
indiv <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 2)

# only use real observations
indiv <- indiv[indiv$Obs_Id %in% obs_ids, ]
gc()

# INTERACTIONS DATA #

# read in observation index data
inter <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 3)

# only use real observations
inter <- inter[inter$Obs_Id %in% obs_ids, ]

# LOCATION DATA #

# read in observation index data
loc <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 4)

# only use real observations
loc <- loc[loc$Obs_Id %in% obs_ids, ]
gc()

# TIME ESTIMATE DATA #

# read in observation index data
est <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 5)

# only use real observations
est <- est[est$Pseudonym %in% pseudo, ]
gc()

# EVENING WORK ESTIMATE DATA #

# read in observation index data
eve <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 6)

# only use real observations
eve <- eve[eve$Pseudonym %in% pseudo, ]
gc()

# SCHOOL DAY TIMES #

# read in observation index data
sch <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 7)

# only use real observations
sch <- sch[sch$Pseudonym %in% pseudo, ]
gc()


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
file_name <- "TimeCaT_03-09-2026_all 18 teachers.xlsx"
file_name <- "TimeCaT_06-08-2026 all teachers.xlsx"
dir_data_in <- "C:/Users/stefanie.meliss/OneDrive - Ambition Institute/research_projects/2025_Q3_Lift_TimeandMotion/Data/Time and motion all schools data"

# OBSERVATION INDEX #

# read in observation index data
orig_obs <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 1)

# only use real observations
orig_obs <- orig_obs[orig_obs$Type == "real", ]
gc()

# save TimeCaT ids
obs_ids <- orig_obs$Obs_Id

# save pseudonyms
pseudo <- orig_obs$Pseudonym

# -------------------------------------------------------------------
# determine variable levels according to the taxonomy used 
# -------------------------------------------------------------------

# read in data
lookup_levels <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 5)
gc()

# tidy info o levels
lookup_levels$Current <- NULL
lookup_levels$Old <- NULL
lookup_levels <- lookup_levels[!is.na(lookup_levels$Name), ]
lookup_levels$Name <- sub("[[:space:]]+$", "", lookup_levels$Name)
lookup_levels$Label <- sub("[[:space:]]+$", "", lookup_levels$Label)

# -------------------------------------------------------------------
# Read in data 
# -------------------------------------------------------------------

# INDIVIDUAL DATA #

# read in observation index data
orig_indiv <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 2)

# only use real observations
orig_indiv <- orig_indiv[orig_indiv$Obs_Id %in% obs_ids, ]
gc()

# fix namings
orig_indiv <- left_join(orig_indiv %>%
                     mutate(Name = sub("[[:space:]]+$", "", Name)), lookup_levels)
lvl_act <- unique(orig_indiv$Label)
lvl_act <- stringr::str_sort(c(lvl_act, "D6. Self-study and professional development"), numeric = TRUE)
lvl_act <- lvl_act[lvl_act != "None"]
# lvl_act <- gsub("assessment ", "", lvl_act)
lvl_act


# INTERACTIONS DATA #

# read in data
orig_inter <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 3)

# only use real observations
orig_inter <- orig_inter[orig_inter$Obs_Id %in% obs_ids, ]

# fix namings
orig_inter <- left_join(orig_inter %>%
                          mutate(Name = sub("[[:space:]]+$", "", Name)), lookup_levels)
lvl_stake <- unique(orig_inter$Label)
lvl_stake <- stringr::str_sort(c(lvl_stake, "C3. Parent or carer review of records"), numeric = TRUE)
lvl_stake <- lvl_stake[lvl_stake != "None"]
lvl_stake


# LOCATIONS DATA #

# read in data
orig_loco <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 4)

# only use real observations
orig_loco <- orig_loco[orig_loco$Obs_Id %in% obs_ids, ]
gc()

# fix namings
orig_loco <- left_join(orig_loco %>%
                    mutate(Name = sub("[[:space:]]+$", "", Name)), lookup_levels)
lvl_loc <- orig_loco %>%
  select(Code, Label) %>%
  filter(!duplicated(.)) %>%
  arrange(Code, Label) %>%
  pull(Label)
lvl_loc <- sort(lvl_loc[lvl_loc != "None"])
lvl_loc <- lvl_loc[c(grep("^TS", lvl_loc), grep("^SW", lvl_loc), grep("^SA", lvl_loc), grep("^SC", lvl_loc))]
lvl_loc

# TIME ESTIMATE DATA #

# read in observation index data
orig_survey <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 6)

# only use real observations
orig_survey <- orig_survey[orig_survey$Pseudonym %in% pseudo, ]
gc()

# rename columns 
names(orig_survey) <- c(
  # survey info
  "school",
  "School_code",
  "Pseudonym",
  "export_id",
  "survey_id",
  "start_time",
  "completion_time",
  "email",
  "name",
  "year_or_subject",
  # demogs
  "age",
  "gender",
  "teaching_exp_years",
  "Phase",
  "contract_type",
  # subjective experiences
  "neg_conseq_no_hydration",
  "neg_conseq_no_comfort_break",
  "enough_rest",
  "feel_successful",
  "feel_autonomy",
  "feel_school_align",
  "feel_connected",
  # time estimates
  "teaching_time",
  "marking_time",
  "assessment_time",
  "behaviour_time",
  "colleagues_time",
  "planning_resources_time",
  "pastoral_time",
  "admin_time",
  "rest_time",
  "transitions_time",
  "travel_time"
)

# create mapping table with just the code and the corresponding survey item
lookup_survey_task_map <- tribble(
  ~survey_item,                    ~Code, ~Survey_code,
  "teaching_time",                 "A1",   "S1",
  "teaching_time",                 "A2",   "S1",
  "teaching_time",                 "A3",   "S1",
  "teaching_time",                 "A5",   "S1",
  "teaching_time",                 "A6",   "S1",
  
  "marking_time",                  "D2",   "S5",
  
  "assessment_time",               "D3",   "S4",
  
  "behaviour_time",                "A7",   "S2",
  
  "colleagues_time",               "B1",   "S9",
  "colleagues_time",               "B2",   "S9",
  "colleagues_time",               "B3",   "S9",
  "colleagues_time",               "B4",   "S9",
  "colleagues_time",               "B5",   "S9",
  "colleagues_time",               "B6",   "S9",

  "planning_resources_time",       "D1",   "S7",
  "planning_resources_time",       "D7",   "S7",
  
  "pastoral_time",                 "A8",   "S6",
  "pastoral_time",                 "A9",   "S6",
  
  "admin_time",                    "A12",  "S8",
  "admin_time",                    "D6",   "S8",
  "admin_time",                    "D4",   "S8",
  
  "rest_time",                     "F1",   "S10",
  "rest_time",                     "F2A",  "S10",
  
  "transitions_time",              "A4",   "S3",
  
  "travel_time",                   "G1",   "S11"
)

# List your time columns (use your short names or the originals)
time_cols <- c(
  "teaching_time", "marking_time", "assessment_time", "behaviour_time",
  "colleagues_time", "planning_resources_time", "pastoral_time", "admin_time",
  "rest_time", "transitions_time", "travel_time"
)


# convert estimates to minutes
df_survey <- orig_survey %>%
  mutate(across(all_of(time_cols), ~sapply(., parse_duration_to_minutes )))

# fix data for the one ppt who estimated for a week rather than a day
df_survey[df_survey$Pseudonym == "SST1", time_cols] <- df_survey[df_survey$Pseudonym == "SST1", time_cols]/5


# Combine original and converted values side by side
survey_compare <- orig_survey %>%
  select(all_of(time_cols)) %>%
  rename_with(~ paste0(.x, "_old")) %>%
  bind_cols(
    df_survey %>% select(all_of(time_cols)) %>% rename_with(~ paste0(.x, "_new"))
  )

# check results
survey_compare <- survey_compare[, sort(names(survey_compare))]

# delete dfs
rm(survey_compare)

# EVENING WORK ESTIMATE DATA #

# read in observation index data
orig_eve <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 7)

# only use real observations
orig_eve <- orig_eve[orig_eve$Pseudonym %in% pseudo, ]
gc()

# SCHOOL DAY TIMES #

# read in observation index data
orig_sch <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 8)

# only use real observations
orig_sch <- orig_sch[orig_sch$Pseudonym %in% pseudo, ]
gc()

# combine levels
lvl <- c(lvl_stake, lvl_act, lvl_loc)
lvl_c <- stringr::str_extract(lvl, "^[A-Z]+[0-9]+")


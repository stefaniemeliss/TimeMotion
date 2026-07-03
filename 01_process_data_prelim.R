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
names(obs_index)[names(obs_index) == "NA..2"] <- "School_name"

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

# locoATION DATA #

# read in observation index data
loco <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 4)

# only use real observations
loco <- loco[loco$Obs_Id %in% obs_ids, ]
gc()

# TIME ESTIMATE DATA #

# read in observation index data
survey <- xlsx::read.xlsx(file = file.path(dir_data_in, file_name), sheetIndex = 5)

# only use real observations
survey <- survey[survey$Pseudonym %in% pseudo, ]
gc()

# rename columns 
names(survey) <- c(
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
  "phase",
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
survey_task_mapping <- tribble(
  ~survey_item,                    ~task_code,
  "teaching_time",                 "A2",
  "teaching_time",                 "A5",
  "teaching_time",                 "A6",
  # "teaching_time",                 "A9",
  "teaching_time",                 "A10",
  "teaching_time",                 "A12",
  "marking_time",                  "D8",
  "assessment_time",               "D2",
  "behaviour_time",                "A3",
  "colleagues_time",               "B1",
  "colleagues_time",               "B2",
  "colleagues_time",               "B3",
  "colleagues_time",               "B4",
  "colleagues_time",               "B5",
  "colleagues_time",               "B6",
  "colleagues_time",               "B7",
  "planning_resources_time",       "D4",
  "planning_resources_time",       "D5",
  "pastoral_time",                 "A7",
  "pastoral_time",                 "A8",
  "admin_time",                    "A9",
  "admin_time",                    "D1",
  "admin_time",                    "D3",
  "rest_time",                     "F1",
  "rest_time",                     "F2",
  "transitions_time",              "A11",
  "travel_time",                   "G1"
)

# List your time columns (use your short names or the originals)
time_cols <- c(
  "teaching_time", "marking_time", "assessment_time", "behaviour_time",
  "colleagues_time", "planning_resources_time", "pastoral_time", "admin_time",
  "rest_time", "transitions_time", "travel_time"
)


# create safety copy
survey_orig <- survey 

# convert estimates to minutes
survey <- survey %>%
  mutate(across(all_of(time_cols), ~sapply(., parse_duration_to_minutes )))

# Combine original and converted values side by side
survey_compare <- survey_orig %>%
  select(all_of(time_cols)) %>%
  rename_with(~ paste0(.x, "_old")) %>%
  bind_cols(
    survey %>% select(all_of(time_cols)) %>% rename_with(~ paste0(.x, "_new"))
  )

# check results
survey_compare <- survey_compare[, sort(names(survey_compare))]

# delete dfs
rm(survey_orig, survey_compare)

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

# add observation date
sch <- sch %>% 
  left_join(., obs_index %>%
                    select(Pseudonym, Date)) %>%
  mutate(
    pupil_hours_start = paste(Date, Pupil.school.day.start.time.),
    pupil_hours_end = paste(Date, Pupil.school.day.end.time),
    ) %>%
  mutate(
    pupil_hours_start = as.POSIXct(pupil_hours_start, format = "%a %m/%d/%Y %I.%M%p", tz = "Europe/London"),
    pupil_hours_end = as.POSIXct(pupil_hours_end, format = "%a %m/%d/%Y %I.%M%p", tz = "Europe/London")
    )



# -------------------------------------------------------------------
# determine variable levels according to the taxonomy used 
# -------------------------------------------------------------------

# individual tasks
indiv$activity_name <- sapply(indiv$Name, clean_items)
lvl_act <- unique(indiv$activity_name)
lvl_act <- stringr::str_sort(c(lvl_act, "D6. Self-study and professional development"), numeric = TRUE)
lvl_act <- lvl_act[lvl_act != "None"]
lvl_act

# stakeholder interactions
# lvl_stake <- unique(inter$Display_Name)
# lvl_stake <- sort(c(lvl_stake, "C3. Parent logs"))
# lvl_stake <- c(lvl_stake[-1], lvl_stake[1])

inter$stakeholder <- sapply(inter$Name, clean_items)
lvl_stake <- unique(inter$stakeholder)
lvl_stake <- stringr::str_sort(c(lvl_stake, "B6. Professional development meeting", "C3. Parent or carer review of records"), numeric = TRUE)
lvl_stake <- lvl_stake[lvl_stake != "None"]
lvl_stake

# locations
lvl_loc <-c(
  # Teaching and learning spaces
  "Main classroom",
  "Other classroom",
  "School library",
  "School hall",
  # Pastoral and administrative spaces
  "Pastoral office",
  "Administrative office",
  "Senior leadership team office",
  "Reception",
  "Reprographics",  
  # Staff and meeting spaces
  "Staffroom",
  "Meeting/PPA room",
  # Shared and communal areas
  "Canteen",  
  "Outside classroom door",
  "Corridor",
  "Front of school and wider school grounds ",
  "Playground",
  # Other
  "Other location (please state)",
  "None"
)

# combine levels
lvl <- c(lvl_stake, lvl_act, lvl_loc)

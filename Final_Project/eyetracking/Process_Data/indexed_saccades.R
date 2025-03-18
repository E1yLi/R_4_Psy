library(dplyr)
library(readxl)
library(writexl)

# Load and process saccades dataset
saccades <- read_excel("xl_files/EDV_saccades.xlsx") |> 
  select(
    index_trial = TRIAL_INDEX,
    stimulus_id_trial = NAME,
    type_stimulus = TYPE,
    session = DATA_FILE,
    
    total_saccades = TRIAL_SACCADE_TOTAL,
    next_fix_x = NEXT_FIX_X,
    next_fix_y = NEXT_FIX_Y,
    next_fix_duration = NEXT_FIX_DURATION,
    next_fix_pupil = NEXT_FIX_PUPIL,
    amplitude = CURRENT_SAC_AMPLITUDE,
    angle = CURRENT_SAC_ANGLE,
    contains_blink = CURRENT_SAC_CONTAINS_BLINK,
    direction = CURRENT_SAC_DIRECTION,
    duration = CURRENT_SAC_DURATION,
    end_interest_area = CURRENT_SAC_END_INTEREST_AREA_LABEL,
    end_x = CURRENT_SAC_END_X,
    end_y = CURRENT_SAC_END_Y,
    index_in_trial = CURRENT_SAC_INDEX,
    nearest_end_iarea = CURRENT_SAC_NEAREST_END_INTEREST_AREA,
    nearest_start_iarea = CURRENT_SAC_NEAREST_START_INTEREST_AREA_LABEL,
    start_interest_area = CURRENT_SAC_START_INTEREST_AREA_LABEL,
    start_x = CURRENT_SAC_START_X,
    start_y = CURRENT_SAC_START_Y,
    velocity = CURRENT_SAC_AMPLITUDE / CURRENT_SAC_DURATION,
    end_time = CURRENT_SAC_END_TIME )  |>
  filter(type_stimulus == "passive")

# Load messages dataset
load("rdata/messages.RData")

# Merge saccades with messages
saccades <- messages |> 
  left_join(select(saccades, -index_trial, -type_stimulus), 
            by = c("stimulus_id_trial", "session")) |> mutate_at(vars(
              total_saccades, next_fix_x, next_fix_y, next_fix_duration, next_fix_pupil, 
              amplitude, angle, duration, end_x, end_y, start_x, start_y, velocity,length_stimulus), as.numeric) 


saccades <- saccades |> filter( # remove invalid observations
    next_fix_x !=  ".",
    end_x != ".") |> filter(!is.na(next_fix_x) & !is.na(end_x))

grouped_saccades <- saccades |> 
  group_by(index_trial, session) |> 
  group_split()

processed_saccades <- list()

for (group in grouped_saccades) {
  has_first <- FALSE
  idx <- 1
  filtered_saccades <- list()
  for (i in seq_len(nrow(group))) {
    row <- group[i, ]

    if ((row$end_x > 802 & row$end_x < 1398) & has_first) {
      row$index_in_trial <- idx
      row$end_time <- row$end_time - first_t # test for neg values later on 
      idx <- idx + 1
      filtered_saccades <- append(filtered_saccades, list(row))
      next
    }
    
    # if (row$start_x == "." | row$end_x == ".") {
    #   next
    # }
    
    if ((row$end_x > 802 & row$end_x < 1398) & !has_first) { # 3 visual angles from stimulus
      row$index_in_trial <- 1
      first_t <- row$end_time
      row$end_time <- 1 # not zero for regression coefs 
      has_first <- TRUE
      filtered_saccades <- append(filtered_saccades, list(row))
      next
    }
  }
  if (length(filtered_saccades) == 0){
    print(group)
  }
  filtered_group <- bind_rows(filtered_saccades)
  filtered_group$total_saccades <- nrow(filtered_group)
  processed_saccades <- append(processed_saccades, list(filtered_group))
}

indexed_saccades <- bind_rows(processed_saccades) |> ungroup()
print("")
# indexed_saccades <- indexed_saccades |> 
#   mutate(
#     dist_landing_2_fix = as.numeric(next_fix_x) - as.numeric(end_x),
#     dist_from_comma = case_when(
#       nchar(length_stimulus) == 3 ~ end_x - 948,
#       nchar(length_stimulus) == 4 ~ end_x - 948,
#       nchar(length_stimulus) == 5 ~ end_x - 1014,
#       nchar(length_stimulus) == 6 ~ end_x - 1080),
#     
#       next_fix_duration = (next_fix_duration - mean(next_fix_duration, na.rm = TRUE)) / 
#         sd(next_fix_duration, na.rm = TRUE),
#       
#       next_fix_pupil = (next_fix_pupil - mean(next_fix_pupil, na.rm = TRUE)) / 
#         sd(next_fix_pupil, na.rm = TRUE),
#       
#       amplitude = (amplitude - mean(amplitude, na.rm = TRUE)) / 
#         sd(amplitude, na.rm = TRUE),
#       
#       duration = (duration - mean(duration, na.rm = TRUE)) / 
#         sd(duration, na.rm = TRUE),
#       next_fix_y = (1080 - next_fix_y) + (1080 / 2),
#       end_y = (1080 - end_y) + (1080 / 2),
#       start_y = (1080 - start_y) + (1080 / 2),
#       next_fix_x = next_fix_x + (1920 / 2),
#       end_x = end_x + (1920 / 2),
#       start_x = start_x + (1920 / 2))

save(indexed_saccades, file = "rdata/indexed_saccades.RData")

if (!dir.exists("xl_output")) {
  dir.create("xl_output", recursive = TRUE)
}

write_xlsx(indexed_saccades, "xl_output/indexed_saccades.xlsx")



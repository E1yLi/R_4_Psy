# join trial info from saccades with messages
library(dplyr)
library(readxl)
library(writexl)


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
    start_y = CURRENT_SAC_START_Y
  )



load("rdata/messages.RData")

saccades <- messages |> 
  left_join(select(saccades, -index_trial, -type_stimulus), 
            by = c("stimulus_id_trial", "session"))

save(saccades, file = "rdata/saccades.RData")

write_xlsx(saccades, "xl_output/saccades.xlsx")

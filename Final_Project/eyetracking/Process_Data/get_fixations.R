# join trial info from fixations with messages

library(dplyr)
library(readxl)

fixations <- read_excel("xl_files/EDV_fixations.xlsx") |> 
  select(
    index_trial = TRIAL_INDEX,
    stimulus_id_trial = NAME,
    type_stimulus = TYPE,
    session = DATA_FILE,
    
    iarea = CURRENT_FIX_INTEREST_AREA_LABEL,
    nearest_iarea = CURRENT_FIX_NEAREST_INTEREST_AREA_LABEL,
    dist_nearest_iarea = CURRENT_FIX_NEAREST_INTEREST_AREA_DISTANCE,
    dwell_iarea = CURRENT_FIX_INTEREST_AREA_DWELL_TIME,
    pupil_size = CURRENT_FIX_PUPIL,
    x = CURRENT_FIX_X,
    y = CURRENT_FIX_Y,
    duration = CURRENT_FIX_DURATION,
    index_in_trial = CURRENT_FIX_INDEX,
    total_in_trial = TRIAL_FIXATION_TOTAL,
    start_t = CURRENT_FIX_START,
    end_t = CURRENT_FIX_END
  )

save(fixations, file = "rdata/fixations.RData")
load("rdata/messages.RData")

fixations <- messages |>
  left_join(select(fixations, -index_trial, -type_stimulus), 
            by = c("stimulus_id_trial", "session"))

save(fixations, file = "rdata/fixations.RData")
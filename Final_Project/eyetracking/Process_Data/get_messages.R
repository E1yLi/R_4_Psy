# this script aligns stimulus presented with eyelink and psychopy trial indexing

#Tests: 
# No passive trials dropped
#stimulus is consistent between the participants


library(dplyr)
library(readxl)

edv_messages <- read_excel("xl_files/EDV_messages_1.xlsx") |> 
  select(CURRENT_MSG_TEXT, CURRENT_MSG_TIME, DATA_FILE, CURRENT_MSG_FIX_X, TRIAL_INDEX,TYPE,NAME)

messages <- data.frame(index_trial = character(),
                       stimulus_id_trial = character(), #psychopy id
                       session = character(),
                       stimulus = character(),
                       type_stimulus = character(),
                       length_stimulus = numeric(),
                       answer = logical(),
                       early_end = logical(),
                       stringsAsFactors = FALSE)

trial_groups <- edv_messages |> group_by(TRIAL_INDEX, DATA_FILE) |> group_split()

for (group in trial_groups) {
  if (!(startsWith(group$CURRENT_MSG_TEXT[1], "TRIALID") & 
        startsWith(tail(group$CURRENT_MSG_TEXT, 1), "TRIAL_RESULT"))) {
    cat("Skipping TRIAL_INDEX:", unique(group$TRIAL_INDEX), "DATA_FILE:", unique(group$DATA_FILE), "\n")
    next
  }
  
  trialid_row <- group |> filter(startsWith(CURRENT_MSG_TEXT, "TRIALID "))
  stimulus_id_trial <- trialid_row$NAME[1]
  id <- trialid_row$DATA_FILE[1]
  index_trial <- trialid_row$TRIAL_INDEX[1]
  type_stimulus <-trialid_row$TYPE[1]
  fill_msg <- group |> filter(startsWith(CURRENT_MSG_TEXT, "FILL "))
  stimulus <- if (nrow(fill_msg) > 0) gsub("\\D", "", fill_msg$CURRENT_MSG_TEXT[1]) else NA
  len <- nchar(stimulus)
  trial_result_row <- group |> filter(startsWith(CURRENT_MSG_TEXT, "TRIAL_RESULT "))
  
  if (nrow(trial_result_row) > 0) {
    result_text <- sub("TRIAL_RESULT ", "", trial_result_row$CURRENT_MSG_TEXT)
    answer <- ifelse(substr(result_text, 1, 1) == "0", FALSE,
                     ifelse(substr(result_text, 1, 1) == "1", TRUE, NA))
    early_end <- ifelse(trial_result_row$CURRENT_MSG_FIX_X[1] > 700, TRUE, FALSE)
    
    messages <- rbind(messages,data.frame(
                      index_trial =  index_trial, 
                      session= id, # eyelink trial id
                      stimulus_id_trial = stimulus_id_trial, #psychopy id
                      stimulus = stimulus,
                      type_stimulus = type_stimulus,
                      length_stimulus = len,
                      answer = answer,
                      early_end = early_end,
                      stringsAsFactors = FALSE) )
  }
}

save(messages, file = "rdata/messages.RData")

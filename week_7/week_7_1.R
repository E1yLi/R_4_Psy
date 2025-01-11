# R course for beginners
# Week 7
# assignment by Ely Liebowitz, id 209386309 

library(dplyr)


all_data <- data.frame()

# Loop through files and row-bind them
for (cur in files) {
  file_data <- read.csv(cur)
  all_data <- rbind(all_data, file_data)
}

raw_data <- all_data |> mutate(
    task = ifelse(grepl("word", condition), "read_word", "name_ink"),
    congruency = ifelse(grepl("_incong", condition), "incongruent", "congruent"),
    accuracy = ifelse(is.na(participant_response), NA,ifelse(correct_response == participant_response, 0, 1)),
    rt_sec = ifelse(is.na(rt), NA,(as.numeric(rt) / 1000)),
    id = subject) |> select(id, task, congruency, block, trial, accuracy, rt_sec)

raw_data$task = as.factor(raw_data$task)
raw_data$congruency = as.factor(raw_data$congruency)
raw_data$id = as.factor(raw_data$id)

print(head(raw_data))

save(raw_data, file = "raw_data.RData")

# id      task  congruency block trial accuracy rt_sec
# 1 subj001 read_word   congruent     1     1        0  0.736
# 2 subj001 read_word   congruent     1     2        0  0.634
# 3 subj001 read_word   congruent     1     3        0  0.786
# 4 subj001  name_ink incongruent     1     4        0  1.265
# 5 subj001 read_word   congruent     1     5       NA     NA
# 6 subj001  name_ink incongruent     1     6       NA     NA
# 
# > save(raw_data, file = "raw_data.RData")
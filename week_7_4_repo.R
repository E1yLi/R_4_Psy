# R course for beginners
# Week 7
# assignment by Ely Liebowitz, id  

library(dplyr)
files <- list.files(path = "stroop_data", pattern = "*.csv", full.names = TRUE)


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

# R course for beginners
# Week 7
# assignment by Ely Liebowitz, id  

library(dplyr)
library(lme4)

load("raw_data.rdata")

filtered_data <- raw_data |>
  filter(!is.na(rt_sec), rt_sec >= 0.3 & rt_sec <= 3)

filtered_data |>summarise(
  mean_accuracy = mean(accuracy, na.rm = TRUE),
  sd_accuracy = sd(accuracy, na.rm = TRUE),
  min_accuracy = min(accuracy, na.rm = TRUE),
  max_accuracy = max(accuracy, na.rm = TRUE)
) |>
  print()

exclusion_stats <- raw_data |> 
  group_by(id) |> 
  summarise(
    total_trials = n(),
    excluded_trials = sum(is.na(rt_sec) | rt_sec < 0.3 | rt_sec > 3),
    exclusion_rate = excluded_trials / total_trials
  )

cat(sprintf("Average exclusion rate: %.2f percent", mean(exclusion_stats$exclusion_rate) * 100))
cat(sprintf("SD of exclusion rate: %.2f percent ", sd(exclusion_stats$exclusion_rate) * 100))


# Average exclusion rate: 6.22%
# SD of exclusion rate: 2.13%

exclusion_stats |> group_by(id) |> summarise(sprintf("%.2f percent of trials excluded",exclusion_rate * 100))

# id      `sprintf("%.2f percent of trials excluded", exclusion_rate * 100)`
# 1 subj001 8.75 percent of trials excluded                                   
# 2 subj002 8.50 percent of trials excluded                                   
# 3 subj003 2.50 percent of trials excluded                                   

model <- glmer(accuracy ~ task * congruency + rt_sec + (1 | id), 
               data = filtered_data, family = binomial)

summary(model) |>
  print()

anova(model) |>
  print()

# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                         -4.24015    0.28791 -14.727  < 2e-16 ***
#   taskread_word                        0.26573    0.17066   1.557  0.11946    
# congruencyincongruent               -0.03481    0.15904  -0.219  0.82676    
# rt_sec                               0.76152    0.25993   2.930  0.00339 ** 
#   taskread_word:congruencyincongruent  0.13057    0.21665   0.603  0.54672    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Correlation of Fixed Effects:
#   (Intr) tskrd_ cngrnc rt_sec
# taskred_wrd -0.649                     
# cngrncyncng -0.034  0.342              
# rt_sec      -0.920  0.434 -0.255       
# tskrd_wrd:c  0.036 -0.563 -0.731  0.175
# 
# > anova(model) |>
#   +   print()
# Analysis of Variance Table
# npar Sum Sq Mean Sq F value
# task               1 0.3052  0.3052  0.3052
# congruency         1 0.7718  0.7718  0.7718
# rt_sec             1 8.2091  8.2091  8.2091
# task:congruency    1 0.3619  0.3619  0.3619

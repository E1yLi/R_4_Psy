library(dplyr)

summarize_df <- function(data, subject_start = NULL, subject_end = NULL) {
  if (nrow(data) < 10) {
    stop("Error: DataFrame provided is too short to analyze.")
  }
  
  if (!is.null(subject_start) & !is.null(subject_end)) {
    data <- data %>%
      filter(subject_number >= subject_start & subject_number <= subject_end)
  }
  
  if (nrow(data) < 10) {
    stop("Error: Filtered DataFrame provided is too short to analyze.")
  }
  
  results <- data %>%
    summarise(
      df_name = "imported_df",
      avg_age = mean(age, na.rm = TRUE),
      min_age = min(age, na.rm = TRUE),
      max_age = max(age, na.rm = TRUE),
      var_age = var(age, na.rm = TRUE),
      mean_rt = mean(avg_rt, na.rm = TRUE),
      min_rt = min(avg_rt, na.rm = TRUE),
      max_rt = max(avg_rt, na.rm = TRUE),
      var_rt = var(avg_rt, na.rm = TRUE),
      avg_depression = mean(depression, na.rm = TRUE),
      min_depression = min(depression, na.rm = TRUE),
      max_depression = max(depression, na.rm = TRUE),
      var_depression = var(depression, na.rm = TRUE),
      mean_sleep = mean(avg_sleep, na.rm = TRUE),
      min_sleep = min(avg_sleep, na.rm = TRUE),
      max_sleep = max(avg_sleep, na.rm = TRUE),
      var_sleep = var(avg_sleep, na.rm = TRUE),
      gender_counts = paste0("Male: ", sum(data$gender == "Male", na.rm = TRUE),
                             "; Female: ", sum(data$gender == "Female", na.rm = TRUE))
    )
  
  return(results)
}

write.csv(summarize_df(read.csv("analysis.csv"),1,150), "results_of_analysis.csv", row.names = FALSE)
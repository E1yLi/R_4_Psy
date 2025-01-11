#analysis

library(dplyr)

n <- 1000  


df <- tibble(
  subject_number = 1:n,
  age = sample(18:80, n, replace = TRUE), 
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  avg_rt = runif(n, 200, 6000), 
  depression = round(runif(n, 0, 100)), 
  avg_sleep = runif(n, 2, 12)  
)


write.csv(df, "analysis.csv", row.names = FALSE)

cat("Dataset saved: 'participants_data.csv'.\n")

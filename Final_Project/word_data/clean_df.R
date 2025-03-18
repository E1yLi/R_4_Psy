#simple functions to transform  xl to RDATA and remove NA
library(readxl)
library(writexl)
library(dplyr)

df <- read_excel("data/ratings.xlsx")

df <- df  |>na.omit()

if (!dir.exists("xl_output")) dir.create("xl_output")

write_xlsx(df, "xl_output/words_for_sgpt.xlsx")
save(df, file = "output/dictionary_df.RData")

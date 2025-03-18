library(readxl)
library(dplyr)
library(purrr)


load("output/dictionary_df.RData")


folder_path <- "data/sgpt_backups"

file_list <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

process_file <- function(file) {
  df <- read_excel(file)

  df <- df |> 
    mutate(embeddings = pmap(select(df, `1`:`2048`), c)) |> 
    select(name, embeddings)
  
  return(df)
}

embeddings_df <- bind_rows(lapply(file_list, process_file))
embeddings_df <- embeddings_df |> distinct(name, .keep_all = TRUE)

dictionary_df <- df |> 
  left_join(embeddings_df, by = "name")


save(dictionary_df, file = "output/dictionary_df.RData")

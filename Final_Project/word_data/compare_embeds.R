library(dplyr)
library(ggplot2)
library(ggrepel)
library(purrr)
# Load dictionary_df with name and embeddings columns
load("output/dictionary_df.RData")
output_path <- "output/pca.RData"

if (!file.exists(output_path)) {
  embedding_matrix <- do.call(rbind, dictionary_df$embeddings) # this is a really big matrix, beware of crashes during PCA
  pca_result <- prcomp(embedding_matrix, center = TRUE, scale. = TRUE)
  save(pca_result, file = output_path)
} else {
  load(output_path)
}

plot_word_embeddings <- function(word_list, dictionary_df, title_ext) {
  
  df_word_list <- tibble(name = word_list) |>
    left_join(dictionary_df, by = "name") |>
    mutate(embed_vec = map(embeddings, ~ if (is.null(.x)) NA_real_ else as.numeric(.x))) |>
    mutate(
      PC1 = map_dbl(embed_vec, ~ if (all(is.na(.x))) NA_real_ else sum(.x * pca_result$rotation[,1])),
      PC2 = map_dbl(embed_vec, ~ if (all(is.na(.x))) NA_real_ else sum(.x * pca_result$rotation[,2]))
    )|>
    mutate(idx = row_number()) |>
    group_by(grp = ceiling(idx / 2)) |>
    filter(!any(is.na(PC1) | is.na(PC2))) |>
    ungroup()
  
  ggplot(df_word_list, aes(PC1, PC2)) +
    geom_point(shape = "x", color = "black", size = 3) +
    geom_line(aes(group = grp), color = "red", linewidth = 0.1) +
    geom_text_repel(aes(label = name)) +
    labs(title = paste0("PCA Projection: ", title_ext)) +
    theme_minimal()

}
words <- "empty full minimum maximum none all only multiple miniature gigantic"
#words <- "warm cool hot cold steam frost fire ice summer winter boil freeze"
word_list <- c(strsplit( words, "[ \n]+")[[1]]) # split by line break OR SPACE

plot_word_embeddings(word_list, dictionary_df,"All or Nothing?")


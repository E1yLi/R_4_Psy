library(dplyr)
library(purrr)
library(tidyverse)
library(jtools)
library(pROC)

load("output/weighted_avg_embedding.RData")
#load data
folder_path <- "data/embeddings"
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

#cleanup func for the test data which was saved incorrectly
process_file_tidyverse <- function(file) {
  df <- read_csv(file, col_types = cols(.default = "c")) |> 
    select(condition, name, '1':'2048') |>  
    mutate(across(everything(), ~ str_replace_all(.x, "tensor\\(|, device='cuda:0'\\)", ""))) |>  
    mutate(across(-c(condition, name), as.numeric))  
  return(df)
}

embeddings_df <- bind_rows(lapply(file_list, process_file_tidyverse))

embeddings_df <- embeddings_df |> 
  mutate(embeddings = pmap(select(embeddings_df, `1`:`2048`), c))|> 
  select(condition, name, embeddings) 

for (i in 1:nrow(embeddings_df)) {  #make sure theyre all unit vecs
  embedding <- embeddings_df$embeddings[[i]]
  norm <- sqrt(sum(embedding^2))
  embeddings_df$embeddings[[i]] <- embedding / norm
}


#get cosine 
embeddings_df <- embeddings_df |> 
  mutate(dot_product = map_dbl(embeddings, ~ sum(.x * weighted_avg_embedding)))

#normalize output
# embeddings_df <- embeddings_df |> 
#   mutate(scaled_conc = scale(dot_product, center = TRUE, scale = TRUE)[, 1])

embeddings_df$condition <- factor(embeddings_df$condition, levels = c("wiki", "vis"))

logit_model <- glm(condition ~ dot_product, data = embeddings_df, family = binomial)

summ(logit_model)

embeddings_df$predicted_probs <- predict(logit_model, type = "response")

roc_obj <- roc(embeddings_df$condition, predicted_probs)
auc_value <- auc(roc_obj)

plot(roc_obj, col = "blue", lwd = 2, main = paste("ROC Curve - LogReg wiki/Visual | AUC:", round(auc_value, 3)))
# 
# MODEL INFO:
#   Observations: 86
# Dependent Variable: condition
# Type: Generalized linear model
# Family: binomial 
# Link function: logit 
# 
# MODEL FIT:
#   χ²(1) = 31.72, p = 0.00
# Pseudo-R² (Cragg-Uhler) = 0.41
# Pseudo-R² (McFadden) = 0.27
# AIC = 91.50, BIC = 96.41 
# 
# Standard errors:MLE
# ------------------------------------------------
#   Est.   S.E.   z val.      p
# ----------------- ------- ------ -------- ------
#   (Intercept)         -1.37   0.40    -3.40   0.00
# dot_product         15.60   3.44     4.53   0.00
# ------------------------------------------------
embeddings_df$p <- predict(logit_model, type = "response")

ggplot(embeddings_df, aes(x = condition, y = p, color = condition)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +  # Single box for each condition
  geom_point(position = position_jitter(width = 0.01), size = 2, alpha = 0.9) +  # Jittered scatter plot
  geom_line(aes(group = name), alpha = 0.3, color = "gray") +  # Connect points of the same name
  labs(title ="Logisitic Model Predicted Probability of Class Visual",
       x = "Condition",
       y = "Predicted Probability (p)") +
  theme_minimal() +
  theme(legend.position = "top")


#sigmoid plot that isnt very informative
# scaled_conc_seq <- data.frame(scaled_conc = seq(min(embeddings_df$scaled_conc, na.rm = TRUE), 
#                                                 max(embeddings_df$scaled_conc, na.rm = TRUE), 
#                                                 length.out = 300))
# 
# scaled_conc_seq$p <- predict(logit_model, newdata = scaled_conc_seq, type = "response")

# ggplot(scaled_conc_seq, aes(x = scaled_conc, y = p)) +
#   geom_line(color = "gray", size = 1) +  # Sigmoid curve
#   geom_point(data = embeddings_df, aes(x = scaled_conc, y = p, color = condition), alpha = 0.9) +  # Original points
#   labs(title = "Sigmoid Curve for Logistic Regression",
#        x = "Scaled Dot Product (scaled_conc)",
#        y = "Predicted Probability (p)") +
#   theme_minimal()


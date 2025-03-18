library(dplyr)
library(ggrepel)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(purrr)
library(jtools)
library(car)
# 
# Compute  CONC VEC
#
# Get Cosines with the CONC VEC
# 

load("output/dictionary_df.RData")

train_df <- dictionary_df
train_df$Conc <- scale(train_df$Conc.M, center = TRUE, scale = TRUE)

train_df <- train_df |>
  mutate(embeddings= map(embeddings, ~ .x / sqrt(sum(.x^2))))

compute_weighted_avg_embedding <- function(train_df) {
  
  embedding_matrix <- do.call(rbind, train_df$embeddings)
  
  scaled_conc <- scale(train_df$Conc.M, center = TRUE, scale = FALSE)[,1]
  
  #weighted_embeddings <- sweep(embedding_matrix,1 , scaled_conc,"*")
  weighted_embeddings <- embedding_matrix * scaled_conc
  
  avg_embedding <- colMeans(weighted_embeddings, na.rm = TRUE)
  avg_embedding <- avg_embedding / sqrt(sum(avg_embedding^2))
  return(avg_embedding)
}

weighted_avg_embedding <- compute_weighted_avg_embedding(train_df) #compute conc vec

train_df <- train_df |> 
  mutate(
    dot_product = map_dbl(embeddings, ~ sum(.x %*% weighted_avg_embedding)))

min_dot_product <- min(train_df$dot_product, na.rm = TRUE)
max_dot_product <- max(train_df$dot_product, na.rm = TRUE)

print(min_dot_product)
print(max_dot_product)

####
lm_model <- lm(Conc ~ dot_product, data = train_df)
summ(lm_model)
ggplot(data.frame(Fitted = lm_model$fitted.values, Residuals = residuals(lm_model)), 
       aes(x = Fitted, y = Residuals)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values (OLS Model)")


ncvTest(lm_model)

weights <- 1 / lm(abs(lm_model$residuals) ~ lm_model$fitted.values)$fitted.values^2
wls_model <- lm(Conc ~ dot_product, data = train_df, weights = weights)

ncvTest(wls_model)

summ(wls_model)

ggplot(data.frame(Fitted = wls_model$fitted.values, Residuals = residuals(wls_model)), 
       aes(x = Fitted, y = Residuals)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values (WLS Model)")


# Extract coefficients
intercept <- coef(wls_model)[1]

slope <- coef(wls_model)[2]

# Training Set Plot
plot_train <- ggplot(train_df, aes(x = dot_product, y = Conc)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
  geom_abline(intercept = intercept, slope = slope, color = "black", linewidth = 1) +  # Manually added regression line
  scale_fill_distiller(palette = "Spectral") +
  scale_y_continuous(limits = c(-3, 3)) + 
  labs(title = "Training Set: Density Map with WLS line",
       subtitle = paste("Train N:", nrow(train_df)),
       x = "Dot Product",
       y = "Mean Concreteness Rating") +
  theme_minimal()+ theme(aspect.ratio=1)

# Test Set Plot
# plot_test <- ggplot(train_df, aes(x = dot_product, y = Conc)) +
#   stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha = 0.5) +
#   geom_abline(intercept = intercept, slope = slope, color = "black", linewidth = 1) +  # Manually added regression line
#   scale_fill_distiller(palette = "Spectral") + scale_y_continuous(limits = c(-3, 3)) + 
#   labs(title = "Test Set: Density Map with OLS line",
#        subtitle = paste("Test N:", nrow(train_df)),
#        x = "Dot Product",
#        y = "Mean Concreteness Rating") +
#   theme_minimal()+ theme(aspect.ratio=1)


# Combine plots side by side
grid.arrange(plot_train, nrow = 1)


# MODEL INFO:
#   Observations: 14938
# Dependent Variable: Conc
# Type: OLS linear regression 
# 
# MODEL FIT:
#   F(1,14936) = 17530.90, p = 0.00
# R² = 0.54
# Adj. R² = 0.54 
# 
# Standard errors:OLS
# ------------------------------------------------
#   Est.   S.E.   t val.      p
# ----------------- ------- ------ -------- ------
#   (Intercept)          3.50   0.01   589.16   0.00
# dot_product         26.03   0.20   132.40   0.00
# ------------------------------------------------


num_samples <-40

# Sample 40 values equally spaced along dot_product
sample_df <- train_df |> 
  arrange(dot_product) |> 
  slice(round(seq(1, n(), length.out = num_samples)))

ggplot(sample_df, aes(x = dot_product, y = Conc)) +
  geom_point(color = "black", size = 1) +  # Plot points
  geom_abline(intercept = intercept, slope = slope, color = "black", linewidth = 1) +  
  geom_segment(aes(xend = dot_product, yend = predict(wls_model, newdata = sample_df)), 
               color = "red", linetype = "dashed", size = 0.5) +  # Residuals
  geom_text_repel(aes(label = name), size = 3, box.padding = 0.3) +  # Add word labels
  labs(title = "Regression of Cosine Scale onto mean Conc. Rating",
       subtitle = "Sample of 40 words from Britannica dataset w/ residuals",
       x = "Cosine Score", y = "Mean Concreteness Rating") +
  theme_minimal()
save(weighted_avg_embedding, file = "output/weighted_avg_embedding.RData")
save(train_df, file = "output/train_df.RData")

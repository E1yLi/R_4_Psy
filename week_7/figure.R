library(ggplot2)

# Create a figure: Reaction time by task and congruency
plot <- ggplot(filtered_data, aes(x = congruency, y = rt_sec, fill = task)) +
  geom_boxplot() +
  labs(
    title = "Reaction Time by Task and Congruency",
    x = "Congruency",
    y = "Reaction Time (seconds)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria"), # Customize font
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Save the figure
ggsave("reaction_time_plot.png", plot = plot, width = 8, height = 6)

print("Figure saved as 'reaction_time_plot.png'")
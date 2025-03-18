library(magick)
library(dplyr)
library(ggpubr) # for bg image
library(grid)


images_path <- "images/"
plots_path <- "plots/single_trial_plots/"

arrow_width <- 0.4
arrow_color <- "cyan"
arrow_tip_size <- 0.14
fixation_fill <- NA # hollow
fixation_line_width <- 0.3
fixation_color <- "lightblue"
fixation_diameter <- 8

load("saccades.RData")

visualize_trial <- function(df, image_list) {
  if (!dir.exists(plots_path)) {
    dir.create(plots_path, recursive = TRUE)
  }
  
  for (image_num in image_list) {
    df_trial <- df |> filter(IMAGE_PATH == paste0(images_path, as.character(image_num), ".png"))
    if (nrow(df_trial) == 0) next
    
    img_path <- df_trial$IMAGE_PATH[1]
    if (!file.exists(img_path)) next
    
    img <- image_read(img_path)
    img_gg <- ggplot() + background_image(img)
    
    p <- img_gg +
      geom_segment(aes(x = CURRENT_SAC_START_X, y = CURRENT_SAC_START_Y,
                       xend = CURRENT_SAC_END_X, yend = CURRENT_SAC_END_Y),
                   data = df_trial,
                   arrow = arrow(length = unit(arrow_tip_size, "cm")),
                   color = arrow_color,
                   linewidth = arrow_width) +
      geom_point(aes(x = NEXT_FIX_X, y = NEXT_FIX_Y),
                 data = df_trial,
                 color = fixation_color,
                 fill = fixation_fill,
                 shape = 21,
                 stroke = fixation_line_width,
                 size = fixation_diameter) +
      scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) +
      scale_y_reverse(limits = c(1080, 0), expand = c(0, 0)) +
      ggtitle(paste("Trial:", image_num))
    
    ggsave(filename = paste0(plots_path, image_num, ".png"), plot = p, limitsize = FALSE, width = 680 / 40, height = 420 / 40)
  }
}

image_list <- c(9546,
                567,
                84637,
                46729,
                5698,
                782,
                43926,
                67925,
                3849,
                6274,
                92648,
                9452,
                574,
                9246,
                9568,
                79236,
                295763,
                34267,
                279,
                879524,
                7352,
                45837,
                38624,
                824796,
                56478,
                6932,
                25934,
                35478,
                643978,
                458,
                8453,
                479,
                92743,
                214,
                546389,
                3982,
                73285,
                87294,
                236475,
                2653,
                849356,
                8759,
                37654,
                67458,
                54876,
                6948,
                84795,
                8269,
                8372,
                93286,
                4938,
                2458,
                376542,
                79243,
                3976,
                697285,
                7329,458, 42836, 9546, 973248) # one of each len
visualize_trial(saccades, image_list)



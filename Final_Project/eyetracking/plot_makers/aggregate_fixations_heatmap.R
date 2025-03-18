# Function to visualize fixation density heatmap per condition

library(ggplot2)
library(dplyr)

if (!dir.exists("plots/heatmaps")) {
  dir.create("plots/heatmaps", recursive = TRUE)
}

visualize_fixation_heatmap <- function(df, image_list,weight_variable_name) {
  
  for (image_num in image_list) {
    df_filtered <- df |> filter(length_stimulus == nchar(image_num))
    
    if (nrow(df_filtered) == 0){
      print("No trials matching ", nchar(image_num),", skipping...")
      next
    } 
    
    img_path <- paste0("images/", image_num, ".png")
    if (!file.exists(img_path)){
      print(image_num, ".png does not exist, skipping...")
      next
    } 
    
    img <- image_read(img_path)
    
    for (fixation_idx in unique(df_filtered$index_in_trial)) {
      df_fixation <- df_filtered |> filter(index_in_trial == fixation_idx)
      
      if (nrow(df_fixation) < 3) next # not enough fixations 
      
      # sd_dur <- sd(df_fixation$duration)
      # df_fixation$duration <- df_fixation$duration/sd_dur #geom_point already takes care of this
      
      heatmap_plot <- ggplot(df_fixation, aes(x = x, y = y)) +
        
        annotation_raster(img, xmin = 0, xmax = 1920, ymin = 0, ymax = 1080) + # background  
        stat_density2d(geom = "polygon", aes(fill = ..level..), alpha = 0.3,) +
        scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0.1, 0.9)) +
        
        geom_point(aes(size = weight), shape = 21, color = "black", fill = NA, stroke = 0.15)+ # interchangeable factor
        
        scale_x_continuous(limits = c(0, 1920), expand = c(0, 0)) + 
        scale_y_continuous(limits = c(0, 1080), expand = c(0, 0)) + 
        coord_fixed() + 
        ggtitle(paste0("Fixation Index: ", unique(df_fixation$index_in_trial)[1], ", Weighted by: ", weight_variable_name))
      
      ggsave(filename = paste0("plots/heatmaps/", image_num, "_fixation_", fixation_idx, "_weight_by_",weight_variable_name,".png"),
             plot = heatmap_plot, width = 680/40, height = 420/40, units = "in")
    }
  }
}

setwd(getSrcDirectory(function(){})[1])
setwd('..')
load("Process_Data/rdata/fixations.RData")
setwd(getSrcDirectory(function(){})[1])

# mutate fixations to match function
fixations_n <- fixations |> 
  mutate(
    length_stimulus = as.numeric(length_stimulus),
    x = as.numeric(x),
    y = 1080 - as.numeric(y), #flip the y axis
    pupil_size = as.numeric(pupil_size),
    duration = as.numeric(duration),
    dist_nearest_iarea = as.numeric(dist_nearest_iarea),
    start_t = as.numeric(start_t),
    end_t = as.numeric(end_t)
  ) 

# limit the maximal number of fixations in a trial (most don't have above 5)
fixations_n <- fixations_n[fixations_n$length_stimulus>0,]
fixations_n <- fixations_n[fixations_n$index_in_trial<6,]
fixations_n <- fixations_n[fixations_n$index_in_trial>1,] #FIX 1 IS FIX ON CROSS

fixations_n <- fixations_n[fixations_n$type_stimulus=='passive',] #only passive viewing

fixations_n <- fixations_n |> mutate(
  weight = pupil_size) # for no weighting make all weights identical with some number value, also use: duration, pupil_size

image_list <- c(458, 42836, 9546, 973248) # choose example images

visualize_fixation_heatmap(fixations_n, image_list,"pupilS")
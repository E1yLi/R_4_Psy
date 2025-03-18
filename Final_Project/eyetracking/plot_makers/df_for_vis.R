library(dplyr)
library(readxl)
setwd(getSrcDirectory(function(){})[1])
setwd('..')
saccades <- read_excel("saccades.xlsx")
saccades <- saccades |> 
  filter(ID %in% c("A", "B"))
trials <- read_excel("conditions.xlsx")

saccades <- saccades |> 
  filter(NAME != "UNDEFINED") |> 
  mutate(NAME = gsub("\\.0$", "", NAME)) |> 
  rowwise() |> 
  mutate(nNum = trials$nNum[which(trials$index == NAME)[1]],
         IMAGE_PATH = ifelse(!is.na(nNum),
                             paste0("images/", nNum, ".png"),
                             paste0("images/", trials$attCheck[which(trials$index == NAME)[1]], ".png"))) |> 
  ungroup()

saccades <- saccades |> 
  mutate_at(vars(CURRENT_SAC_START_X, CURRENT_SAC_START_Y, 
                 CURRENT_SAC_END_X, CURRENT_SAC_END_Y, 
                 NEXT_FIX_X, NEXT_FIX_Y), as.numeric)

save(saccades, file = "visualizer/saccades.RData")

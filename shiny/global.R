library(dplyr)
library(data.table)
library(DT)
library(FactoMineR)
library(FNN)
library(ggplot2)
library(gtools)
library(MASS)
library(plotly)
library(stringr)
library(tidyr)
library(tools)
library(vegan)

library(shiny)
library(shinyWidgets)
library(shinydashboard)

#library(extrafont)
#loadfonts(device='win')

source('..\\r\\pitch_funcs.R')

# Collect and organize data----
pitch_abbrvs <- c('changeup'='CH', 'curveball'='CU', 'cutter'='FC', 'fastball2'='FT', 'fastball4'='FF', 'sinker'='SI', 'slider'='SL', 'splitter'='SF')

pitches <- data.frame()
for(pitch in c('changeup', 'curveball', 'cutter', 'fastball2', 'fastball4', 'sinker', 'slider', 'splitter')){
  df <- fread(paste0('..\\data\\', pitch, '.csv'))
  df$pitch <- pitch_abbrvs[pitch]
  pitches <- smartbind(pitches, df)
  rm(df, pitch)
}
rownames(pitches) <- seq(1:nrow(pitches))
pitches <- pitches %>% select('player_name', 'pitch', 'spin_rate', 'velocity', 'pitch_percent')

pitches_wide <- pitches %>% select(player_name, pitch, spin_rate, velocity)
pitches_wide <- as.data.frame(dcast(setDT(pitches_wide), player_name~pitch, value.var=c('velocity', 'spin_rate')))

# Collect pitch mix combinations
pitch_types_list <- sort(unname(pitch_abbrvs))
pitch_combinations_nums <- sort(unique(unname(apply(table(pitches$player_name, pitches$pitch), 1, function(x){paste(x, collapse = '')}))))
pitch_combinations_chars <- c()
for(combo in pitch_combinations_nums){
  pitch_combinations_chars <- c(pitch_combinations_chars, pitchNumsToChars(pitch_combination_num = combo, pitch_types_list = pitch_types_list))
  rm(combo)
}
rm(pitch_combinations_nums)

# Prepare plot disclaimer text
disclaimer_text <- '* <i>Dimensions 1</i> and <i>2</i> should be interpreted only as an approximation of distances between players.<br>** With the slider at -1, the nearest-neighbor algorithm will place twice the emphasis on velocity as on spin rate. With the slider at 1, the reverse is true, and at the default of 0, the metrics are considered equally.'
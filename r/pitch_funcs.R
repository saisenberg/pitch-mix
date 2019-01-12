# Collect pitch mix combinations
pitchNumsToChars <- function(pitch_combination_num, pitch_types_list){
  pitches_in_combination <- c()
  for(index in 1:nchar(pitch_combination_num)){
    if(strsplit(pitch_combination_num, '')[[1]][index] == 1){
      pitches_in_combination <- c(pitches_in_combination, pitch_types_list[index])
    }
  }
  pitches_in_combination <- paste(pitches_in_combination, collapse = '-')
  return(pitches_in_combination)
}


# For any pitcher, return his top N most-thrown pitches
pitcherTopPitches <- function(player_name, n_pitches, metrics=FALSE){
  df <- pitches[pitches$player_name == player_name,]
  df <- arrange(df, -pitch_percent)
  df <- head(df, min(nrow(df), n_pitches))
  pitch_list <- df$pitch
  if(metrics==FALSE){
    return(pitch_list)
  } else {
    spin_rate_list <- df$spin_rate
    velocity_list <- df$velocity
    metrics_df <- cbind.data.frame(pitch_list, spin_rate_list, velocity_list)
    return(metrics_df)
  }
}

# For any pitcher, return metrics of specific pitches
pitcherMetricsFromPitchList <- function(player_name, pitch_list){
  df <- pitches[(pitches$player_name == player_name) & (pitches$pitch %in% pitch_list),]
  df <- arrange(df, pitch) %>% select(pitch, spin_rate, velocity)
  return(df)
}

# Prepare dataframe with specific pitches (input list CAN be pitcherTopPitches output)
prepClustering <- function(pitch_list, scale=TRUE, scramble=FALSE, spin_ratio_importance=1){
  
  # Subset data to relevant pitches
  return_indices <- which(names(pitches_wide)=='player_name')
  for(pitch in pitch_list){
    add_indices <- grep(pattern = pitch, names(pitches_wide))
    return_indices <- c(return_indices, add_indices)
  }
  df <- pitches_wide[,c(return_indices)]
  df <- df[complete.cases(df),]
  rm(add_indices, pitch)
  
  # Scale data, if necessary
  if(scale & nrow(df)>1){
    df <- cbind(df$player_name, as.data.frame(apply(df[,c(2:ncol(df))], 2, scale)))
  }
  names(df)[1] <- 'player_name'
  df$player_name <- as.character(df$player_name)
  
  # Scramble data to avoid duplicates, if necessary
  if(scramble == TRUE){
    set.seed(1)
    df[,c(2:ncol(df))] <- apply(df[,c(2:ncol(df))], 2, function(x){x + runif(n = length(x), min = -1/1e4, max = 1/1e4)})
  }
  
  # Place lighter/heavier/equal emphasis on spin rate
  for(col in names(df)[2:ncol(df)]){
    if(substr(col, 1, 4)=='spin'){
      df[which(names(df)==col)] = spin_ratio_importance * df[which(names(df)==col)]
    }
  }
  
  return(df)
}

# Find K nearest neighbors (input df from prepClustering)
nearestNeighbors <- function(df, k=3){
  if(nrow(df)<2){
    return(NULL)
  }
  
  
  knn <- get.knn(df[,c(2:ncol(df))], k=k, algorithm='brute')
  ind <- as.data.frame(knn$nn.index)
  dist <- as.data.frame(knn$nn.dist)
  
  ind$player_name <- df$player_name
  
  # Distance colnames
  for(col in 1:ncol(dist)){
    names(dist)[col] <- str_replace(string = names(dist)[col], pattern = 'V', replacement = 'dist')
  }
  
  # Collect neighbor names
  neighbors_list <- list()
  for(neighbor_num in 1:k){
    neighbor_vec <- as.character(ind$player_name[ind[,c(neighbor_num)]])
    neighbors_list[[neighbor_num]] <- neighbor_vec
  }
  df <- as.data.frame(do.call(cbind, neighbors_list))
  df <- cbind(ind$player_name, df, dist[,c(1:k)])
  
  # Neighbor colnames
  names(df)[1] <- 'player_name'
  for(col in 1:ncol(df)){
    names(df)[col] <- str_replace(string = names(df)[col], pattern = 'V', replacement = 'neighbor')
  }
  
  # Convert player names to characters
  df$player_name <- as.character(df$player_name)
  neighbor_indices <- grep(pattern = 'neighbor', x = names(df))
  if(k>1){
    df[,c(neighbor_indices)] <- apply(df[,c(neighbor_indices)], 2, function(x){as.character(x)})
  } else {
    df[,c(neighbor_indices)] <- as.character(df[,c(neighbor_indices)])
    names(df)[3] <- 'dist1'
  }
  
  return(df)
}

# Create MDS dataframe (input df from prepClustering)
createMDS <- function(df, nonmetric=TRUE){
  player_name_index <- which(names(df)=='player_name')
  player_name <- as.vector(df[,c(player_name_index)])
  df <- df[,c(2:ncol(df))]
  
  if(nonmetric==FALSE | ncol(df) == 2){
    mds_df <- cbind(player_name, as.data.frame(cmdscale(dist(df), eig=FALSE, k=2)))
  } else {
    mds_df <- cbind(player_name, as.data.frame(metaMDS(dist(df), k=2, try = 10, trymax = 20)$points))
    
    for(col in 1:ncol(mds_df)){
      names(mds_df)[col] <- str_replace(string = names(mds_df)[col], pattern = 'MDS', replacement = 'V')
    }
  }
  
  mds_df$player_name <- as.character(mds_df$player_name)
  return(mds_df)
}

# Create MDS dataframe based on a pitcher's top pitches
createMDSFromPitcher <- function(player_name, n_pitches, scale=TRUE, nonmetric=TRUE, spin_ratio_importance=1){
  pitch_list <- pitcherTopPitches(player_name = player_name, n_pitches = n_pitches, metrics = FALSE)
  if(n_pitches > 1){
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = FALSE, spin_ratio_importance = spin_ratio_importance)
  } else {
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = TRUE, spin_ratio_importance = spin_ratio_importance)
  }
  mds_df <- createMDS(df = clustering_df, nonmetric = nonmetric)
  mds_df$player_name <- as.character(mds_df$player_name)
  return(mds_df)
}

# Create MDS dataframe based on a pitcher's top pitches
createMDSFromPitchList <- function(pitch_list, scale=TRUE, nonmetric=TRUE, spin_ratio_importance=1){
  if(length(pitch_list) > 1){
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = FALSE, spin_ratio_importance=spin_ratio_importance)
  } else {
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = TRUE, spin_ratio_importance=spin_ratio_importance)
  }
  mds_df <- createMDS(df = clustering_df, nonmetric = nonmetric)
  mds_df$player_name <- as.character(mds_df$player_name)
  return(mds_df)
}

# Create MDS dataframe (input df from prepClustering)
createPCA <- function(df, pct_variance, force_num_PC=NULL){
  if(pct_variance >= 1 | pct_variance <= 0){
    return('Please choose a pct_variance between 0 and 1, non-inclusive')
  }
  
  pca_obj <- PCA(df[,c(2:ncol(df))], graph=FALSE)
  vars_exp <- pca_obj$eig[,3]/100
  
  for(num in 2:length(vars_exp)){
    if((vars_exp[num-1] < pct_variance) & (vars_exp[num] >= pct_variance)){
      num_PC <- num
    } else {
      next
    }
  }
  
  if(!is.null(force_num_PC)){
    num_PC <- force_num_PC
  }
  
  pca_df <- cbind.data.frame(df$player_name, pca_obj$ind$coord[,c(1:num_PC)])
  names(pca_df)[1] <- 'player_name'
  return(pca_df)
}

# Create PCA dataframe based on a pitcher's top pitches
createPCAFromPitcher <- function(player_name, n_pitches, pct_variance, force_num_PC=NULL, spin_ratio_importance=1){
  pitch_list <- pitcherTopPitches(player_name = player_name, n_pitches = n_pitches, metrics = FALSE)
  clustering_df <- prepClustering(pitch_list = pitch_list, scale = TRUE, scramble = FALSE, spin_ratio_importance=spin_ratio_importance)
  pca_df <- createPCA(df = clustering_df, pct_variance = pct_variance, force_num_PC = force_num_PC)
  pca_df$player_name <- as.character(pca_df$player_name)
  
  # Dimension colnames
  for(col in 1:ncol(pca_df)){
    names(pca_df)[col] <- str_replace(string = names(pca_df)[col], pattern = 'Dim.', replacement = 'V')
  }
  
  return(pca_df)
}

# Find K nearest neighbors from a pitcher's N top pitches
nearestNeighborsFromPitcher <- function(player_name, n_pitches, k=3, type='NONE', scale=TRUE, nonmetric=TRUE, spin_ratio_importance = 1){
  pitch_list <- pitcherTopPitches(player_name = player_name, n_pitches = n_pitches, metrics = FALSE)
  clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, spin_ratio_importance = spin_ratio_importance)
  
  if(type=='MDS'){
    clustering_df <- createMDS(df = clustering_df, nonmetric = nonmetric)
  } else if(type=='PCA'){
    clustering_df <- createPCA(df = clustering_df, pct_variance = 0.8)
  }
  
  neighbors_df <- nearestNeighbors(clustering_df, min(k, nrow(clustering_df)-1))
  neighbors_names <- as.character(neighbors_df[neighbors_df$player_name==player_name,grep(pattern = 'neighbor', x = names(neighbors_df))])
  neighbors_dists <- as.numeric(neighbors_df[neighbors_df$player_name==player_name, grep(pattern = 'dist', x = names(neighbors_df))])
  neighbors_df <- as.data.frame(cbind.data.frame(neighbors_names, neighbors_dists))
  neighbors_df$neighbors_names <- as.character(neighbors_df$neighbors_names)
  neighbors_df$neighbors_dists <- as.numeric(as.character(neighbors_df$neighbors_dists))
  neighbors_df$neighbors_dists <- as.numeric(neighbors_df$neighbors_dists / (n_pitches*2)) #######
  return(neighbors_df)
}

# Find K nearest neighbors from a list of pitches
nearestNeighborsFromPitchList <- function(pitch_list, k=3, type='NONE', scale=TRUE, nonmetric=TRUE, spin_ratio_importance=1){
  
  # Make sure at least one pitcher throws the input pitch list
  pitches_match_indices <- c()
  for(pitch in pitch_list){
    pitches_match_indices <- c(pitches_match_indices, grep(pattern = pitch, x = pitch_combinations_chars))
  }
  num_matches <- sum(table(pitches_match_indices) == length(pitch_list))
  if(num_matches == 0){
    return(NULL)
  }
  
  clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, spin_ratio_importance=spin_ratio_importance)
  
  if(type=='MDS'){
    clustering_df <- createMDS(df = clustering_df, nonmetric = nonmetric)
  } else if(type=='PCA'){
    clustering_df <- createPCA(df = clustering_df, pct_variance = 0.8)
  }
  
  neighbors_df <- nearestNeighbors(clustering_df, min(k, nrow(clustering_df)-1))
  
  for(col in names(neighbors_df)){
    if(substr(col, 1, 4) == 'dist'){
      neighbors_df[which(names(neighbors_df)==col)] <- neighbors_df[which(names(neighbors_df)==col)] / (length(pitch_list)*2)
    }
  }
  
  return(neighbors_df)
}

# Turn wide nearestNeighbors df to long
nearestNeighborsWideToLong <- function(df){

  neighbors_df <- cbind.data.frame(df$player_name, df[,grep(pattern = 'neighbor', x = names(df))])
  names(neighbors_df)[1] <- 'player_name'
  neighbors_df <- gather(neighbors_df, num, neighbor, neighbor1:names(neighbors_df)[ncol(neighbors_df)])
  
  
  dists_df <- cbind.data.frame(df$player_name, df[,grep(pattern = 'dist', x = names(df))])
  names(dists_df)[1] <- 'player_name'
  dists_df <- gather(dists_df, num, dist, dist1:names(dists_df)[ncol(dists_df)])
  
  full_df <- cbind.data.frame(neighbors_df, dists_df$dist)
  names(full_df) <- c('player_name', 'num', 'neighbors_names', 'neighbors_dists')
  
  full_df$player_name <- as.character(full_df$player_name)
  full_df$num <- as.numeric(str_replace(string = full_df$num, pattern = '[^0-9]+', replacement = ''))
  full_df$neighbors_dists <- sprintf("%.2f", round(full_df$neighbors_dists, 4))
  
  return(full_df)
}
  
  
# Find K nearest neighbors' pitch metrics from a pitcher's N top pitches
neighborMetricsFromPitcher <- function(player_name, n_pitches, k,  type='NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 1){
  pitches_list <- pitcherTopPitches(player_name = player_name, n_pitches = n_pitches, metrics = F)
  neighbors_names <- nearestNeighborsFromPitcher(player_name = player_name, n_pitches = n_pitches, k = k, type = type, scale = scale, nonmetric = nonmetric, spin_ratio_importance = spin_ratio_importance)$neighbors_names
  df <- pitches[(pitches$player_name %in% c(player_name, neighbors_names)) & (pitches$pitch %in% pitches_list), ]
  df <- df[,-c(which(names(df)=='pitch_percent'))]
  rownames(df) <- seq(1:nrow(df))
  return(df)
}

# Find K nearest neighbors' pitch metrics from a pitcher's specific pitch list
neighborMetricsFromPitcherWithPitchList <- function(player_name, pitch_list, k,  type='NONE', scale = TRUE, nonmetric = TRUE, spin_ratio_importance = 1){
  neighbors_names <- nearestNeighborsWideToLong(nearestNeighborsFromPitchList(pitch_list = pitch_list, k = k, spin_ratio_importance = spin_ratio_importance))
  neighbors_names <- neighbors_names[neighbors_names$player_name == player_name, 'neighbors_names']
  df <- pitches[(pitches$player_name %in% c(player_name, neighbors_names)) & (pitches$pitch %in% pitch_list), ]
  df <- df[,-c(which(names(df)=='pitch_percent'))]
  rownames(df) <- seq(1:nrow(df))
  return(df)
}

# Return pitcher's all possible neighbors from a pitcher's N top pitches
allNeighborsFromPitcher <- function(player_name, n_pitches, type='NONE', scale=TRUE, nonmetric=TRUE, spin_ratio_importance=1){
  pitch_list <- pitcherTopPitches(player_name = player_name, n_pitches = n_pitches, metrics = FALSE)
  
  if(n_pitches > 1){
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = FALSE, spin_ratio_importance=spin_ratio_importance)
  } else {
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = TRUE, spin_ratio_importance=spin_ratio_importance)
  }
  
  n_possible_neighbors <- nrow(clustering_df)
  
  # Collect all pitcher distances, including requested pitcher (0)
  neighbor_distances <- nearestNeighborsFromPitcher(player_name = player_name, n_pitches = n_pitches, k = n_possible_neighbors, type = type, scale = scale, nonmetric = nonmetric, spin_ratio_importance = spin_ratio_importance)
  pitcher_row <- as.data.frame(t(c(player_name, 0)))
  colnames(pitcher_row) <- colnames(neighbor_distances)
  pitcher_row$neighbors_names <- as.character(pitcher_row$neighbors_names)
  pitcher_row$neighbors_dists <- as.numeric(as.character(pitcher_row$neighbors_dists))
  neighbor_distances <- bind_rows(neighbor_distances, pitcher_row)
  
  # Collect all pitcher metrics, including those of requested pitcher
  neighbor_metrics <- neighborMetricsFromPitcher(player_name = player_name, n_pitches = n_pitches, k = n_possible_neighbors, type = type, scale = scale, nonmetric = nonmetric, spin_ratio_importance = spin_ratio_importance)
  neighbor_metrics <- as.data.frame(dcast(setDT(neighbor_metrics), player_name~pitch, value.var=c('velocity', 'spin_rate')))
  
  df <- left_join(neighbor_distances, neighbor_metrics, by=c('neighbors_names' = 'player_name')) %>% 
    arrange(neighbors_dists)
  df$neighbors_dists <- sprintf('%.2f', round(df$neighbors_dists, 4))
  df$Rank <- as.numeric(rownames(df))-1
  
  # Format for shiny output
  df[1,'neighbors_dists'] <- NA
  df[1,'Rank'] <- NA
  df <- df %>% rename(Player = neighbors_names, 'Dist.' = neighbors_dists)
  df <- df[,c(ncol(df), 1:ncol(df)-1)]
  for(num in 1:length(names(df))){
    colname <- names(df)[num]
    if(grepl('velocity_', colname)){
      df[,c(num)] <- round(df[,c(num)], 2)
      new_name <- paste0(substr(colname, nchar(colname)-1, nchar(colname)), ' MPH')
    } else if(grepl('spin_rate_', colname)){
      new_name <- paste0(substr(colname, nchar(colname)-1, nchar(colname)), ' RPM')
    } else {
      new_name <- colname
    }
    names(df)[num] <- new_name
    rm(colname, new_name, num)
  }
  
  df <- df[,c(1:3, order(names(df[,c(4:ncol(df))]))+3)]
  
  return(df)
}


# Return pitcher's all possible neighbors from a list of pitches
allNeighborsFromPitcherWithPitchList <- function(player_name, pitch_list, type='NONE', scale=TRUE, nonmetric=TRUE, spin_ratio_importance=1){
  
  if(length(pitch_list) > 1){
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = FALSE, spin_ratio_importance=spin_ratio_importance)
  } else {
    clustering_df <- prepClustering(pitch_list = pitch_list, scale = scale, scramble = TRUE, spin_ratio_importance=spin_ratio_importance)
  }
  
  n_possible_neighbors <- nrow(clustering_df)
  
  # Collect all pitcher distances, including requested pitcher (0)
  neighbor_distances <- nearestNeighborsWideToLong(nearestNeighborsFromPitchList(pitch_list = pitch_list, k = n_possible_neighbors, type = type, scale = scale, nonmetric = nonmetric, spin_ratio_importance = spin_ratio_importance)) 
  neighbor_distances <- neighbor_distances[neighbor_distances$player_name == player_name, c('neighbors_names', 'neighbors_dists')]
  pitcher_row <- as.data.frame(t(c(player_name, '0.00')))
  colnames(pitcher_row) <- colnames(neighbor_distances)
  pitcher_row$neighbors_names <- as.character(pitcher_row$neighbors_names)
  pitcher_row$neighbors_dists <- as.numeric(as.character(pitcher_row$neighbors_dists))
  neighbor_distances <- rbind(pitcher_row, neighbor_distances)
  
  # Collect all pitcher metrics, including those of requested pitcher
  neighbor_metrics <- neighborMetricsFromPitcherWithPitchList(player_name = player_name, pitch_list = pitch_list, k = n_possible_neighbors, type = type, scale = scale, nonmetric = nonmetric, spin_ratio_importance = spin_ratio_importance)
  neighbor_metrics <- as.data.frame(dcast(setDT(neighbor_metrics), player_name~pitch, value.var=c('velocity', 'spin_rate')))
  
  df <- left_join(neighbor_distances, neighbor_metrics, by=c('neighbors_names' = 'player_name')) 
  df$neighbors_dists <- as.numeric(as.character(df$neighbors_dists))
  df <- df %>% arrange(neighbors_dists)
  df$neighbors_dists <- sprintf("%.2f", df$neighbors_dists)
  df$Rank <- as.numeric(rownames(df))-1
  
  # Format for shiny output
  df[1,'neighbors_dists'] <- NA
  df[1,'Rank'] <- NA
  df <- df %>% rename(Player = neighbors_names, 'Dist.' = neighbors_dists)
  df <- df[,c(ncol(df), 1:ncol(df)-1)]
  for(num in 1:length(names(df))){
    colname <- names(df)[num]
    if(grepl('velocity_', colname)){
      df[,c(num)] <- round(df[,c(num)], 2)
      new_name <- paste0(substr(colname, nchar(colname)-1, nchar(colname)), ' MPH')
    } else if(grepl('spin_rate_', colname)){
      new_name <- paste0(substr(colname, nchar(colname)-1, nchar(colname)), ' RPM')
    } else {
      new_name <- colname
    }
    names(df)[num] <- new_name
    rm(colname, new_name, num)
  }
  df <- df[,c(1:3, order(names(df[,c(4:ncol(df))]))+3)]
  
  return(df)
}

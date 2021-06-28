############################################################################################
# Can we predict people if will choose a certain option out of a set?
# written by Dawn Holford
# updated 14 Apr 2021

# Script to add classifications to dataset: whether it is the best option according to different criteria. 
# Includes variables about the choice sets: how many of the options would be the best choice, how far are they from the next best option by the criteria?

#---- set up workspace -----
rm(list=ls())

packages <- c("tidyverse")
purrr::map(packages, library, character.only = TRUE)

seed <- 123

setwd('..')
getwd()

data <- read_csv("data/choice_dataset.csv") %>%
  filter(trialcode == "choice")  # filter out non-choice data

# ------ make extraction function -------
extract_strat <- function(data, stratvar, min = 0){
  includevars <- c()
  
  for (i in 1:6){
    varname <- paste0("choice",i,"_",stratvar)
    includevars <- cbind(includevars, varname)
  }
  
  strat <- data %>% dplyr::select(!!includevars[1], !!includevars[2], !!includevars[3], !!includevars[4], !!includevars[5], !!includevars[6])
  
  best <- tibble(best1 = character(), best2 = character(),
                 best3 = character(), best4 = character(),
                 best5 = character(), best6 = character())
  if(min == 0){
    
    for(i in 1:nrow(strat)){
      repcols <- rep(colnames(strat)[which(strat[i,] == min(strat[i, 1:6]))], times = 6)
      stratbest <- tibble(best1 = substr(repcols[1], start = 1, stop = 7),
                          best2 = substr(repcols[2], start = 1, stop = 7),
                          best3 = substr(repcols[3], start = 1, stop = 7),
                          best4 = substr(repcols[4], start = 1, stop = 7),
                          best5 = substr(repcols[5], start = 1, stop = 7),
                          best6 = substr(repcols[6], start = 1, stop = 7))
      best <- bind_rows(best, stratbest)
    }
  } else if (min == 1) {
    
    for(i in 1:nrow(strat)){
      repcols <- rep(colnames(strat)[which(strat[i,] == max(strat[i, 1:6]))], times = 6)
      stratbest <- tibble(best1 = substr(repcols[1], start = 1, stop = 7),
                          best2 = substr(repcols[2], start = 1, stop = 7),
                          best3 = substr(repcols[3], start = 1, stop = 7),
                          best4 = substr(repcols[4], start = 1, stop = 7),
                          best5 = substr(repcols[5], start = 1, stop = 7),
                          best6 = substr(repcols[6], start = 1, stop = 7))
      best <- bind_rows(best, stratbest)
    }
  }
  return(best)
}

# ------ run extraction function to get the best choices under a number of metrics ------
eqw4 <- extract_strat(data, "wadd_ri4")
eqw5 <- extract_strat(data,"wadd_ri5")
expert <- extract_strat(data, "nutri_rating", 1)
tfl3 <- extract_strat(data, "wadd_tfl3")
tfl4 <- extract_strat(data, "wadd_tfl4")
ri_sugar <- extract_strat(data, "sugar_ri")
ri_fat <- extract_strat(data, "fat_ri")
ri_energy <- extract_strat(data, "energy_ri")
ri_salt <- extract_strat(data, "salt_ri")
ri_satfat <- extract_strat(data, "satfat_ri")
tfl_sugar <- extract_strat(data, "sugar_tfl")
tfl_fat <- extract_strat(data, "fat_tfl")
tfl_salt <- extract_strat(data,"salt_tfl")
tfl_satfat <- extract_strat(data,"satfat_tfl")

# ----- compare choice data with extracted strategies -----
strategies <- data

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% eqw4[i,]){
    strategies$is_eqw4[i] <- 1
  } else {strategies$is_eqw4[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% eqw5[i,]){
    strategies$is_eqw5[i] <- 1
  } else {strategies$is_eqw5[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% expert[i,]){
    strategies$is_expert[i] <- 1
  } else {strategies$is_expert[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl3[i,]){
    strategies$is_tfl3[i] <- 1
  } else {strategies$is_tfl3[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl4[i,]){
    strategies$is_tfl4[i] <- 1
  } else {strategies$is_tfl4[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% ri_sugar[i,]){
    strategies$is_sugarri[i] <- 1
  } else {strategies$is_sugarri[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% ri_fat[i,]){
    strategies$is_fatri[i] <- 1
  } else {strategies$is_fatri[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% ri_energy[i,]){
    strategies$is_energyri[i] <- 1
  } else {strategies$is_energyri[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% ri_salt[i,]){
    strategies$is_saltri[i] <- 1
  } else {strategies$is_saltri[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% ri_satfat[i,]){
    strategies$is_satfatri[i] <- 1
  } else {strategies$is_satfatri[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl_sugar[i,]){
    strategies$is_sugartfl[i] <- 1
  } else {strategies$is_sugartfl[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl_fat[i,]){
    strategies$is_fattfl[i] <- 1
  } else {strategies$is_fattfl[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl_salt[i,]){
    strategies$is_salttfl[i] <- 1
  } else {strategies$is_salttfl[i] <-  0}
}

for (i in 1:nrow(strategies)){
  if(strategies$response[i] %in% tfl_satfat[i,]){
    strategies$is_satfattfl[i] <- 1
  } else {strategies$is_satfattfl[i] <-  0}
}

strategies <- strategies %>%
  mutate(sum_allstrat = is_eqw4 + is_eqw5 + is_tfl3 + is_tfl4 + is_expert + is_sugarri + is_fatri + is_energyri + is_saltri +
           is_satfatri + is_sugartfl + is_fattfl + is_salttfl + is_satfattfl,
         sum_visstrat = is_eqw4 + is_tfl3 + is_sugarri + is_fatri + is_energyri + is_saltri +
           is_sugartfl + is_fattfl + is_salttfl)

summary(strategies$sum_allstrat) # they all correspond to at least 2 (suggests some overlap)
summary(strategies$sum_visstrat) # there are some single-strategy choices

# ------- calculate number of options that are equally the best choice under each criteria  -------
for(i in 1:nrow(eqw4)){
  eqw4$n_best[i] <- length(unique(t(eqw4[i,1:6])))
}

for(i in 1:nrow(eqw5)){
  eqw5$n_best[i] <- length(unique(t(eqw5[i, 1:6])))
}

for(i in 1:nrow(expert)){
  expert$n_best[i] <- length(unique(t(expert[i, 1:6])))
}

for (i in 1:nrow(tfl3)){
  tfl3$n_best[i] <- length(unique(t(tfl3[i, 1:6])))
}

for(i in 1:nrow(tfl4)){
  tfl4$n_best[i] <- length(unique(t(tfl4[i, 1:6])))
}

for(i in 1:nrow(ri_sugar)){
  ri_sugar$n_best[i] <- length(unique(t(ri_sugar[i, 1:6])))
}

for(i in 1:nrow(ri_fat)){
  ri_fat$n_best[i] <- length(unique(t(ri_fat[i,1:6])))
}

for(i in 1:nrow(ri_energy)){
  ri_energy$n_best[i] <- length(unique(t(ri_energy[i, 1:6])))
}

for(i in 1:nrow(ri_salt)){
  ri_salt$n_best[i] <- length(unique(t(ri_salt[i, 1:6])))
}

for(i in 1:nrow(ri_satfat)){
  ri_satfat$n_best[i] <- length(unique(t(ri_satfat[i, 1:6])))
}

for(i in 1:nrow(tfl_sugar)){
  tfl_sugar$n_best[i] <- length(unique(t(tfl_sugar[i, 1:6])))
}

for(i in 1:nrow(tfl_fat)){
  tfl_fat$n_best[i] <- length(unique(t(tfl_fat[i, 1:6])))
}

for(i in 1:nrow(tfl_salt)){
  tfl_salt$n_best[i] <- length(unique(t(tfl_salt[i, 1:6])))
}

for(i in 1:nrow(tfl_satfat)){
  tfl_satfat$n_best[i] <- length(unique(t(tfl_satfat[i, 1:6])))
}

# run a random check to see if it looks fine
x <-sample(1:nrow(strategies), 1)
tfl_sugar[x,]
x <-sample(1:nrow(strategies), 1)
ri_fat[x,]
x <-sample(1:nrow(strategies), 1)
expert[x,]
x <-sample(1:nrow(strategies), 1)
ri_salt[x,]

# add the above to strategies dataset
for(i in 1:nrow(strategies)){
  strategies$eqw4_nbest[i] <- eqw4$n_best[i]
  strategies$eqw5_nbest[i] <- eqw5$n_best[i]
  strategies$expert_nbest[i] <- expert$n_best[i]
  strategies$tfl3_nbest[i] <- tfl3$n_best[i]
  strategies$tfl4_nbest[i] <- tfl4$n_best[i]
  strategies$ri_sugar_nbest[i] <- ri_sugar$n_best[i]
  strategies$ri_fat_nbest[i] <- ri_fat$n_best[i]
  strategies$ri_energy_nbest[i] <- ri_energy$n_best[i]
  strategies$ri_salt_nbest[i] <- ri_salt$n_best[i]
  strategies$ri_satfat_nbest[i] <- ri_satfat$n_best[i]
  strategies$tfl_sugar_nbest[i] <- tfl_sugar$n_best[i]
  strategies$tfl_fat_nbest[i] <- tfl_fat$n_best[i]
  strategies$tfl_salt_nbest[i] <- tfl_salt$n_best[i]
  strategies$tfl_satfat_nbest[i] <- tfl_satfat$n_best[i]
}

# --------- calculate distance factors of best choices from others ------
exp_dist <- strategies %>%
  dplyr::select(contains("nutri_rating")) %>%
  dplyr::select(-choice_nutri_rating)

for(i in 1:nrow(exp_dist)){
  values <- sort(unique(t(exp_dist[i,1:6])))
  exp_dist$nutri_distance[i] <- max(values)-nth(values, length(values)-1)
}

exp_dist$nutri_distance[is.na(exp_dist$nutri_distance)] <- 0

eqw4_dist <- strategies %>%
  dplyr::select(contains("wadd_ri4")) %>%
  dplyr::select(-choice_wadd_ri4)

for(i in 1:nrow(eqw4_dist)){
  values <- sort(unique(t(eqw4_dist[i,1:6])))
  eqw4_dist$eqw4_distance[i] <- nth(values, 2)-min(values)
}

eqw5_dist <- strategies %>%
  dplyr::select(contains("wadd_ri5")) %>%
  dplyr::select(-choice_wadd_ri5)

for(i in 1:nrow(eqw5_dist)){
  values <- sort(unique(t(eqw5_dist[i,1:6])))
  eqw5_dist$eqw5_distance[i] <- nth(values, 2)-min(values)
}

tfl3_dist <- strategies %>%
  dplyr::select(contains("wadd_tfl3")) %>%
  dplyr::select(-choice_wadd_tfl3)

for(i in 1:nrow(tfl3_dist)){
  values <- sort(unique(t(tfl3_dist[i,1:6])))
  tfl3_dist$tfl3_distance[i] <- nth(values, 2)-min(values)
}

tfl3_dist$tfl3_distance[is.na(tfl3_dist$tfl3_distance)] <- 0

tfl4_dist <- strategies %>%
  dplyr::select(contains("wadd_tfl4")) %>%
  dplyr::select(-choice_wadd_tfl4)

for(i in 1:nrow(tfl4_dist)){
  values <- sort(unique(t(tfl4_dist[i,1:6])))
  tfl4_dist$tfl4_distance[i] <- nth(values, 2)-min(values)
}

tfl4_dist$tfl4_distance[is.na(tfl4_dist$tfl4_distance)] <- 0

ri_sugar_dist <- strategies %>%
  dplyr::select(contains("sugar_ri")) %>%
  dplyr::select(-choice_sugar_ri)

for(i in 1:nrow(ri_sugar_dist)){
  values <- sort(unique(t(ri_sugar_dist[i,1:6])))
  ri_sugar_dist$ri_sugar_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(ri_sugar_dist$ri_sugar_distance))

ri_fat_dist <- strategies %>%
  dplyr::select(contains("fat_ri")) %>%
  dplyr::select(-choice_fat_ri)

for(i in 1:nrow(ri_fat_dist)){
  values <- sort(unique(t(ri_fat_dist[i,1:6])))
  ri_fat_dist$ri_fat_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(ri_fat_dist$ri_fat_distance))

ri_energy_dist <- strategies %>%
  dplyr::select(contains("energy_ri")) %>%
  dplyr::select(-choice_energy_ri)

for(i in 1:nrow(ri_energy_dist)){
  values <- sort(unique(t(ri_energy_dist[i,1:6])))
  ri_energy_dist$ri_energy_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(ri_energy_dist$ri_energy_distance))

ri_salt_dist <- strategies %>%
  dplyr::select(contains("salt_ri")) %>%
  dplyr::select(-choice_salt_ri)

for(i in 1:nrow(ri_salt_dist)){
  values <- sort(unique(t(ri_salt_dist[i,1:6])))
  ri_salt_dist$ri_salt_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(ri_salt_dist$ri_salt_distance))

ri_satfat_dist <- strategies %>%
  dplyr::select(contains("satfat_ri")) %>%
  dplyr::select(-choice_satfat_ri)

for(i in 1:nrow(ri_satfat_dist)){
  values <- sort(unique(t(ri_satfat_dist[i,1:6])))
  ri_satfat_dist$ri_satfat_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(ri_satfat_dist$ri_satfat_distance))

tfl_sugar_dist <- strategies %>%
  dplyr::select(contains("sugar_tfl")) %>%
  dplyr::select(-choice_sugar_tfl)

for(i in 1:nrow(tfl_sugar_dist)){
  values <- sort(unique(t(tfl_sugar_dist[i,1:6])))
  tfl_sugar_dist$tfl_sugar_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(tfl_sugar_dist$tfl_sugar_distance))
tfl_sugar_dist$tfl_sugar_distance[is.na(tfl_sugar_dist$tfl_sugar_distance)] <- 0


tfl_fat_dist <- strategies %>%
  dplyr::select(contains("fat_tfl")) %>%
  dplyr::select(-choice_fat_tfl)

for(i in 1:nrow(tfl_fat_dist)){
  values <- sort(unique(t(tfl_fat_dist[i,1:6])))
  tfl_fat_dist$tfl_fat_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(tfl_fat_dist$tfl_fat_distance))
tfl_fat_dist$tfl_fat_distance[is.na(tfl_fat_dist$tfl_fat_distance)] <- 0


tfl_salt_dist <- strategies %>%
  dplyr::select(contains("salt_tfl")) %>%
  dplyr::select(-choice_salt_tfl)

for(i in 1:nrow(tfl_salt_dist)){
  values <- sort(unique(t(tfl_salt_dist[i,1:6])))
  tfl_salt_dist$tfl_salt_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(tfl_salt_dist$tfl_salt_distance))
tfl_salt_dist$tfl_salt_distance[is.na(tfl_salt_dist$tfl_salt_distance)] <- 0

tfl_satfat_dist <- strategies %>%
  dplyr::select(contains("satfat_tfl")) %>%
  dplyr::select(-choice_satfat_tfl)

for(i in 1:nrow(tfl_satfat_dist)){
  values <- sort(unique(t(tfl_satfat_dist[i,1:6])))
  tfl_satfat_dist$tfl_satfat_distance[i] <- nth(values, 2)-min(values)
}

sum(is.na(tfl_satfat_dist$tfl_satfat_distance))
tfl_satfat_dist$tfl_satfat_distance[is.na(tfl_satfat_dist$tfl_satfat_distance)] <- 0

# add to strategies
for (i in 1:nrow(strategies)){
  strategies$nutri_distance[i] <- exp_dist$nutri_distance[i]
  strategies$eqw4_distance[i] <- eqw4_dist$eqw4_distance[i]
  strategies$eqw5_distance[i] <- eqw5_dist$eqw5_distance[i]
  strategies$tfl3_distance[i] <- tfl3_dist$tfl3_distance[i]
  strategies$tfl4_distance[i] <- tfl4_dist$tfl4_distance[i]
  strategies$ri_sugar_distance[i] <- ri_sugar_dist$ri_sugar_distance[i]
  strategies$ri_fat_distance[i] <- ri_fat_dist$ri_fat_distance[i]
  strategies$ri_energy_distance[i] <- ri_energy_dist$ri_energy_distance[i]
  strategies$ri_salt_distance[i] <- ri_salt_dist$ri_salt_distance[i]
  strategies$ri_satfat_distance[i] <- ri_satfat_dist$ri_satfat_distance[i]
  strategies$tfl_sugar_distance[i] <- tfl_sugar_dist$tfl_sugar_distance[i]
  strategies$tfl_fat_distance[i] <- tfl_fat_dist$tfl_fat_distance[i]
  strategies$tfl_salt_distance[i] <- tfl_salt_dist$tfl_salt_distance[i]
  strategies$tfl_satfat_distance[i] <- tfl_satfat_dist$tfl_satfat_distance[i]
}


write_csv(strategies, "data/data_with_strategies.csv")

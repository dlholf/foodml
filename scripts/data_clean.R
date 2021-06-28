############################################################################################
# Can we predict if people will choose a certain option out of a set?
# written by Dawn Holford
# updated 14 Apr 2021 
# This script cleans and sets up the data.

#---- set up workspace -----
rm(list=ls())

packages <- c("tidyverse")
purrr::map(packages, library, character.only = TRUE)

seed <- 123

setwd('..')
getwd()

# ------- import and clean data to make a full dataset --------

# all stimuli values
food_list <- read_csv("data/Food.csv") %>%
  mutate(health = ifelse(Nutri_Rating == 1.5, "Poor",                # make a categorical variable for nutritionist ratings, since there are only 4 of them, a linear regression model doesn't work
                         ifelse(Nutri_Rating == 2.5, "Mod",
                                ifelse(Nutri_Rating == 3.5, "Good",
                                       ifelse(Nutri_Rating == 4.5, "Vgood", "NA")))),
         energy_p100 = Energy/Serve_size,   # create variables for per 100g nutrition
         fat_p100 = Fat/Serve_size,
         satfat_p100 = Sat_Fat/Serve_size,
         sugar_p100 = Sugar/Serve_size,
         salt_p100 = Salt/Serve_size,
         energy_ri = Energy/2000,          # create variables for % recommended intake (based on NHS recommended total intakes)
         fat_ri = Fat/70,
         satfat_ri = Sat_Fat/20,
         sugar_ri = Sugar/90,
         salt_ri = Salt/6
  ) %>%
  mutate(fat_tfl = ifelse(fat_p100 > 17.5 | Fat > 21, 3, ifelse(fat_p100 <= 3, 1, 2)),    # calculate banding based on https://www.nutrition.org.uk/healthyliving/helpingyoueatwell/324-labels.html?start=3
         satfat_tfl = ifelse(satfat_p100 > 5 | Sat_Fat > 6, 3, ifelse(satfat_p100 <= 1.5, 1, 2)),
         sugar_tfl = ifelse(sugar_p100 > 22.5 | Sugar > 27, 3, ifelse(sugar_p100 <= 5, 1, 2)),
         salt_tfl = ifelse((salt_p100 > 1.5 | Salt > 1.8), 3, ifelse(salt_p100 <= 0.3, 1, 2))
  ) %>%
  mutate(wadd_ri5 = (energy_ri + fat_ri + satfat_ri + sugar_ri + salt_ri)/5,    # calculate a weighted average score across the 5 nutrients based on lowest overall GDA contribution across nutrients
         wadd_ri4 = (energy_ri + fat_ri + sugar_ri + salt_ri)/4, # for only 4 nutrients used in the exp
         wadd_tfl4 = (fat_tfl + sugar_tfl + salt_tfl + satfat_tfl)/4,               # right now we are only looking at a limited number, but we could expand this for more info
         wadd_tfl3 = (fat_tfl + sugar_tfl + salt_tfl)/3
  ) %>%
  mutate(image_code = paste0(ID_code,".png"))

# all choice response values
energy <- read_csv("data/fc_task_en.csv") %>%
  mutate(best = 1) # which should be the best choice: 1 is energy, 2 is fat, 3 is sugar, 4 is salt, 5 is WADD

wadd <- read_csv("data/fc_task_wadd.csv") %>%
  mutate(best = 5)

fat <- read_csv("data/fc_task_fat.csv") %>%
  mutate(best = 2)

sugar <- read_csv("data/fc_task_sug.csv") %>%
  mutate(best = 3)

salt <- read_csv("data/fc_task_salt.csv") %>%
  mutate(best = 4)

choice_data <- rbind(energy, fat, sugar, salt, wadd) %>%
  dplyr::rename(choice1 = picture.choice1.currentitem,
                choice2 = picture.choice2.currentitem,
                choice3 = picture.choice3.currentitem,
                choice4 = picture.choice4.currentitem,
                choice5 = picture.choice5.currentitem,
                choice6 = picture.choice6.currentitem,
                trial = values.trial_num,
                response = trial.choice.response,
                switch = values.switch,
                correct = trial.choice.correct,
                rt = trial.choice.latency,
                id = subject) %>%
  mutate(food_type = ifelse(picture.foodtype.currentitem == "yog_t.png", "Yoghurt",
                            ifelse(picture.foodtype.currentitem == "ready_t.png", "Ready_meal",
                                   ifelse(picture.foodtype.currentitem == "sand_t.png", "Sandwich",
                                          ifelse(picture.foodtype.currentitem == "soup_t.png","Soup",
                                                 ifelse(picture.foodtype.currentitem == "crisps_t.png","Crisps",
                                                        ifelse(picture.foodtype.currentitem == "cereal_t.png","Cereal", NA))))))) %>%
  mutate(choice_item = ifelse(response == "choice1", substr(choice1, start = 1, stop = nchar(choice1)-4),
                              ifelse(response == "choice2", substr(choice2, start = 1, stop = nchar(choice2)-4),
                                     ifelse(response == "choice3", substr(choice3, start = 1, stop = nchar(choice3)-4),
                                            ifelse(response == "choice4", substr(choice4, start= 1, stop = nchar(choice4)-4),
                                                   ifelse(response == "choice5", substr(choice5, start = 1, stop = nchar(choice5)-4),
                                                          ifelse(response == "choice6",substr(choice6, start = 1, stop = nchar(choice6)-4), NA)))))))

rm(energy, fat, salt, sugar, wadd)

# ------- make variables for nutrition content of all choices, including final choice ------
# import nutrition data by cross-checking with stimuli list
for (i in 1:nrow(choice_data)){
  choice_data$choice1_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_servesize[i] <- food_list$Serve_size[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_health[i] <- food_list$health[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_energy[i] <- food_list$Energy[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_fat[i] <- food_list$Fat[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_sugar[i] <- food_list$Sugar[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_salt[i] <- food_list$Salt[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_satfat[i] <- food_list$Sat_Fat[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_energy_p100[i] <- food_list$energy_p100[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_fat_p100[i] <- food_list$fat_p100[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_salt_p100[i] <- food_list$salt_p100[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_energy_ri[i] <- food_list$energy_ri[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_fat_ri[i] <- food_list$fat_ri[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_salt_ri[i] <- food_list$salt_ri[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice1[i]==food_list$image_code]
  
  choice_data$choice1_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice1[i]==food_list$image_code]
  choice_data$choice1_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice1[i]==food_list$image_code]
  
}

for (i in 1:nrow(choice_data)){
  choice_data$choice2_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_servesize[i] <- food_list$Serve_size[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_health[i] <- food_list$health[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_energy[i] <- food_list$Energy[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_fat[i] <- food_list$Fat[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_sugar[i] <- food_list$Sugar[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_salt[i] <- food_list$Salt[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_satfat[i] <- food_list$Sat_Fat[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_energy_p100[i] <- food_list$energy_p100[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_fat_p100[i] <- food_list$fat_p100[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_salt_p100[i] <- food_list$salt_p100[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_energy_ri[i] <- food_list$energy_ri[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_fat_ri[i] <- food_list$fat_ri[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_salt_ri[i] <- food_list$salt_ri[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice2[i]==food_list$image_code]
  
  choice_data$choice2_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice2[i]==food_list$image_code]
  choice_data$choice2_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice2[i]==food_list$image_code]
  
}

for (i in 1:nrow(choice_data)){
  choice_data$choice3_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_servesize[i] <- food_list$Serve_size[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_health[i] <- food_list$health[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_energy[i] <- food_list$Energy[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_fat[i] <- food_list$Fat[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_sugar[i] <- food_list$Sugar[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_salt[i] <- food_list$Salt[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_satfat[i] <- food_list$Sat_Fat[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_energy_p100[i] <- food_list$energy_p100[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_fat_p100[i] <- food_list$fat_p100[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_salt_p100[i] <- food_list$salt_p100[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_energy_ri[i] <- food_list$energy_ri[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_fat_ri[i] <- food_list$fat_ri[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_salt_ri[i] <- food_list$salt_ri[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice3[i]==food_list$image_code]
  
  choice_data$choice3_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice3[i]==food_list$image_code]
  choice_data$choice3_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice3[i]==food_list$image_code]
  
}

for (i in 1:nrow(choice_data)){
  choice_data$choice4_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_servesize[i] <- food_list$Serve_size[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_health[i] <- food_list$health[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_energy[i] <- food_list$Energy[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_fat[i] <- food_list$Fat[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_sugar[i] <- food_list$Sugar[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_salt[i] <- food_list$Salt[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_satfat[i] <- food_list$Sat_Fat[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_energy_p100[i] <- food_list$energy_p100[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_fat_p100[i] <- food_list$fat_p100[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_salt_p100[i] <- food_list$salt_p100[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_energy_ri[i] <- food_list$energy_ri[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_fat_ri[i] <- food_list$fat_ri[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_salt_ri[i] <- food_list$salt_ri[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice4[i]==food_list$image_code]
  
  choice_data$choice4_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice4[i]==food_list$image_code]
  choice_data$choice4_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice4[i]==food_list$image_code]
  
}

for (i in 1:nrow(choice_data)){
  choice_data$choice5_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_servesize[i] <- food_list$Serve_size[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_health[i] <- food_list$health[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_energy[i] <- food_list$Energy[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_fat[i] <- food_list$Fat[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_sugar[i] <- food_list$Sugar[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_salt[i] <- food_list$Salt[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_satfat[i] <- food_list$Sat_Fat[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_energy_p100[i] <- food_list$energy_p100[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_fat_p100[i] <- food_list$fat_p100[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_salt_p100[i] <- food_list$salt_p100[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_energy_ri[i] <- food_list$energy_ri[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_fat_ri[i] <- food_list$fat_ri[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_salt_ri[i] <- food_list$salt_ri[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice5[i]==food_list$image_code]
  
  choice_data$choice5_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice5[i]==food_list$image_code]
  choice_data$choice5_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice5[i]==food_list$image_code]
  
}

for (i in 1:nrow(choice_data)){
  choice_data$choice6_nutri_rating[i] <- food_list$Nutri_Rating[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_servesize[i] <- food_list$Serve_size[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_health[i] <- food_list$health[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_energy[i] <- food_list$Energy[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_fat[i] <- food_list$Fat[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_sugar[i] <- food_list$Sugar[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_salt[i] <- food_list$Salt[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_satfat[i] <- food_list$Sat_Fat[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_energy_p100[i] <- food_list$energy_p100[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_fat_p100[i] <- food_list$fat_p100[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_sugar_p100[i] <- food_list$sugar_p100[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_salt_p100[i] <- food_list$salt_p100[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_satfat_p100[i] <- food_list$satfat_p100[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_energy_ri[i] <- food_list$energy_ri[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_fat_ri[i] <- food_list$fat_ri[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_sugar_ri[i] <- food_list$sugar_ri[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_salt_ri[i] <- food_list$salt_ri[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_satfat_ri[i] <- food_list$satfat_ri[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_fat_tfl[i] <- food_list$fat_tfl[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_sugar_tfl[i] <- food_list$sugar_tfl[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_salt_tfl[i] <- food_list$salt_tfl[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_satfat_tfl[i] <- food_list$satfat_tfl[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_wadd_ri5[i] <- food_list$wadd_ri5[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_wadd_ri4[i] <- food_list$wadd_ri4[choice_data$choice6[i]==food_list$image_code]
  
  choice_data$choice6_wadd_tfl4[i] <- food_list$wadd_tfl4[choice_data$choice6[i]==food_list$image_code]
  choice_data$choice6_wadd_tfl3[i] <- food_list$wadd_tfl3[choice_data$choice6[i]==food_list$image_code]
  
}

# check some random numbers to see that it copied correctly
x <- sample(1:nrow(choice_data), 1)
choice_data$choice4_fat_ri[x]
food_list$fat_ri[food_list$image_code == choice_data$choice4[x]]

choice_data$choice2_sugar[x]
food_list$Sugar[food_list$image_code == choice_data$choice2[x]]

rm(x, i, food_list)

# create variable that contains nutrition content of partipants' actual choices 
for (i in 1:nrow(choice_data)){
  if(choice_data$response[i]=="choice1"){
    choice_data$choice_nutri_rating[i] <- choice_data$choice1_nutri_rating[i] 
    choice_data$choice_servesize[i] <- choice_data$choice1_servesize[i]
    choice_data$choice_health[i] <- choice_data$choice1_health[i]
    
    choice_data$choice_energy[i] <- choice_data$choice1_energy[i]
    choice_data$choice_fat[i] <- choice_data$choice1_fat[i]
    choice_data$choice_sugar[i] <- choice_data$choice1_sugar[i]
    choice_data$choice_salt[i] <- choice_data$choice1_salt[i]
    choice_data$choice_satfat[i] <- choice_data$choice1_satfat[i]
    
    choice_data$choice_energy_p100[i] <- choice_data$choice1_energy_p100[i]
    choice_data$choice_fat_p100[i] <- choice_data$choice1_fat_p100[i]
    choice_data$choice_sugar_p100[i] <- choice_data$choice1_sugar_p100[i]
    choice_data$choice_salt_p100[i] <- choice_data$choice1_salt_p100[i]
    choice_data$choice_satfat_p100[i] <- choice_data$choice1_satfat_p100[i]
    
    choice_data$choice_energy_ri[i] <- choice_data$choice1_energy_ri[i]
    choice_data$choice_fat_ri[i] <- choice_data$choice1_fat_ri[i]
    choice_data$choice_sugar_ri[i] <- choice_data$choice1_sugar_ri[i]
    choice_data$choice_salt_ri[i] <- choice_data$choice1_salt_ri[i]
    choice_data$choice_satfat_ri[i] <- choice_data$choice1_satfat_ri[i]
    
    choice_data$choice_fat_tfl[i] <- choice_data$choice1_fat_tfl[i]
    choice_data$choice_sugar_tfl[i] <- choice_data$choice1_sugar_tfl[i]
    choice_data$choice_salt_tfl[i] <- choice_data$choice1_salt_tfl[i]
    choice_data$choice_satfat_tfl[i] <- choice_data$choice1_satfat_tfl[i]
    
    choice_data$choice_wadd_ri5[i] <- choice_data$choice1_wadd_ri5[i]
    choice_data$choice_wadd_ri4[i] <- choice_data$choice1_wadd_ri4[i]
    
    choice_data$choice_wadd_tfl4[i] <- choice_data$choice1_wadd_tfl4[i]
    choice_data$choice_wadd_tfl3[i] <- choice_data$choice1_wadd_tfl3[i]
    
  } else 
    if(choice_data$response[i]=="choice2"){
      
      choice_data$choice_nutri_rating[i] <- choice_data$choice2_nutri_rating[i]
      choice_data$choice_servesize[i] <- choice_data$choice2_servesize[i]
      choice_data$choice_health[i] <- choice_data$choice2_health[i]
      
      choice_data$choice_energy[i] <- choice_data$choice2_energy[i]
      choice_data$choice_fat[i] <- choice_data$choice2_fat[i]
      choice_data$choice_sugar[i] <- choice_data$choice2_sugar[i]
      choice_data$choice_salt[i] <- choice_data$choice2_salt[i]
      choice_data$choice_satfat[i] <- choice_data$choice2_satfat[i]
      
      choice_data$choice_energy_p100[i] <- choice_data$choice2_energy_p100[i]
      choice_data$choice_fat_p100[i] <- choice_data$choice2_fat_p100[i]
      choice_data$choice_sugar_p100[i] <- choice_data$choice2_sugar_p100[i]
      choice_data$choice_salt_p100[i] <- choice_data$choice2_salt_p100[i]
      choice_data$choice_satfat_p100[i] <- choice_data$choice2_satfat_p100[i]
      
      choice_data$choice_energy_ri[i] <- choice_data$choice2_energy_ri[i]
      choice_data$choice_fat_ri[i] <- choice_data$choice2_fat_ri[i]
      choice_data$choice_sugar_ri[i] <- choice_data$choice2_sugar_ri[i]
      choice_data$choice_salt_ri[i] <- choice_data$choice2_salt_ri[i]
      choice_data$choice_satfat_ri[i] <- choice_data$choice2_satfat_ri[i]
      
      choice_data$choice_fat_tfl[i] <- choice_data$choice2_fat_tfl[i]
      choice_data$choice_sugar_tfl[i] <- choice_data$choice2_sugar_tfl[i]
      choice_data$choice_salt_tfl[i] <- choice_data$choice2_salt_tfl[i]
      choice_data$choice_satfat_tfl[i] <- choice_data$choice2_satfat_tfl[i]
      
      choice_data$choice_wadd_ri5[i] <- choice_data$choice2_wadd_ri5[i]
      choice_data$choice_wadd_ri4[i] <- choice_data$choice2_wadd_ri4[i]
      
      choice_data$choice_wadd_tfl4[i] <- choice_data$choice2_wadd_tfl4[i]
      choice_data$choice_wadd_tfl3[i] <- choice_data$choice2_wadd_tfl3[i]
      
    } else
      if (choice_data$response[i] == "choice3"){
        
        choice_data$choice_nutri_rating[i] <- choice_data$choice3_nutri_rating[i]
        choice_data$choice_servesize[i] <- choice_data$choice3_servesize[i]
        choice_data$choice_health[i] <- choice_data$choice3_health[i]
        
        choice_data$choice_energy[i] <- choice_data$choice3_energy[i]
        choice_data$choice_fat[i] <- choice_data$choice3_fat[i]
        choice_data$choice_sugar[i] <- choice_data$choice3_sugar[i]
        choice_data$choice_salt[i] <- choice_data$choice3_salt[i]
        choice_data$choice_satfat[i] <- choice_data$choice3_satfat[i]
        
        choice_data$choice_energy_p100[i] <- choice_data$choice3_energy_p100[i]
        choice_data$choice_fat_p100[i] <- choice_data$choice3_fat_p100[i]
        choice_data$choice_sugar_p100[i] <- choice_data$choice3_sugar_p100[i]
        choice_data$choice_salt_p100[i] <- choice_data$choice3_salt_p100[i]
        choice_data$choice_satfat_p100[i] <- choice_data$choice3_satfat_p100[i]
        
        choice_data$choice_energy_ri[i] <- choice_data$choice3_energy_ri[i]
        choice_data$choice_fat_ri[i] <- choice_data$choice3_fat_ri[i]
        choice_data$choice_sugar_ri[i] <- choice_data$choice3_sugar_ri[i]
        choice_data$choice_salt_ri[i] <- choice_data$choice3_salt_ri[i]
        choice_data$choice_satfat_ri[i] <- choice_data$choice3_satfat_ri[i]
        
        choice_data$choice_fat_tfl[i] <- choice_data$choice3_fat_tfl[i]
        choice_data$choice_sugar_tfl[i] <- choice_data$choice3_sugar_tfl[i]
        choice_data$choice_salt_tfl[i] <- choice_data$choice3_salt_tfl[i]
        choice_data$choice_satfat_tfl[i] <- choice_data$choice3_satfat_tfl[i]
        
        choice_data$choice_wadd_ri5[i] <- choice_data$choice3_wadd_ri5[i]
        choice_data$choice_wadd_ri4[i] <- choice_data$choice3_wadd_ri4[i]
        
        choice_data$choice_wadd_tfl4[i] <- choice_data$choice3_wadd_tfl4[i]
        choice_data$choice_wadd_tfl3[i] <- choice_data$choice3_wadd_tfl3[i]
        
      } else
        if(choice_data$response[i]=="choice4"){
          
          choice_data$choice_nutri_rating[i] <- choice_data$choice4_nutri_rating[i]
          choice_data$choice_servesize[i] <- choice_data$choice4_servesize[i]
          choice_data$choice_health[i] <- choice_data$choice4_health[i]
          
          choice_data$choice_energy[i] <- choice_data$choice4_energy[i]
          choice_data$choice_fat[i] <- choice_data$choice4_fat[i]
          choice_data$choice_sugar[i] <- choice_data$choice4_sugar[i]
          choice_data$choice_salt[i] <- choice_data$choice4_salt[i]
          choice_data$choice_satfat[i] <- choice_data$choice4_satfat[i]
          
          choice_data$choice_energy_p100[i] <- choice_data$choice4_energy_p100[i]
          choice_data$choice_fat_p100[i] <- choice_data$choice4_fat_p100[i]
          choice_data$choice_sugar_p100[i] <- choice_data$choice4_sugar_p100[i]
          choice_data$choice_salt_p100[i] <- choice_data$choice4_salt_p100[i]
          choice_data$choice_satfat_p100[i] <- choice_data$choice4_satfat_p100[i]
          
          choice_data$choice_energy_ri[i] <- choice_data$choice4_energy_ri[i]
          choice_data$choice_fat_ri[i] <- choice_data$choice4_fat_ri[i]
          choice_data$choice_sugar_ri[i] <- choice_data$choice4_sugar_ri[i]
          choice_data$choice_salt_ri[i] <- choice_data$choice4_salt_ri[i]
          choice_data$choice_satfat_ri[i] <- choice_data$choice4_satfat_ri[i]
          
          choice_data$choice_fat_tfl[i] <- choice_data$choice4_fat_tfl[i]
          choice_data$choice_sugar_tfl[i] <- choice_data$choice4_sugar_tfl[i]
          choice_data$choice_salt_tfl[i] <- choice_data$choice4_salt_tfl[i]
          choice_data$choice_satfat_tfl[i] <- choice_data$choice4_satfat_tfl[i]
          
          choice_data$choice_wadd_ri5[i] <- choice_data$choice4_wadd_ri5[i]
          choice_data$choice_wadd_ri4[i] <- choice_data$choice4_wadd_ri4[i]
          
          choice_data$choice_wadd_tfl4[i] <- choice_data$choice4_wadd_tfl4[i]
          choice_data$choice_wadd_tfl3[i] <- choice_data$choice4_wadd_tfl3[i]
          
        } else
          if (choice_data$response[i] == "choice5") {
            
            choice_data$choice_nutri_rating[i] <- choice_data$choice5_nutri_rating[i]
            choice_data$choice_servesize[i] <- choice_data$choice5_servesize[i]
            choice_data$choice_health[i] <- choice_data$choice5_health[i]
            
            choice_data$choice_energy[i] <- choice_data$choice5_energy[i]
            choice_data$choice_fat[i] <- choice_data$choice5_fat[i]
            choice_data$choice_sugar[i] <- choice_data$choice5_sugar[i]
            choice_data$choice_salt[i] <- choice_data$choice5_salt[i]
            choice_data$choice_satfat[i] <- choice_data$choice5_satfat[i]
            
            choice_data$choice_energy_p100[i] <- choice_data$choice5_energy_p100[i]
            choice_data$choice_fat_p100[i] <- choice_data$choice5_fat_p100[i]
            choice_data$choice_sugar_p100[i] <- choice_data$choice5_sugar_p100[i]
            choice_data$choice_salt_p100[i] <- choice_data$choice5_salt_p100[i]
            choice_data$choice_satfat_p100[i] <- choice_data$choice5_satfat_p100[i]
            
            choice_data$choice_energy_ri[i] <- choice_data$choice5_energy_ri[i]
            choice_data$choice_fat_ri[i] <- choice_data$choice5_fat_ri[i]
            choice_data$choice_sugar_ri[i] <- choice_data$choice5_sugar_ri[i]
            choice_data$choice_salt_ri[i] <- choice_data$choice5_salt_ri[i]
            choice_data$choice_satfat_ri[i] <- choice_data$choice5_satfat_ri[i]
            
            choice_data$choice_fat_tfl[i] <- choice_data$choice5_fat_tfl[i]
            choice_data$choice_sugar_tfl[i] <- choice_data$choice5_sugar_tfl[i]
            choice_data$choice_salt_tfl[i] <- choice_data$choice5_salt_tfl[i]
            choice_data$choice_satfat_tfl[i] <- choice_data$choice5_satfat_tfl[i]
            
            choice_data$choice_wadd_ri5[i] <- choice_data$choice5_wadd_ri5[i]
            choice_data$choice_wadd_ri4[i] <- choice_data$choice5_wadd_ri4[i]
            
            choice_data$choice_wadd_tfl4[i] <- choice_data$choice5_wadd_tfl4[i]
            choice_data$choice_wadd_tfl3[i] <- choice_data$choice5_wadd_tfl3[i]
            
          } else
            if (choice_data$response[i]=="choice6"){
              
              choice_data$choice_nutri_rating[i] <- choice_data$choice6_nutri_rating[i]
              choice_data$choice_servesize[i] <- choice_data$choice6_servesize[i]
              choice_data$choice_health[i] <- choice_data$choice6_health[i]
              
              choice_data$choice_energy[i] <- choice_data$choice6_energy[i]
              choice_data$choice_fat[i] <- choice_data$choice6_fat[i]
              choice_data$choice_sugar[i] <- choice_data$choice6_sugar[i]
              choice_data$choice_salt[i] <- choice_data$choice6_salt[i]
              choice_data$choice_satfat[i] <- choice_data$choice6_satfat[i]
              
              choice_data$choice_energy_p100[i] <- choice_data$choice6_energy_p100[i]
              choice_data$choice_fat_p100[i] <- choice_data$choice6_fat_p100[i]
              choice_data$choice_sugar_p100[i] <- choice_data$choice6_sugar_p100[i]
              choice_data$choice_salt_p100[i] <- choice_data$choice6_salt_p100[i]
              choice_data$choice_satfat_p100[i] <- choice_data$choice6_satfat_p100[i]
              
              choice_data$choice_energy_ri[i] <- choice_data$choice6_energy_ri[i]
              choice_data$choice_fat_ri[i] <- choice_data$choice6_fat_ri[i]
              choice_data$choice_sugar_ri[i] <- choice_data$choice6_sugar_ri[i]
              choice_data$choice_salt_ri[i] <- choice_data$choice6_salt_ri[i]
              choice_data$choice_satfat_ri[i] <- choice_data$choice6_satfat_ri[i]
              
              choice_data$choice_fat_tfl[i] <- choice_data$choice6_fat_tfl[i]
              choice_data$choice_sugar_tfl[i] <- choice_data$choice6_sugar_tfl[i]
              choice_data$choice_salt_tfl[i] <- choice_data$choice6_salt_tfl[i]
              choice_data$choice_satfat_tfl[i] <- choice_data$choice6_satfat_tfl[i]
              
              choice_data$choice_wadd_ri5[i] <- choice_data$choice6_wadd_ri5[i]
              choice_data$choice_wadd_ri4[i] <- choice_data$choice6_wadd_ri4[i]
              
              choice_data$choice_wadd_tfl4[i] <- choice_data$choice6_wadd_tfl4[i]
              choice_data$choice_wadd_tfl3[i] <- choice_data$choice6_wadd_tfl3[i]
            }
}

# check some random numbers to see that it copied correctly
x <- sample(1:nrow(choice_data), 1)
choice_data$response[x]
choice_data$choice_fat_ri[x]
choice_data$choice2_fat_ri[x] # adjust number to whichever the response was

x <- sample(1:nrow(choice_data), 1)
choice_data$response[x]
choice_data$choice_sugar[x]
choice_data$choice5_sugar[x] # adjust number to whichever the response was

rm(x, i)

# interim save
# write_csv(choice_data, "data/choice_dataset.csv")
# choice_data <- read_csv("data/choice_dataset.csv")

# Merge with demographic data
demographics <- read_csv("data/surveyData.csv") %>%
  dplyr::select(-X1)

demographics$health_factor[demographics$health_factor == -1] <- NA  # set those who didn't specify what is most important to health as NA

data <- merge(choice_data, demographics, by = "id")

rm(demographics, choice_data)

# save
write_csv(data, "data/choice_dataset.csv")

# data file for modelling
# data <- read_csv("data/choice_dataset.csv") %>%
# filter(trialcode == "choice")  # filter out non-choice data

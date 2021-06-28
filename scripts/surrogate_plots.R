############################################################################################
# Can we predict if people will choose a certain option out of a set?
# written by Dawn Holford
# Updated 12 May 2021

# code for surrogate trees (with "yes"/"no" classification)

#-------- Set up workspace -----
rm(list=ls())
getwd() # check the working directory

#### This was how I got h2o to install: ------
# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/4/R")
# restart R before running the next lines

#####
packages <- c("tidyverse","tidymodels","h2o","caret","party","partykit","gridExtra","ggbeeswarm")
purrr::map(packages, library, character.only = TRUE)

h2o.init()  # Had to install only Java DK 11 (NOT 15) for this to work

seed <- 123
# ------- Make functions -----
# surrogate tree
surrogate <- function(mod, dat, depth){
  df_preds <- h2o.predict(mod, dat) %>%
    as.data.frame()
  df_orig <- as.data.frame(dat)
  
  df <- cbind(df_orig, df_preds) %>%
    dplyr::select(rt:dv, predict) %>%
    dplyr::select(-dv)
  
  model <- train(
    predict ~., data = df, method = "ctree2",
    na.action = na.omit,
    trControl = trainControl("cv", number = 5),  # using 5 cross-validation folds
    tuneGrid = expand.grid(maxdepth = depth, mincriterion = 0.95)  # you can adjust tree depth here, start with 3 and work up
  )
  
  return(model) 
}

# ------- Import data ------
alldata <- read_csv("data/data_with_strategies.csv")

dat <- alldata %>%
  dplyr::select(rt, food_type, 
                health_factor, HAS, NUT, gen, age, sum_visstrat,
                eqw4_nbest, expert_nbest, tfl3_nbest, ri_sugar_nbest:ri_salt_nbest, tfl_sugar_nbest:tfl_salt_nbest,
                nutri_distance:eqw4_distance, tfl3_distance, ri_sugar_distance:ri_salt_distance, tfl_sugar_distance:tfl_salt_distance,
                is_eqw4, is_expert, is_tfl3, is_sugarri:is_saltri, is_sugartfl:is_salttfl
  ) %>%
  mutate(is_cereal = ifelse(food_type == "Cereal", 1,0),
         is_yoghurt = ifelse(food_type == "Yoghurt", 1, 0),
         is_sandwich = ifelse(food_type == "Sandwich", 1, 0),
         is_crisps = ifelse(food_type == "Crisps", 1,0),
         is_readymeal = ifelse(food_type == "Ready_meal", 1, 0),
         is_soup = ifelse(food_type == "Soup", 1, 0),
         reduce_calories = ifelse(health_factor == 1, 1, 0),
         reduce_fat = ifelse(health_factor == 2, 1, 0),
         reduce_sugar = ifelse(health_factor == 3, 1, 0),
         reduce_salt = ifelse(health_factor == 4, 1, 0),
         reduce_all = ifelse(health_factor == 5, 1,0)
  ) %>%
  dplyr::select(rt, HAS:reduce_all) 

fctcols <- c("gen", "is_cereal","is_yoghurt","is_sandwich","is_crisps","is_readymeal","is_soup",
             "reduce_calories","reduce_fat","reduce_sugar","reduce_salt","reduce_all",
             "is_fatri","is_sugarri","is_saltri",
             "is_energyri","is_eqw4","is_tfl3","is_expert",
             "is_fattfl","is_sugartfl","is_salttfl")

dat[fctcols] <- lapply(dat[fctcols], factor)  # factor variables

sapply(dat, class)  # check class

rm(fctcols, alldata)

# ------- Build model for minimising %RIs -----
###### Sugar ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, testmodel, mod, glm)

sugarri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_sugar_nbest, ri_sugar_distance,
                is_sugarri) %>%
  mutate(dv = ifelse(is_sugarri == 1, "Yes","No")) %>%
  dplyr::select(-is_sugarri)  # this was just to test the surrogate trees but I think it makes the plot nicer

sugarri$dv <- factor(sugarri$dv)

# convert to h2o objects
moddat <- as.h2o(sugarri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) # 69/31 split
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = F,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_grid__1_AutoML_20210518_095453_model_2")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/RI/sugar/sugarri_surrogate.png", res = 100, height = 1000, width = 1200)
plot(testmodel$finalModel)
dev.off()

###### Fat ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, sugarri)

fatri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_fat_nbest, ri_fat_distance,
                is_fatri)  %>%
  mutate(dv = ifelse(is_fatri == 1, "Yes","No")) %>%
  dplyr::select(-is_fatri)  # this was just to test the surrogate trees but I think it makes the plot nicer

fatri$dv <- factor(fatri$dv)

# convert to h2o objects
moddat <- as.h2o(fatri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = F,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_1_AutoML_20210518_095814")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/RI/fat/fatri_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()



###### Energy ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, fatri)

energyri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_energy_nbest, ri_energy_distance,
                is_energyri)  %>%
  mutate(dv = ifelse(is_energyri == 1, "Yes","No")) %>%
  dplyr::select(-is_energyri)  # this was just to test the surrogate trees but I think it makes the plot nicer

energyri$dv <- factor(energyri$dv)

# convert to h2o objects
moddat <- as.h2o(energyri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = F,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("XGBoost_grid__1_AutoML_20210518_100337_model_2")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/RI/energy/energyri_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()


###### Salt ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, energyri)

saltri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_salt_nbest, ri_salt_distance,
                is_saltri)  %>%
  mutate(dv = ifelse(is_saltri == 1, "Yes","No")) %>%
  dplyr::select(-is_saltri)  # this was just to test the surrogate trees but I think it makes the plot nicer

saltri$dv <- factor(saltri$dv)

# convert to h2o objects
moddat <- as.h2o(saltri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = F,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_1_AutoML_20210518_100558")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/RI/salt/saltri_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()



###### All (average) ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, saltri)

eqw4 <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                eqw4_nbest, eqw4_distance,
                is_eqw4)  %>%
  mutate(dv = ifelse(is_eqw4 == 1, "Yes","No")) %>%
  dplyr::select(-is_eqw4)  

eqw4$dv <- factor(eqw4$dv)

# convert to h2o objects
moddat <- as.h2o(eqw4)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = F,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_3_AutoML_20210518_100807")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/RI/eqw/eqw4ri_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()

# ------- Build model for minimising TFL bands ---
###### Sugar ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, eqw4)

sugartfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_sugar_nbest, tfl_sugar_distance,
                is_sugartfl)  %>%
  mutate(dv = ifelse(is_sugartfl == 1, "Yes","No")) %>%
  dplyr::select(-is_sugartfl)  # this was just to test the surrogate trees but I think it makes the plot nicer

sugartfl$dv <- factor(sugartfl$dv)

# convert to h2o objects
moddat <- as.h2o(sugartfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.6, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = T,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_5_AutoML_20210518_101038")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/tfl/sugar/sugartfl_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()


###### Fat ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, testmodel, mod, sugartfl)

fattfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_fat_nbest, tfl_fat_distance,
                is_fattfl)  %>%
  mutate(dv = ifelse(is_fattfl == 1, "Yes","No")) %>%
  dplyr::select(-is_fattfl)  # this was just to test the surrogate trees but I think it makes the plot nicer

fattfl$dv <- factor(fattfl$dv)

# convert to h2o objects
moddat <- as.h2o(fattfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = T,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("XGBoost_1_AutoML_20210518_101234")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/tfl/fat/fattfl_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()

###### Salt ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, fattfl)

salttfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_salt_nbest, tfl_salt_distance,
                is_salttfl)  %>%
  mutate(dv = ifelse(is_salttfl == 1, "Yes","No")) %>%
  dplyr::select(-is_salttfl)  # this was just to test the surrogate trees but I think it makes the plot nicer

salttfl$dv <- factor(salttfl$dv)

# convert to h2o objects
moddat <- as.h2o(salttfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = T,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_grid__1_AutoML_20210518_101435_model_3")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/tfl/salt/salttfl_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()

###### All (average) ######
# remove previous models
# rm(train, test, split, moddat, lb, amls, testmodel, mod, salttfl)

tfl3 <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl3_nbest, tfl3_distance,
                is_tfl3)  %>%
  mutate(dv = ifelse(is_tfl3 == 1, "Yes","No")) %>%
  dplyr::select(-is_tfl3)  # this was just to test the surrogate trees but I think it makes the plot nicer

tfl3$dv <- factor(tfl3$dv)

# convert to h2o objects
moddat <- as.h2o(tfl3)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

# check sample occurrences
print(h2o.table(moddat['dv'])) 
print(h2o.table(train['dv']))
print(h2o.table(test['dv']))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = 'dv',                   # if x is not set, it automatically defaults to all the other variables in the data
                   training_frame = train,
                   max_models = 20,         # number of base models
                   balance_classes = T,     # use setting to balance the response classes 
                   exclude_algos = c("StackedEnsemble","DeepLearning"),
                   # max_runtime_secs = 60,  # comment out to run the full 20 models
                   seed = seed,
                   nfolds = 5,   # default folds number for validation
                   sort_metric = "mean_per_class_error")

# view all the models
lb <- h2o.get_leaderboard(object = amls)
print(lb, n = nrow(lb))

mod <- h2o.getModel("GBM_grid__1_AutoML_20210518_101807_model_1")

# ------ Surrogate models ----
testmodel <- surrogate(mod, test, 3)

png("plots/tfl/average/tfl3_surrogate.png", res = 80, height = 800, width = 1000)
plot(testmodel$finalModel)
dev.off()

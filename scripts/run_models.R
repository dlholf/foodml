############################################################################################
# Can we predict people will choose a certain option out of a set?
# Written by Dawn Holford
# Updated 26 May 2021

#-------- Set up workspace -----
rm(list=ls())
setwd('..') # if opening the script straight from script folder
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
packages <- c("tidyverse","tidymodels","h2o","caret","party","partykit","gridExtra","ggbeeswarm","stringi")
purrr::map(packages, library, character.only = TRUE)

h2o.init()  # Had to install only Java DK 11 (NOT 15) for this to work

seed <- 123
# ------- Make functions -----

# extract shap values from h2o (different methods for gbm and xgboost)
extract_shap <- function(mod, dat, type = "gbm"){
  
  if(type == "gbm"){
    shaps <- predict_contributions.H2OModel(mod, dat)
    shap_frame <- shaps %>%
      as.data.frame() %>%
      dplyr::select(-BiasTerm) 
    
    orig_dat <- as.data.frame(dat) %>%
      dplyr::select(names(shap_frame)) 
    
    orig_dat[] <- lapply(orig_dat, as.numeric)
    
    names(orig_dat) <- paste0("actual_",names(orig_dat))
    
    shap_frame <- shap_frame %>% 
      gather(feature,  shap_value) %>%
      group_by(feature) %>%
      mutate(shap_importance = mean(abs(shap_value)),
             shap_force = mean(shap_value)) %>%
      ungroup()
    
    orig_dat <- orig_dat %>%
      gather(actualvars, actualvals) %>%
      group_by(actualvars) %>%
      mutate(z_vals = (actualvals - mean(actualvals, na.rm = T))/sd(actualvals, na.rm = T)) %>%
      ungroup
    
    shap_df <- cbind(shap_frame, orig_dat) 
    
  } 
  else if(type == "xgboost"){
    
    shaps <- predict_contributions.H2OModel(mod, dat)
    shap_frame <- shaps %>%
      as.data.frame() %>%
      dplyr::select(-BiasTerm) %>%
      dplyr::select(sort(names(.)))
    
    singles <- shap_frame %>%
      dplyr::select(!ends_with(c(".1","NA.",".0",".2")))
    
    orig_dat <- as.data.frame(dat) 
    
    orig_dat <- orig_dat %>%
      dplyr::select(-ncol(orig_dat))
    
    orig_dat[] <- lapply(orig_dat, as.numeric)
    
    continuousvars <- orig_dat %>%
      dplyr::select(names(singles)) 
    
    othervars <- orig_dat %>%
      dplyr::select(!names(singles))
    
    nvals <- tibble(uniques = 0,
                    .rows = ncol(othervars))
    
    for (i in 1:ncol(othervars)){
      nvals$uniques[i] <- nrow(unique(othervars[i]))
    }
    
    keepcols <- which(nvals$uniques %in% c(2,3))
    
    for (i in 1:3){
      
      dupl <- othervars %>%
        select(all_of(keepcols))
      
      names(dupl) <- paste0(names(dupl),"_val",i)
      
      tib <- paste("vals",i, sep = "")
      assign(tib, dupl)
    }
    
    orig_dat <- cbind(continuousvars,vals1, vals2, vals3) %>%
      dplyr::select(sort(names(.)))
    
    names(orig_dat) <- paste0("actual_",names(orig_dat))
    
    shap_frame <- shap_frame %>% 
      gather(feature,  shap_value) %>%
      group_by(feature) %>%
      mutate(shap_importance = mean(abs(shap_value)),
             shap_force = mean(shap_value)) %>%
      ungroup()
    
    orig_dat <- orig_dat %>%
      gather(actualvars, actualvals) %>%
      group_by(actualvars) %>%
      mutate(z_vals = (actualvals - mean(actualvals, na.rm = T))/sd(actualvals, na.rm = T)) %>%
      ungroup
    
    shap_df <- cbind(shap_frame, orig_dat) 
    
  }
  
  return(shap_df)
  
}

# get correlations of shap values with actual values
shapmetrics <- function(data){
  
  features <- unique(data$feature)
  
  shap_cors <- tibble()
  for(i in 1:length(features)){
    
    cd <- data %>% filter(feature == features[i])
    corr <- cor.test(cd$shap_value, cd$z_vals)
    feature <- features[i]
    r <- corr$estimate
    p <- corr$p.value
    ci_lower <- corr$conf.int[1]
    ci_upper <- corr$conf.int[2]
    
    cors <- cbind(feature, r, p, ci_lower, ci_upper)
    
    shap_cors <- rbind(shap_cors, cors)
  }
  rownames(shap_cors) <- c()
  
  shap_cors <- arrange(shap_cors, desc(r))  
  
  return(shap_cors)
  
}

# shap plot
plot_shap <- function(df){
  ggplot(df, aes(x = shap_value, y = reorder(feature, shap_importance), color = z_vals)) +
    ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.9, alpha = 0.5, width = 0.15) +
    xlab("SHAP value") +
    ylab(NULL) +
    theme_minimal(base_size = 15) +
    scale_color_gradientn("Predictor value\n(z-transformation)", 
                          colors = c("#003f5c","#003f5c", "#2f4b7c","#665191","#a05195","#d45087","#f95d6a", "#ff7c43","#ffa600"),
                          values = scales::rescale(c(-4,-3.95,-3.9, -3,-2, -1,0,max(df$z_vals))))
}

# partial dependent plot
pdp <- function(mod, dat, feature, w, var_type = c("continuous","categorical"), modtype = "gbm", refclass = NULL){
  
  feature <- feature
  
  SHAP_values <- predict_contributions.H2OModel(mod, dat)
  
  variables <- as.data.frame(dat)
  
  col_index <- which(colnames(variables)==feature)
  
  if(var_type == "categorical"){
    
    if(modtype == "gbm"){ 
      
      pdplot <- SHAP_values %>%
        as.data.frame() %>%
        select(-BiasTerm) %>% 
        mutate(pred_values = as.vector(variables[,col_index])) %>% 
        ggplot(aes(x = pred_values, y = eval(as.name(feature)))) +
        geom_jitter(aes(color = eval(as.name(feature))), width = w) +
        scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
        ylab('Shapley\'s values') +
        xlab(paste0("Predictor classes for \"",feature,"\"")) +
        theme_minimal(base_size = 15) 
      
    }
    
    else if(modtype == "xgboost"){
      
      xgboostvar <- paste0(feature,refclass)
      
      pdplot <- SHAP_values %>%
        as.data.frame() %>%
        select(-BiasTerm) %>% 
        mutate(pred_values = as.vector(variables[,col_index])) %>% 
        ggplot(aes(x = pred_values, y = eval(as.name(xgboostvar)))) +
        geom_jitter(aes(color = eval(as.name(xgboostvar))), width = w) +
        scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
        ylab('Shapley\'s values') +
        xlab(paste0("Predictor classes for \"",feature,"\",","\nSHAP value with reference to class = ",stri_sub(refclass,1, -1))) +
        theme_minimal(base_size = 15) 
      
    }
    
  } else if (var_type == "continuous"){
    
    pdplot <- SHAP_values %>%
      as.data.frame() %>%
      select(-BiasTerm) %>% 
      mutate(pred_values = as.vector(variables[,col_index])) %>% 
      ggplot(aes(x = pred_values, y = eval(as.name(feature)))) +
      geom_point(aes(color = eval(as.name(feature)))) +
      scale_colour_gradient(low = "red", high = "blue", name = 'SHAP values') +
      ylab("Shapley\'s values") +
      xlab(paste0("Predictor values for \"", feature,"\"")) +
      theme_minimal(base_size = 15) +
      geom_smooth()
  }
  
  return(pdplot)
  
}

# shap and variable importance
plot_shap_full <- function(df){
  p1 <- ggplot(df, aes(x = shap_value, y = reorder(feature, shap_importance))) +
    ggbeeswarm::geom_quasirandom(groupOnX = FALSE, varwidth = TRUE, size = 0.9, alpha = 0.5, width = 0.15) +
    xlab("SHAP value") +
    ylab(NULL) +
    theme_minimal(base_size = 15)
  
  p2 <- df %>% 
    select(feature, shap_importance) %>%
    distinct() %>% 
    ggplot(aes(x = reorder(feature, shap_importance), 
               y = shap_importance)) +
    geom_col(fill = 'black') +
    coord_flip() +
    xlab(NULL) +
    ylab("mean(|SHAP value|)") +
    theme_minimal(base_size = 15)
  
  plot <- gridExtra::grid.arrange(p1, p2, nrow = 1)
  
  return(plot)
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
# rm(train, test, split, moddat, y, lb, amls, testmodel, trainmodel, mod, glm)

sugarri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_sugar_nbest, ri_sugar_distance,
                is_sugarri)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(sugarri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_sugarri'

# check sample occurrences
print(h2o.table(moddat[y])) # 69/31 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_grid__1_AutoML_20210526_101630_model_2")
glm <- h2o.getModel("GLM_1_AutoML_20210512_172847")

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/sugarri_model.csv")
write_csv(confmatrix, "metrics/sugarri_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/sugarri_glm.csv")
write_csv(glmmatrix, "metrics/sugarri_glmmatrix.csv")
write_csv(glm_coef, "metrics/sugarri_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)

write_csv(shapvals, "metrics/sugarri_shap.csv")

plot_shap(shapvals)
ggsave("plots/RI/sugar/sugarri_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check

ggsave("plots/RI/sugar/sugarri_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/sugarri_shapcorrs.csv")
rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "ri_sugar_distance", 0.2,  var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_dist.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "sum_visstrat", 0.2, var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2, var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2, var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_soup", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_soup.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "HAS", 0.2,  var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_HAS.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "NUT", 0.2,  var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_NUT.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_sugfactor.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "age", 0.2,  var_type = "continuous")
ggsave("plots/RI/sugar/sugarri_pdp_age.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_calories", 0.2,  var_type = "categorical")
ggsave("plots/RI/sugar/sugarri_pdp_energyfactor.png", height = 6, width = 6, units = "in", dpi = 200)

###### Fat ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, sugarri)

fatri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_fat_nbest, ri_fat_distance,
                is_fatri)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(fatri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_fatri'

# check sample occurrences
print(h2o.table(moddat[y])) # 71/29 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_1_AutoML_20210526_102124")
glm <- h2o.getModel("GLM_1_AutoML_20210512_180055")


h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/fatri_model.csv")
write_csv(confmatrix, "metrics/fatri_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/fatri_glm.csv")
write_csv(glmmatrix, "metrics/fatri_glmmatrix.csv")
write_csv(glm_coef, "metrics/fatri_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)
write_csv(shapvals, "metrics/fatri_shap.csv")

plot_shap(shapvals)
ggsave("plots/RI/fat/fatri_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check

ggsave("plots/RI/fat/fatri_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/fatri_shapcorrs.csv")

rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/RI/fat/fatri_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "ri_fat_distance", 0.2,  var_type = "continuous")
ggsave("plots/RI/fat/fatri_pdp_dist.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_soup", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_soup.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "HAS", 0.2,  var_type = "continuous")
ggsave("plots/RI/fat/fatri_pdp_HAS.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/RI/fat/fatri_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_sugfactor.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "age", 0.2,  var_type = "continuous")
ggsave("plots/RI/fat/fatri_pdp_age.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_salt", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_saltfactor.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_fat", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_fatfactor.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_calories", 0.2,  var_type = "categorical")
ggsave("plots/RI/fat/fatri_pdp_energyfactor.png", height = 6, width = 6, units = "in", dpi = 200)


###### Energy ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, fatri)

energyri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_energy_nbest, ri_energy_distance,
                is_energyri)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(energyri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_energyri'

# check sample occurrences
print(h2o.table(moddat[y])) # 67/33 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("XGBoost_grid__1_AutoML_20210526_151838_model_2")
glm <- h2o.getModel("GLM_1_AutoML_20210518_083146")

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/energyri_model.csv")
write_csv(confmatrix, "metrics/energyri_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/energyri_glm.csv")
write_csv(glmmatrix, "metrics/energyri_glmmatrix.csv")
write_csv(glm_coef, "metrics/energyri_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test, type = "xgboost")  

write_csv(shapvals, "metrics/energyri_shap.csv")

plot_shap(shapvals)

ggsave("plots/RI/energy/energyri_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check

ggsave("plots/RI/energy/energyri_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/energyri_shapcorrs.csv")

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/RI/energy/energyri_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "ri_energy_distance", 0.2,  var_type = "continuous")
ggsave("plots/RI/energy/energyri_pdp_distance.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/RI/energy/energyri_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/RI/energy/energyri_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/RI/energy/energyri_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/RI/energy/energyri_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/RI/energy/energyri_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "NUT", 0.2,  var_type = "continuous")
ggsave("plots/RI/energy/energyri_pdp_nutlabel.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_calories", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/RI/energy/energyri_pdp_factorcalories.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_salt", 0.2, var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/RI/energy/energyri_pdp_factorsalt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_all", 0.2, var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/RI/energy/energyri_pdp_factorall.png", height = 6, width = 6, units = "in", dpi = 200)

###### Salt ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, energyri)

saltri <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_salt_nbest, ri_salt_distance,
                is_saltri)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(saltri)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_saltri'

# check sample occurrences
print(h2o.table(moddat[y])) # 66/34 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_1_AutoML_20210526_161026")
glm <- h2o.getModel("GLM_1_AutoML_20210518_084741")

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/saltri_model.csv")
write_csv(confmatrix, "metrics/saltri_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/saltri_glm.csv")
write_csv(glmmatrix, "metrics/saltri_glmmatrix.csv")
write_csv(glm_coef, "metrics/saltri_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)
write_csv(shapvals, "metrics/saltri_shap.csv")

plot_shap(shapvals)
ggsave("plots/RI/salt/saltri_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check

ggsave("plots/RI/salt/saltri_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/saltri_shapcorrs.csv")

rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/RI/salt/saltri_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "ri_salt_distance", 0.2,  var_type = "continuous")
ggsave("plots/RI/salt/saltri_pdp_dist.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "gen", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_gender.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_factorsugar.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_salt", 0.2,  var_type = "categorical")
ggsave("plots/RI/salt/saltri_pdp_factorsalt.png", height = 6, width = 6, units = "in", dpi = 200)





###### All (average) ######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, saltri)

eqw4 <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                eqw4_nbest, eqw4_distance,
                is_eqw4)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(eqw4)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_eqw4'

# check sample occurrences
print(h2o.table(moddat[y])) # 57/43 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_3_AutoML_20210526_105204")
glm <- h2o.getModel("GLM_1_AutoML_20210518_085837")

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/eqw4ri_model.csv")
write_csv(confmatrix, "metrics/eqw4ri_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/eqw4ri_glm.csv")
write_csv(glmmatrix, "metrics/eqw4ri_glmmatrix.csv")
write_csv(glm_coef, "metrics/eqw4ri_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)
write_csv(shapvals, "metrics/eqw4ri_shap.csv")

plot_shap(shapvals)
ggsave("plots/RI/eqw/eqw4ri_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check
ggsave("plots/RI/eqw/eqw4ri_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/eqw4ri_shapcorrs.csv")
rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/RI/eqw/eqw4ri_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "eqw4_distance", 0.2,  var_type = "continuous")
ggsave("plots/RI/eqw/eqw4ri_pdp_distance.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/RI/eqw/eqw4ri_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "NUT", 0.2,  var_type = "continuous")
ggsave("plots/RI/eqw/eqw4ri_pdp_nutlabels.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_soup", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_soup.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_all", 0.2, var_type = "categorical")
ggsave("plots/RI/eqw/eqw4ri_pdp_factorall.png", height = 6, width = 6, units = "in", dpi = 200)

# -------- Build model for minimising TFL bands ----
######## Sugar #######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, eqw4)

sugartfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_sugar_nbest, tfl_sugar_distance,
                is_sugartfl)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(sugartfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.6, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_sugartfl'

# check sample occurrences
print(h2o.table(moddat[y])) # massive imbalance (99.7%) so made a 0.6 split
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_5_AutoML_20210526_172802") 
glm <- h2o.getModel("GLM_1_AutoML_20210518_091041") # best model actually, 20% false positive

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/sugartfl_model.csv")
write_csv(confmatrix, "metrics/sugartfl_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/sugartfl_glm.csv")
write_csv(glmmatrix, "metrics/sugartfl_glmmatrix.csv")
write_csv(glm_coef, "metrics/sugartfl_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)
write_csv(shapvals, "metrics/sugartfl_shap.csv")

plot_shap(shapvals)
ggsave("plots/tfl/sugar/sugartfl_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check
ggsave("plots/tfl/sugar/sugartfl_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/sugartfl_shapcorrs.csv")

rm(shapcors)
# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl_sugar_nbest", 0.2,  var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_nbest.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl_sugar_distance", 0.2,  var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_distance.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "NUT", 0.2,  var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_nutlabel.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/tfl/sugar/sugartfl_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2, var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_factorsugar.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2, var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2, var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2, var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2, var_type = "categorical")
ggsave("plots/tfl/sugar/sugartfl_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)


######## Fat #######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, sugartfl)

fattfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_fat_nbest, tfl_fat_distance,
                is_fattfl)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(fattfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_fattfl'

# check sample occurrences
print(h2o.table(moddat[y])) # 97/3 imbalance
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("XGBoost_1_AutoML_20210526_175013") 
glm <- h2o.getModel("GLM_1_AutoML_20210518_092538") 

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/fattfl_model.csv")
write_csv(confmatrix, "metrics/fattfl_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/fattfl_glm.csv")
write_csv(glmmatrix, "metrics/fattfl_glmmatrix.csv")
write_csv(glm_coef, "metrics/fattfl_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test, type = "xgboost")  
write_csv(shapvals, "metrics/fattfl_shap.csv")

plot_shap(shapvals)
ggsave("plots/tfl/fat/fattfl_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check
ggsave("plots/tfl/fat/fattfl_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/fattfl_shapcorrs.csv")

rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/tfl/fat/fattfl_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl_fat_nbest", 0.2,  var_type = "continuous")
ggsave("plots/tfl/fat/fattfl_pdp_nbest.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_fat", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/tfl/fat/fattfl_pdp_factorfat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/tfl/fat/fattfl_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/tfl/fat/fattfl_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "HAS", 0.2,  var_type = "continuous")
ggsave("plots/tfl/fat/fattfl_pdp_has.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".1")
ggsave("plots/tfl/fat/fattfl_pdp_factorsugar.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "NUT", 0.2,  var_type = "continuous")
ggsave("plots/tfl/fat/fattfl_pdp_nutlabel.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/tfl/fat/fattfl_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_calories", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/tfl/fat/fattfl_pdp_factorenergy.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_salt", 0.2,  var_type = "categorical", modtype = "xgboost", refclass = ".0")
ggsave("plots/tfl/fat/fattfl_pdp_factorsalt.png", height = 6, width = 6, units = "in", dpi = 200)


######## Salt #######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, fattfl)

salttfl <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_salt_nbest, tfl_salt_distance,
                is_salttfl)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(salttfl)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_salttfl'

# check sample occurrences
print(h2o.table(moddat[y])) # 91/9 imbalance
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_grid__1_AutoML_20210526_110641_model_3") 
glm <- h2o.getModel("GLM_1_AutoML_20210518_093532") 

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/salttfl_model.csv")
write_csv(confmatrix, "metrics/salttfl_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/salttfl_glm.csv")
write_csv(glmmatrix, "metrics/salttfl_glmmatrix.csv")
write_csv(glm_coef, "metrics/salttfl_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)  
write_csv(shapvals, "metrics/salttfl_shap.csv")

plot_shap(shapvals)
ggsave("plots/tfl/salt/salttfl_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check
ggsave("plots/tfl/salt/salttfl_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/salttfl_shapcorrs.csv")

rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/tfl/salt/salttfl_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl_salt_nbest", 0.2,  var_type = "continuous")
ggsave("plots/tfl/salt/salttfl_pdp_nbest.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl_salt_distance", 0.2,  var_type = "continuous")
ggsave("plots/tfl/salt/salttfl_pdp_distance.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_soup", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_soup.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/tfl/salt/salttfl_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_salt", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_factorsalt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/tfl/salt/salttfl_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)



######## All (average) #######
# remove previous models
# rm(train, test, split, moddat, y, lb, amls, mod, glm, shapvals, salttfl)

tfl3 <- dat %>% 
  dplyr::select(rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl3_nbest, tfl3_distance,
                is_tfl3)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

# convert to h2o objects
moddat <- as.h2o(tfl3)

split <- h2o.splitFrame(data = moddat, ratios = 0.8, seed = seed)
train <- split[[1]]
test <- split[[2]]

y <- 'is_tfl3'

# check sample occurrences
print(h2o.table(moddat[y])) # 91/9 imbalance
print(h2o.table(train[y]))
print(h2o.table(test[y]))

# run model 
# Go to http://localhost:54321/flow/index.html to see the models being run and all the hyperparameters. 
# Search: 'Auto Model' to find the right ones quicker
amls <- h2o.automl(y = y,                   # if x is not set, it automatically defaults to all the other variables in the data
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

mod <- h2o.getModel("GBM_grid__1_AutoML_20210526_182130_model_1") 
glm <- h2o.getModel("GLM_1_AutoML_20210518_094414") 

h2o.confusionMatrix(mod, test) # check confusion matrix

# ------- save metrics -----
confmatrix <- h2o.confusionMatrix(mod, test)

auc <- h2o.auc(mod)
logloss <- h2o.logloss(mod)
mpce <- h2o.mean_per_class_error(mod)
mse <- h2o.mse(mod)

f1 <- mean(h2o.F1(h2o.performance(mod, test))$f1)
f2 <- mean(h2o.F2(h2o.performance(mod, test))$f2)
f_5 <- mean(h2o.F0point5(h2o.performance(mod, test))$f0point5)

metrics <- tibble(auc = auc, logloss = logloss, mean_per_class_error = mpce, mse = mse, f1 = f1, f2 = f2, f.5 = f_5)

write_csv(metrics, "metrics/tfl3_model.csv")
write_csv(confmatrix, "metrics/tfl3_confmatrix.csv")

rm(confmatrix, auc, logloss, mpce, mse, metrics, f1, f2, f_5)

glmmatrix <- h2o.confusionMatrix(glm, test)
glmauc <- h2o.auc(glm)
glmll <- h2o.logloss(glm)
glmmpce <- h2o.mean_per_class_error(glm)
glmmse <- h2o.mse(glm)

glm_coef <- glm@model$coefficients_table

glmf1 <- mean(h2o.F1(h2o.performance(glm, test))$f1)
glmf2 <- mean(h2o.F2(h2o.performance(glm, test))$f2)
glmf_5 <- mean(h2o.F0point5(h2o.performance(glm, test))$f0point5)

glmmetrics <- tibble(auc=glmauc, logloss = glmll, mean_per_class_error = glmmpce, mse = glmmse, f1 = glmf1, f2 = glmf2, f.5 = glmf_5)
write_csv(glmmetrics, "metrics/tfl3_glm.csv")
write_csv(glmmatrix, "metrics/tfl3_glmmatrix.csv")
write_csv(glm_coef, "metrics/tfl3_glmcoef.csv")

rm(glmmetrics, glmll, glmauc, glmmpce, glmmse, glmmatrix, glm_coef, glmf1, glmf2, glmf_5)

# plot(h2o.performance(mod, test), type = "roc")


# ------ Shap plots -------

shapvals <- extract_shap(mod, test)  
write_csv(shapvals, "metrics/tfl3_shap.csv")

plot_shap(shapvals)
ggsave("plots/tfl/average/tfl3_shap.png", height = 8, width = 6, units = "in", dpi = 200)

h2o.shap_summary_plot(mod, test) # cross-check
ggsave("plots/tfl/average/tfl3_h2oshap.png", height = 8, width = 6, units = "in", dpi = 200)

# plot_shap_full(shapvals) # for variable importance

# shap value correlations
shapcors <- shapmetrics(shapvals)

write_csv(shapcors, "metrics/tfl3_shapcorrs.csv")

rm(shapcors)

# ------ PDP plots ------
pdp(mod, test, "sum_visstrat", 0.2,  var_type = "continuous")
ggsave("plots/tfl/average/tfl3_pdp_sumstrat.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl3_nbest", 0.2,  var_type = "continuous")
ggsave("plots/tfl/average/tfl3_pdp_nbest.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "tfl3_distance", 0.2,  var_type = "continuous")
ggsave("plots/tfl/average/tfl3_pdp_distance.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "rt", 0.2,  var_type = "continuous")
ggsave("plots/tfl/average/tfl3_pdp_rt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_readymeal", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_readymeal.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_sandwich", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_sandwich.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_yoghurt", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_yoghurt.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_crisps", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_crisps.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_sugar", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_factorsugar.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_soup", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_soup.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "reduce_all", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_factorall.png", height = 6, width = 6, units = "in", dpi = 200)

pdp(mod, test, "is_cereal", 0.2,  var_type = "categorical")
ggsave("plots/tfl/average/tfl3_pdp_cereal.png", height = 6, width = 6, units = "in", dpi = 200)

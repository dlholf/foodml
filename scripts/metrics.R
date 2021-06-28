############################################################################################
# Can we predict people will choose a certain option out of a set?
# written by Dawn Holford
# Updated 7 Jun 2021

# This script examines model metrics and performance for reporting
# Models generated from run_models.R and extracted metrics saved in a folder

# ----- Set up workspace ------
rm(list= ls())

packages <- c("tidyverse","lme4")
purrr::map(packages, library, character.only= T)

# ----- Read in model metrics ------
models <- list("sugarri","fatri","energyri","saltri","eqw4ri","sugartfl","fattfl","salttfl","tfl3") 

# ML model metrics 
mods <- paste0("metrics/", models, "_model.csv")
confmats <- paste0("metrics/",models, "_confmatrix.csv")

metrics <- purrr::map(mods, read_csv)
matrices <- purrr::map(confmats, read_csv) # note for confidence matrices, first row is for false positives, second for false negatives. cols = actual, rows = predicted

metrics <- rbind(models, metrics)
matrices <- rbind(models, matrices)

rm(mods, confmats)

# glm metrics
glms <- paste0("metrics/", models, "_glm.csv")
glmmats <- paste0("metrics/", models, "_glmmatrix.csv")
glmcoefs <- paste0("metrics/", models,"_glmcoef.csv")

glm_metrics <- purrr::map(glms, read_csv)
glm_matrices <- purrr::map(glmmats, read_csv)
glm_coefs <- purrr::map(glmcoefs, read_csv)

rm(glms, glmmats, glmcoefs)

glm_metrics <- rbind(models, glm_metrics)
glm_matrices <- rbind(models, glm_matrices)
glm_coefs <- rbind(models, glm_coefs)

# shaps
shapcorrs <- paste0("metrics/", models, "_shapcorrs.csv")

shap_corrs <- purrr::map(shaps, read_csv)
shap_corrs <- rbind(models, shap_corrs)

rm(shapcorrs)

shaplist <- paste0("metrics/", models,"_shap.csv")

shaps <- purrr::map(shaplist, read_csv)
shaps <- rbind(models, shaps)
rm(shaplist)

# ------- Inspect the metrics and matrices -------
# print metrics
for(i in 1:length(models)){
  print(metrics[1,i])
  print(metrics[2,i])
}

# print matrices
for(i in 1:length(models)){
  print(matrices[1,i])
  print(matrices[2,i])
}


# print GLM metrics
for(i in 1:length(models)){
  print(glm_metrics[1,i])
  print(glm_metrics[2,i])
}

# print GLM matrices
for(i in 1:length(models)){
  print(glm_matrices[1,i])
  print(glm_matrices[2,i])
}

# print GLM coefs
for(i in 1:length(models)){
  print(glm_coefs[1,i])
  glm_coefs[2,i] %>% as_tibble() %>% print(n = Inf)
}

rm(glm_coefs, glm_matrices, glm_metrics, matrices, metrics, models)

# print shaps
for(i in 1:length(models)){
  print(shap_metrics[1,i])
  shap_metrics[2,i] %>% as_tibble() %>% print(n = Inf)
}

# -------- Comparison with standard GLM  -----
# ------- Import data ------
alldata <- read_csv("data/data_with_strategies.csv")

dat <- alldata %>%
  dplyr::select(id, rt, food_type, 
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
         reduce_all = ifelse(health_factor == 5, 1,0),
         is_female = ifelse(gen == 1, 0, 1)
  ) %>%
  dplyr::select(id, rt, is_female, HAS:NUT, age:reduce_all) 

fctcols <- c("id","is_female", "is_cereal","is_yoghurt","is_sandwich","is_crisps","is_readymeal","is_soup",
             "reduce_calories","reduce_fat","reduce_sugar","reduce_salt","reduce_all",
             "is_fatri","is_sugarri","is_saltri",
             "is_energyri","is_eqw4","is_tfl3","is_expert",
             "is_fattfl","is_sugartfl","is_salttfl")

dat[fctcols] <- lapply(dat[fctcols], factor)  # factor variables

sapply(dat, class)  # check class

zcols <- c("rt","HAS","NUT","age","sum_visstrat","nutri_distance","eqw4_distance","tfl3_distance",
           "ri_sugar_distance","ri_fat_distance","ri_energy_distance","ri_salt_distance","tfl_sugar_distance",
           "tfl_fat_distance","tfl_salt_distance")

centcols <- c("eqw4_nbest","expert_nbest","tfl3_nbest","ri_sugar_nbest","ri_fat_nbest","ri_energy_nbest",
              "ri_salt_nbest","tfl_sugar_nbest","tfl_fat_nbest","tfl_salt_nbest")


dat[zcols] <- scale(dat[zcols])
dat[centcols] <- scale(dat[centcols], scale = F)

rm(fctcols, zcols, centcols, alldata)

# ------ sugarri -----
sugarri <- dat %>% 
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_sugar_nbest, ri_sugar_distance,
                is_sugarri)  # sticking with the 0-1 classification as I am more confident of it for the shap plots.

sugarglm <- glmer(data = sugarri, is_sugarri ~ is_female + is_cereal + is_yoghurt  + 
                    is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
                    reduce_salt + rt + HAS + NUT + age + sum_visstrat +
                    ri_sugar_distance + (1|id),
                  family = binomial(link = "logit"), nAGQ = 0)

summary(sugarglm) 
exp(coef(summary(sugarglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(sugarglm, sugarri)

# ------ fatri-------
fatri <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_fat_nbest, ri_fat_distance,
                is_fatri)

fatglm <- glmer(data = fatri, is_fatri ~ is_female + is_cereal  + is_sandwich + 
                  is_crisps + is_readymeal + is_soup + reduce_calories + reduce_sugar +
                  reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat  +
                  ri_fat_distance + (1|id),
                family = binomial(link = "logit"), nAGQ = 0)

summary(fatglm)
exp(coef(summary(fatglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(fatglm, fatri)

# ------- energyri -----
energyri <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_energy_nbest, ri_energy_distance,
                is_energyri)

energyglm <- glmer(data = energyri, is_energyri ~ is_female  + is_yoghurt + is_sandwich + 
                     is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat  +
                     reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat  +
                     ri_energy_distance + (1|id),
                   family = binomial(link = "logit"), nAGQ = 0)

summary(energyglm)
exp(coef(summary(energyglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(energyglm, energyri)

# ------- saltri ----
saltri <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                ri_salt_nbest, ri_salt_distance,
                is_saltri)

saltglm <- glmer(data = saltri, is_saltri ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
                   is_crisps + is_readymeal  + reduce_calories + reduce_fat + reduce_sugar +
                   reduce_salt + rt + HAS + NUT + age + sum_visstrat + ri_salt_nbest +
                   ri_salt_distance + (1|id),
                 family = binomial(link = "logit"), nAGQ = 0)

summary(saltglm)
exp(coef(summary(saltglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(saltglm, saltri)

# ----- eqw4 ------
eqw4 <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                eqw4_nbest, eqw4_distance,
                is_eqw4)

eqwglm <- glmer(data = eqw4, is_eqw4 ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
                  is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
                  reduce_salt + rt + HAS + NUT + age + sum_visstrat  +
                  eqw4_distance + (1|id),
                family = binomial(link = "logit"), nAGQ = 0)

summary(eqwglm)
exp(coef(summary(eqwglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(eqwglm, eqw4)


# ----- sugartfl -----
sugartfl <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_sugar_nbest, tfl_sugar_distance,
                is_sugartfl)

# tflsugarglm <- glmer(data = sugartfl, is_sugartfl ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
#                   is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
#                   reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat + tfl_sugar_nbest +
#                   tfl_sugar_distance + (1|id),
#                 family = binomial(link = "logit"), nAGQ = 0)

tflsugarglm <- glmer(data = sugartfl, is_sugartfl ~ is_female  + is_cereal + is_yoghurt + is_sandwich + 
                       is_crisps + is_readymeal +
                       reduce_fat + reduce_sugar + reduce_salt + 
                       reduce_salt  + rt + HAS + NUT + age + sum_visstrat + tfl_sugar_nbest +
                       (1|id),
                     family = binomial(link = "logit"), nAGQ = 0)

summary(tflsugarglm)
exp(coef(summary(tflsugarglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(tflsugarglm, sugartfl)


# ----- fattfl -----
fattfl <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_fat_nbest, tfl_fat_distance,
                is_fattfl)

# tflfatglm <- glmer(data = fattfl, is_fattfl ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
#                        is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
#                        reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat + tfl_fat_nbest +
#                        tfl_fat_distance + (1|id),
#                      family = binomial(link = "logit"), nAGQ = 0)

tflfatglm <- glmer(data = fattfl, is_fattfl ~ is_female  + 
                     is_readymeal + reduce_calories + reduce_fat + reduce_sugar +
                     reduce_salt  + rt + HAS + NUT + age + sum_visstrat + tfl_fat_nbest +
                     tfl_fat_distance + (1|id),
                   family = binomial(link = "logit"), nAGQ = 0)

summary(tflfatglm)
exp(coef(summary(tflfatglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(tflfatglm, fattfl)

# ----- salttfl -----
salttfl <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl_salt_nbest, tfl_salt_distance,
                is_salttfl)

# tflsaltglm <- glmer(data = salttfl, is_salttfl ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
#                      is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
#                      reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat + tfl_salt_nbest +
#                      tfl_salt_distance + (1|id),
#                    family = binomial(link = "logit"), nAGQ = 0)

tflsaltglm <- glmer(data = salttfl, is_salttfl ~ is_female  + 
                      is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
                      reduce_salt + rt + HAS + NUT + age + sum_visstrat + tfl_salt_nbest +
                      (1|id),
                    family = binomial(link = "logit"), nAGQ = 0)

summary(tflsaltglm)
exp(coef(summary(tflsaltglm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(tflsaltglm, salttfl)

# ------ tfl3 ------
tfl3 <- dat %>%
  dplyr::select(id, rt:sum_visstrat,
                is_cereal:reduce_all,
                tfl3_nbest, tfl3_distance,
                is_tfl3)

# tfl3glm <- glmer(data = tfl3, is_tfl3 ~ is_female + is_cereal + is_yoghurt + is_sandwich + 
#                      is_crisps + is_readymeal + is_soup + reduce_calories + reduce_fat + reduce_sugar +
#                      reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat + tfl3_nbest +
#                      tfl3_distance + (1|id),
#                    family = binomial(link = "logit"), nAGQ = 0)

tfl3glm <- glmer(data = tfl3, is_tfl3 ~ is_female + reduce_fat + reduce_sugar +
                   reduce_salt + reduce_all + rt + HAS + NUT + age + sum_visstrat + tfl3_nbest +
                   (1|id),
                 family = binomial(link = "logit"), nAGQ = 0)

summary(tfl3glm)
exp(coef(summary(tfl3glm))) # ORs, first column; use exp(sugarglm@beta) for raw numbers

rm(tfl3glm, tfl3)
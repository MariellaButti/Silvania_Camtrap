# Juliana, vamos montar o codigo de inferencia multimodelos a partir desse script
# vamos ajustando ate que fique do nosso agrado

# Multimodel inference
# Run all possible models with the covariates and see their fit using AIC
# a nice tutorial is available at:
# https://cornelllabofornithology.github.io/ebird-best-practices/occupancy.html

library(MuMIn)
library(stringr)

# fit model
# after creating the object umPonca in Part 2
# here you must write a global model with all covariates

# global model with all variables affecting both psi and p
global.model <- occu(~norm.slope+norm.altitude ~norm.slope+norm.altitude, umPonca)

# look at the regression coefficients from the model
summary(global.model)

# dredge all possible combinations of the occupancy covariates
occ_dredge <- dredge(global.model)
occ_dredge # check multimodel comparison

# model comparison to explore the results for occupancy
mc <- as.data.frame(occ_dredge) %>% 
  select(starts_with("psi(p"), df, AICc, delta, weight)

# shorten names for printing (Elildo: have to check how to customize this for our data)
#names(mc) <- names(mc) %>% 
#  str_extract("(?<=psi\\(pland_[0-9]{2}_)[a-z_]+") %>% 
#  coalesce(names(mc))

# take a quick peak at the model selection table
# each line is a different model
# I still have improve this so that the table shows the variables in each model
# meanwhile you can see the full models using View(occ_dredge)
mutate_all(mc, ~ round(., 3)) %>% 
  head(18) %>% 
  knitr::kable()

# select models with the most support for model averaging (< 2.5 delta aicc)
occ_dredge_delta <- get.models(occ_dredge, subset = delta <= 2.5)

# average models based on model weights 
occ_avg <- model.avg(occ_dredge_delta, fit = TRUE)

# model averaged coefficients for occupancy and detection probability
coef(occ_avg)

# more interesting stuff at the site indicated above

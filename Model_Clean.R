setwd("M:/DATA/Glyphosate/Diet Paper")

totals = read.csv("ASA24_HEI_Totals_Paired.csv", header = TRUE)
urine = read.csv("GP_AMPA.csv", header = TRUE)
participants = read.csv("Participant_Info.csv", header = TRUE)

library(nlme)

# https://stats.stackexchange.com/questions/398444/how-to-correctly-model-repeated-measures-random-effects-in-a-linear-mixed-effect
# https://rcompanion.org/handbook/G_03.html
# https://blog.minitab.com/en/adventures-in-statistics-2/how-important-are-normal-residuals-in-regression-analysis

### data setup
urine$GP_OVER_LOD = !urine$GP_LOD
urine$AMPA_OVER_LOD = !urine$AMPA_LOD

dat = merge(totals, urine, by = "Urine_ID")
dat$Study_ID = dat$Study_ID.x
dat$Study_ID.x = NULL
dat$Study_ID.y = NULL

participants$PhysActDi = NA
participants[!is.na(participants$PhysAct) & participants$PhysAct < 150,"PhysActDi"] = 0
participants[!is.na(participants$PhysAct) & participants$PhysAct >= 150,"PhysActDi"] = 1

### imputation
# library(mice)
# drop = c("Age.Exact","DNAmAge.Exact","BMICat","HerbUse","ZIP","PhysAct",
#          "Meals_FastFood_Text","Meals_OtherRest_Text","Meals_DeliGrocery_Text","Meals_HomeCook_Text",
#          "Meals_FastFood_Weekly","Meals_OtherRest_Weekly","Meals_DeliGrocery_Weekly","Meals_HomeCook_Weekly",
#          "Avg.CR.mgdL","Avg.GP.ngmL","Avg.AMPA.ngmL","Avg.GP.ngmL.Tert","Avg.AMPA.ngmL.Tert",
#          "Avg.GP.ugG","Avg.AMPA.ugG","Avg.GP.ugG.Tert","Avg.AMPA.ugG.Tert")
# participants = participants[,!colnames(participants) %in% drop]
# organic_vars = c("EatOrganic_Fruit","EatOrganic_Veg","EatOrganic_Grain","EatOrganic_Meat","EatOrganic_Eggs","EatOrganic_Dairy")
# sum(complete.cases(participants[,!colnames(participants) %in% organic_vars]))
# imputed_participants <- mice(participants[,!colnames(participants) %in% organic_vars], m=5, method = 'pmm')

#### ok imputation predicted: for race, all white, for education, all college grad, for physact, all 1 (met guidelines), for smoking, all 1 (former)
participants[is.na(participants$Race),"Race"] = "White"
participants[is.na(participants$Smoking),"Smoking"] = 1
participants[is.na(participants$PhysActDi),"PhysActDi"] = 1
participants[is.na(participants$Education),"Education"] = "College_Grad"

participants$Income_Cat = 0
participants[!(is.na(participants$ZIP_Income)) & participants$ZIP_Income >= 100000 & participants$ZIP_Income < 150000 ,"Income_Cat"] = 1
participants[!(is.na(participants$ZIP_Income)) & participants$ZIP_Income >= 150000,"Income_Cat"] = 2
participants[is.na(participants$ZIP_Income),"Income_Cat"] = NA

dat = merge(dat, participants, by = "Study_ID")


levels(dat$Meals_FastFood) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week","Less than 1 per month")
levels(dat$Meals_DeliGrocery) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week","Less than 1 per month")
levels(dat$Meals_OtherRest) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week")
levels(dat$Meals_HomeCook) = c("1 or more per day","4-6 per week","Less than 1 per week","Less than 1 per week","1-3 times per week")
dat$Meals_FastFood = relevel(dat$Meals_FastFood, ref = "Less than 1 per month")
dat$Meals_DeliGrocery = relevel(dat$Meals_DeliGrocery, ref = "Less than 1 per month")
dat$Meals_OtherRest = relevel(dat$Meals_OtherRest, ref = "Less than 1 per month")

dat$Meals_FastFood = factor(dat$Meals_FastFood,levels = c("Less than 1 per month","1-3 times per month","1-3 times per week","4 or more per week"))
dat$Meals_HomeCook = factor(dat$Meals_HomeCook,levels = c("Less than 1 per week","1-3 times per week","4-6 per week","1 or more per day"))
dat$Meals_HomeCook = relevel(dat$Meals_HomeCook, ref = "1 or more per day")

dat$Race = relevel(dat$Race,"White")
levels(dat$Race) = c("White","Asian","Other","Hispanic","Other")

dat$HEI_Quartile = as.integer(cut(dat$HEI_All, quantile(dat$HEI_All, probs=0:4/4, na.rm = TRUE), include.lowest=TRUE))

dat$Smoking = as.factor(dat$Smoking)

dat$PF_SEAFD = dat$PF_SEAFD_HI + dat$PF_SEAFD_LOW
dat$PF_ORGCURED = dat$PF_ORGAN + dat$PF_CUREDMEAT
dat$PF_MEAT_POULTRY = dat$PF_MPS_TOTAL - dat$PF_SEAFD
dat[dat$PF_MEAT_POULTRY < 1E-10,"PF_MEAT_POULTRY"] = 0

dat$PF_Vegetarian = dat$PF_NUTSDS + dat$PF_EGGS + dat$PF_SOY

## used for adjustment by food group
dat$F_QUARTILE = as.integer(cut(dat$F_TOTAL,quantile(dat$F_TOTAL), include.lowest = TRUE))
dat$V_QUARTILE = as.integer(cut(dat$V_TOTAL,quantile(dat$V_TOTAL), include.lowest = TRUE))
dat$PFMPS_QUARTILE = as.integer(cut(dat$PF_MPS_TOTAL,quantile(dat$PF_MPS_TOTAL), include.lowest = TRUE))
dat$PFMP_QUARTILE = as.integer(cut(dat$PF_MEAT_POULTRY,quantile(dat$PF_MEAT_POULTRY), include.lowest = TRUE))
dat$PFVEG_QUARTILE = as.integer(cut(dat$PF_Vegetarian,quantile(dat$PF_Vegetarian), include.lowest = TRUE))
dat$D_QUARTILE = as.integer(cut(dat$D_TOTAL,quantile(dat$D_TOTAL), include.lowest = TRUE))
dat$G_QUARTILE = as.integer(cut(dat$G_TOTAL,quantile(dat$G_TOTAL), include.lowest = TRUE))


dat_sens = dat[dat$DropSensitivity != 1,]




#### Setup functions for running and saving ME models

# ## deprecated: assigns quartiles based on NONZERO values of variable; zero values assigned quartile 0
# quartile_zeros = function(var){
#   var_nonzero = var[var > 0]
#   quartiles = quantile(var_nonzero)
#   output = as.integer(cut(var,c(0,0.99*quartiles[1],quartiles[2:5]),labels = 0:4, include.lowest = TRUE))
#   return(output)
# }


## runs mixed-effects linear models on continuous variables with nlme.
# arguments:
# df: data frame containing data used in model
# out: response variable
# pred: predictor variable of interest. modeled as fixed effect
# factor: logical - should pred be evaluated as a factor?
# fix: variables to include in model as fixed effects in addition to pred. single variable or vector
# rand: variables to include in model as random effects. does not accept a list currently, just a single variable or "var1 + var2" format
# ci: logical - should 95% CIs for model coefficients be calculated?
# shapiro: logical - test model residuals for normality with shapiro-wilk test?
# log: logical - log-transform response variable?
# pred_cut: logical - should pred be transformed? if TRUE, if >= 50% of value of pred are nonzero, cuts it into quartiles, otherwise cuts it into zero/nonzero
me_model = function(df, out, pred, factor = FALSE, fix, rand, ci = TRUE, shapiro = TRUE, log = TRUE, pred_cut = FALSE){
  
  if (factor == TRUE){
    df[,pred] = as.factor(df[,pred])
  }
  
  if (pred_cut == TRUE){
    if (sum(df[,pred] > 0)/nrow(df) >= 0.5){
      cuts = quantile(df[,pred])
      if (cuts[2] == 0){ cuts[2] = 1E-5 }
      df[,pred] = as.integer(cut(df[,pred], cuts, labels = 1:4,include.lowest = TRUE))
      eval_as = "Quartile"
    } else {
      df[,pred] = as.integer(df[,pred] > 0)
      eval_as = "Binary"
    }

  }
  
  if (log == TRUE){
    model_fixed = paste0("log(",out,") ~ ", paste(pred,paste(fix, collapse = " + "),sep = " + "))
    model_random = paste0("~ 1 | ",rand)
  }
  
  else {
    model_fixed = paste0(out," ~ ", paste(pred,paste(fix, collapse = " + "),sep = " + "))
    model_random = paste0("~ 1 | ",rand)
  }
 
  model = lme(as.formula(model_fixed), random = as.formula(model_random), data = df, method = "REML", na.action = "na.omit")
  
  summ = summary(model)
  coeffs = coef(summ)[grepl(pred,rownames(coef(summ))),]
  
  if ( factor == TRUE){
    out = as.data.frame(coeffs)
    out$pred = rownames(out)
  } else {
    out = as.data.frame(t(coeffs))
    out$pred = pred
  }
  
  if (ci == TRUE){
    out$ci.l = out$Value - 1.96*out$Std.Error
    out$ci.u = out$Value + 1.96*out$Std.Error
  }
  
 if (shapiro == TRUE){
    shap.p = shapiro.test(resid(model))[["p.value"]]
    out$Shapiro_P = shap.p
  }
  
  if (factor == TRUE){
    out$VarType = "Factor"
  } else if (pred_cut == TRUE){
    out$VarType = eval_as
  } else{
    out$VarType = "Numeric"
  }
  
  return(out)
   
}
 




### Individual-level (numeric) predictors
 
preds = c("Age","BMI","PhysActDi","Income_Cat",
         "HEI_All","HEI_Quartile","HEI2015_TOTAL_SCORE",
         "EatOrganic")

confounders = c("CR_mgdL","KCAL","HEI2015_TOTAL_SCORE","PhysActDi","Race")

results.GP = as.data.frame(matrix(nrow = 0,ncol = 10))
results.AMPA = as.data.frame(matrix(nrow = 0,ncol = 10))

for (i in 1:length(preds)){
  output.GP = me_model(df = dat, out = "GP_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID")
  output.AMPA = me_model(df = dat, out = "AMPA_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID")
  
  results.GP = rbind(results.GP, output.GP) # todo: fix this to preallocate array. easy for numerics. bit of algebra for factors
  results.AMPA = rbind(results.AMPA, output.AMPA)
}
#AMPA models all violate assumptions. think about
# might need to do detected/not detected, logistic model



### Individual-level (factor) predictors
preds_factor = c("Race","Smoking",
                 "HEI_Quartile",
                 "EatOrganic",
                 "Education","WaterSource",
                 "Meals_FastFood","Meals_HomeCook")

confounders = c("CR_mgdL","KCAL","HEI2015_TOTAL_SCORE","PhysActDi","Race")

results.GP.factor = as.data.frame(matrix(nrow = 0,ncol = 10))
results.AMPA.factor = as.data.frame(matrix(nrow = 0,ncol = 10))

for (i in 1:length(preds)){
  output.GP = me_model(df = dat, out = "GP_ngmL", pred = preds_factor[i], factor = TRUE, fix = confounders, rand = "Study_ID")
  output.AMPA = me_model(df = dat, out = "AMPA_ngmL", pred = preds_factor[i], factor = TRUE, fix = confounders, rand = "Study_ID")
  
  results.GP.factor = rbind(results.GP.factor, output.GP)
  results.AMPA.factor = rbind(results.AMPA.factor, output.AMPA)
}


### Recall-level predictors
preds = c("F_TOTAL",
          "V_TOTAL",
          "G_TOTAL","G_WHOLE","G_REFINED",
          "PF_TOTAL","PF_MEAT_POULTRY",
          "D_TOTAL",
          "V_LEGUMES","Corn","Oats",
          "PF_EGGS","PF_SOY","Soy_Combo","PF_NUTSDS",
          "Coffee_Oz","Tea_Oz",
          "Alcohol_Oz","Wine_Oz","Beer_Oz","Liquor_Oz")

confounders = c("CR_mgdL","KCAL","HEI2015_TOTAL_SCORE","PhysActDi","Race")

results.GP = as.data.frame(matrix(nrow = 0,ncol = 10))
results.AMPA = as.data.frame(matrix(nrow = 0,ncol = 10))

for (i in 1:length(preds)){
  output.GP = me_model(df = dat, out = "GP_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID", pred_cut = TRUE)
  output.AMPA = me_model(df = dat, out = "AMPA_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID", pred_cut = TRUE)
  
  results.GP = rbind(results.GP, output.GP)
  results.AMPA = rbind(results.AMPA, output.AMPA)
}




######### SUBSETTED BY ORGANIC EATING
## categories:  fruit, veg, grain, meat, eggs, dairy

preds = c("F_TOTAL",
          "V_TOTAL",
          "G_TOTAL","G_WHOLE","G_REFINED","Oats",
          "PF_MEAT_POULTRY",
          "PF_EGGS",
          "D_TOTAL",
          "Corn","V_LEGUMES","PF_SOY")

cuts = c("EatOrganic_Fruit",
         "EatOrganic_Veg",
         "EatOrganic_Grain","EatOrganic_Grain","EatOrganic_Grain","EatOrganic_Grain",
         "EatOrganic_Meat",
         "EatOrganic_Eggs",
         "EatOrganic_Dairy",
         "EatOrganic","EatOrganic","EatOrganic")

which = c(2)
confounders = c("CR_mgdL","KCAL","HEI2015_TOTAL_SCORE","PhysActDi","Race")

# RUN MODELS AND SAVE RESULTS
results.GP = as.data.frame(matrix(nrow = 0,ncol = 10))
results.AMPA = as.data.frame(matrix(nrow = 0,ncol = 10))

for (i in 1:length(preds)){
  subset = dat[dat[,cuts[i]] %in% which,]
  output.GP = me_model(df = subset, out = "GP_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID", pred_cut = TRUE)
  output.AMPA = me_model(df = subset, out = "AMPA_ngmL", pred = preds[i], fix = confounders, rand = "Study_ID", pred_cut = TRUE)

  results.GP = rbind(results.GP, output.GP)
  results.AMPA = rbind(results.AMPA, output.AMPA)
  
}

results.GP$OrganicEatingCat = which
results.AMPA$OrganicEatingCat = which


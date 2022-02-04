library(nlme)
source("mixedModel.R")

# a few references
# https://stats.stackexchange.com/questions/398444/how-to-correctly-model-repeated-measures-random-effects-in-a-linear-mixed-effect
# https://rcompanion.org/handbook/G_03.html
# https://blog.minitab.com/en/adventures-in-statistics-2/how-important-are-normal-residuals-in-regression-analysis


# load simplified, simulated data for demonstration
individual = read.csv("Data_Individual.csv", header = TRUE, stringsAsFactors = FALSE)
recall = read.csv("Data_Recall.csv", header = TRUE, stringsAsFactors = FALSE)
urine = read.csv("Data_Urine.csv", header = TRUE, stringsAsFactors = FALSE)

# prepare data a bit
individual$Smok = as.factor(individual$Smok)
individual$EatOrganic = as.factor(individual$EatOrganic)
individual$Race = as.factor(individual$Race)
individual$Race = relevel(individual$Race,ref = "White")

recall$Date = as.Date(recall$Date, format = "%m/%d/%y")
urine$Date = as.Date(urine$Date, format = "%m/%d/%y")

data = merge(recall,individual, by = "StudyID")
data = merge(data,urine, by = c("StudyID","Date"))

data$StudyID = as.character(data$StudyID)


# run linear mixed model for multiple predictor variables + 2 analytes, adjusted for same confounders
preds = c("Age","BMI","HEI2015_TOTAL_SCORE")
preds_factor = c("Race","PhysAct","Smok","EatOrganic")
preds_diet = c("F_TOTAL","V_TOTAL","G_TOTAL","PF_TOTAL","D_TOTAL","V_LEGUMES","PF_SOY","Alcohol_Oz")

confounders = c("CR_mgdL","KCAL","HEI2015_TOTAL_SCORE")


results_Analyte1 = vector(mode = "list",length = length(preds) + length(preds_factor) + length(preds_diet))
results_Analyte2 = vector(mode = "list",length = length(preds) + length(preds_factor) + length(preds_diet))

for (i in 1:length(preds)){
  results_Analyte1[[i]] = mixedModel(df = data, out = "Analyte_1", pred = preds[i], log = TRUE, fix = confounders, rand = "StudyID")
  results_Analyte2[[i]] = mixedModel(df = data, out = "Analyte_2", pred = preds[i], log = TRUE, fix = confounders, rand = "StudyID")
}

for (i in 1:length(preds_factor)){
  results_Analyte1[[i + length(preds)]] = mixedModel(df = data, out = "Analyte_1", pred = preds_factor[i], factor = TRUE, log = TRUE, fix = confounders, rand = "StudyID")
  results_Analyte2[[i + length(preds)]] = mixedModel(df = data, out = "Analyte_2", pred = preds_factor[i], factor = TRUE, log = TRUE, fix = confounders, rand = "StudyID")
}

for (i in 1:length(preds_diet)){
  results_Analyte1[[i + length(preds) + length(preds_factor)]] = mixedModel(df = data, out = "Analyte_1", pred = preds_diet[i], pred_cut = TRUE, log = TRUE, fix = confounders, rand = "StudyID")
  results_Analyte2[[i + length(preds) + length(preds_factor)]] = mixedModel(df = data, out = "Analyte_2", pred = preds_diet[i], pred_cut = TRUE, log = TRUE, fix = confounders, rand = "StudyID")
}

results_Analyte1 = do.call("rbind",results_Analyte1)
results_Analyte2 = do.call("rbind",results_Analyte2)



# Subsetting example: Dietary variables subsetted by organic eating
# Uses preds_diet and confounders from above

# specify variable to subset by
var = "EatOrganic"

cats = levels(data[,var])
catNames = paste0(var,cats)

# initialize: store as list of lists for each analyte
subset_results_Analyte1 = vector(mode = "list",length = length(cats))
names(subset_results_Analyte1) = catNames
for (i in catNames){
  subset_results_Analyte1[[i]] = vector(mode = "list",length = length(preds_diet))
  
}

subset_results_Analyte2 = vector(mode = "list",length = length(cats))
names(subset_results_Analyte2) = catNames
for (i in catNames){
  subset_results_Analyte2[[i]] = vector(mode = "list",length = length(preds_diet))
  
}

# run models for each pred and cat. Be mindful of multiple comparisons when interpreting results
for (j in 1:length(cats)){
  for (i in 1:length(preds_diet)){
    subset = data[data[,var] == cats[j],]
    subset_results_Analyte1[[ catNames[j] ]][[i]] = mixedModel(df = subset, out = "Analyte_1", pred = preds_diet[i], fix = confounders, rand = "StudyID", pred_cut = TRUE)
    subset_results_Analyte2[[ catNames[j] ]][[i]] = mixedModel(df = subset, out = "Analyte_2", pred = preds_diet[i], fix = confounders, rand = "StudyID", pred_cut = TRUE)
  }
  
  subset_results_Analyte1[[ catNames[j] ]] = do.call("rbind",subset_results_Analyte1[[ catNames[j] ]] )
  subset_results_Analyte2[[ catNames[j] ]] = do.call("rbind",subset_results_Analyte2[[ catNames[j] ]] )
}

# for each analyte, you end up with a list with one data.frame for each category of your subsetting variable
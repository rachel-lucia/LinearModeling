setwd("M:/DATA/Glyphosate/Diet Paper")

totals = read.csv("ASA24_HEI_Totals_Paired.csv", header = TRUE)
urine = read.csv("GP_AMPA.csv", header = TRUE)
participants = read.csv("Participant_Info.csv", header = TRUE)

participants$OrganicScorePct = participants$OrganicScore/participants$OrganicScoreDenom
score_cuts =  quantile(participants$OrganicScorePct)
score_cuts[2] = 1E-5
participants$OrganicScoreQuartile = as.integer(cut(participants$OrganicScorePct,score_cuts,labels = 1:4,include.lowest = TRUE))

participants$PhysActDi = NA
participants[!is.na(participants$PhysAct) & participants$PhysAct < 150,"PhysActDi"] = 0
participants[!is.na(participants$PhysAct) & participants$PhysAct >= 150,"PhysActDi"] = 1

participants$Income_Cat = 0
participants[!(is.na(participants$ZIP_Income)) & participants$ZIP_Income >= 100000 & participants$ZIP_Income < 150000 ,"Income_Cat"] = 1
participants[!(is.na(participants$ZIP_Income)) & participants$ZIP_Income >= 150000,"Income_Cat"] = 2
participants[is.na(participants$ZIP_Income),"Income_Cat"] = NA

levels(participants$Meals_FastFood) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week","Less than 1 per month")
levels(participants$Meals_DeliGrocery) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week","Less than 1 per month")
levels(participants$Meals_OtherRest) = c("4 or more per week","4 or more per week","Less than 1 per month","1-3 times per month","1-3 times per week")
levels(participants$Meals_HomeCook) = c("1 or more per day","4-6 per week","Less than 1 per week","Less than 1 per week","1-3 times per week")
participants$Meals_FastFood = relevel(participants$Meals_FastFood, ref = "Less than 1 per month")
participants$Meals_DeliGrocery = relevel(participants$Meals_DeliGrocery, ref = "Less than 1 per month")
participants$Meals_OtherRest = relevel(participants$Meals_OtherRest, ref = "Less than 1 per month")
participants$Meals_HomeCook = relevel(participants$Meals_HomeCook, ref = "1 or more per day")

participants$Race = relevel(participants$Race,"White")
levels(participants$Race) = c("White","Asian","Other","Hispanic","Other")

participants$HEI_Quartile = as.integer(cut(participants$HEI_All, quantile(participants$HEI_All, probs=0:4/4, na.rm = TRUE), include.lowest=TRUE))

participants$Smoking = as.factor(participants$Smoking)


dat = merge(totals, urine, by = "Urine_ID")
dat$Study_ID = dat$Study_ID.x
dat$Study_ID.x = NULL
dat$Study_ID.y = NULL

dat = merge(dat, participants, by = "Study_ID")

dat$PF_SEAFD = dat$PF_SEAFD_HI + dat$PF_SEAFD_LOW
dat$PF_ORGCURED = dat$PF_ORGAN + dat$PF_CUREDMEAT
dat$PF_MEAT_POULTRY = dat$PF_MPS_TOTAL - dat$PF_SEAFD

# generates crosstabulations for categorical and continuous variables
# cols is a categorical variable
# rows can be categorical or continuous
# gets test statistics: fishers for categorical, anova for continuous
crossTab = function(df,rows,cols,dtype){
  
  df.rows = df[,rows]
  df.cols = df[,cols]
  
  if (dtype == "numeric"){
    mean = aggregate(df.rows, by = list(df.cols), mean, na.rm = TRUE)
    sd = aggregate(df.rows, by = list(df.cols), sd, na.rm = TRUE)
    
    out.table = cbind(as.data.frame(mean[,2]),as.data.frame(sd[,2]))
    colnames(out.table) = paste0(rows,c(".Mean",".SD"))
    
    anova = summary(aov(df.rows ~ df.cols))
    out.p = anova[[1]][1,5]
  }  else {
    counts = table(df.cols, df.rows)
    cats = colnames(counts)
    colnames(counts) = paste0(rows,".",cats,".Count")
    
    props = prop.table(counts,1)
    colnames(props) = paste0(rows,".",cats,".Prop")
    
    out.table = cbind(counts, props)
    
    fisher = fisher.test(counts, workspace = 50000000, simulate.p.value = TRUE)
    out.p = fisher$p.value
    
  }
  
  toReturn = list(out.table, out.p)
  
  return(toReturn)
  
}


### get medians and IQRs by groups + kruskal p values
# rows: categorical variable
# cols: numeric variable
crossMedian = function(df,rows,cols){
  
  df.rows = df[,rows]
  df.cols = df[,cols]
  
  out.table = aggregate(df.cols, by = list(df.rows), FUN = quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  out.table = do.call(data.frame,out.table)
  out.table$Group.1 = as.character(out.table$Group.1)
  
  colnames(out.table) = c("Category",paste0(cols,c(".25",".50",".75")))
  out.table$Category = paste0(rows,out.table$Category)
  
  kruskal = kruskal.test(df.cols, g = df.rows)
  out.p = kruskal$p.value
  
  toReturn = list(out.table, out.p)
  
  return(toReturn)
  
}




### individual-level characteristics
vars = c("Age","BMI","PhysActDi","Income_Cat","ZIP_Income",
          "HEI_All","OrganicScorePct","Race","Smoking",
          "Education","WaterSource",
          "Meals_FastFood","Meals_OtherRest","Meals_DeliGrocery","Meals_HomeCook",
         "N_PairedRecalls")

types = c("numeric","numeric","cat","cat","numeric",
          "numeric","numeric","cat","cat",
          "cat","cat",
          "cat","cat","cat","cat",
          "cat")

outcome = "EatOrganic"

table = as.data.frame(matrix(nrow = 3,ncol = 1))
table$V1 = c(0,1,2)
colnames(table) = outcome

pvals = NULL

for (i in 1:length(vars)){
  
  output = crossTab(df = participants, rows = vars[i], cols = outcome, dtype = types[i])
  
  table = cbind(table,output[[1]])
  
  pvals = cbind(pvals,output[[2]])
  
}
colnames(pvals) = paste0("P.",vars)

# get median/IQR
cont_vars = vars[types == "numeric"]
table_med = as.data.frame(matrix(nrow = 3,ncol = 0))

pvals_med = NULL

for (i in 1:length(cont_vars)){
  
  output = crossMedian(df = participants, rows = outcome, cols = cont_vars[i])
  
  outtable = output[[1]]
  
  if (i != 1){table_med = cbind(table_med,outtable[2:4])
  } else { table_med = cbind(table_med,outtable)}
  
  pvals_med = cbind(pvals_med,output[[2]])
  
}
colnames(pvals_med) = paste0("P.",cont_vars)








#### diet variables - major food groups
# get median/IQR
diet_vars = c("KCAL","F_TOTAL","V_TOTAL","G_TOTAL","G_WHOLE","G_REFINED","PF_TOTAL","PF_MEAT_POULTRY","D_TOTAL","GP_ngmL","AMPA_ngmL","GP_ugG","AMPA_ugG")

table_diet = as.data.frame(matrix(nrow = 3,ncol = 0))

pvals_diet = NULL

for (i in 1:length(diet_vars)){
  
  output = crossMedian(df = dat, rows = outcome, cols = diet_vars[i])
  
  outtable = output[[1]]
  
  if (i != 1){table_diet = cbind(table_diet,outtable[2:4])
  } else { table_diet = cbind(table_diet,outtable)}
  
  pvals_diet = cbind(pvals_diet,output[[2]])
  
}
colnames(pvals_diet) = paste0("P.",diet_vars)


#### diet variables - subgroups
# get proportion who consumed
subdiet_vars = c("Coffee_Oz","Tea_Oz",
                 "Alcohol_Oz","Wine_Oz","Liquor_Oz","Beer_Oz",
                 "V_LEGUMES","Corn","Oats","PF_MEAT","PF_ORGCURED","PF_POULT","PF_SEAFD",
                 "PF_EGGS","PF_SOY","Soy_Combo","PF_NUTSDS")
dat_sub = dat[,c(outcome,subdiet_vars)]
dat_sub[,2:ncol(dat_sub)] = as.integer(dat_sub[,2:ncol(dat_sub)] > 1E-10)



table_subdiet = as.data.frame(matrix(nrow = 3,ncol = 1))
table_subdiet$V1 = c(0,1,2)
colnames(table_subdiet) = outcome

pvals_subdiet = NULL

for (i in 1:length(subdiet_vars)){
  
  output = crossTab(df = dat_sub, rows = subdiet_vars[i], cols = outcome, dtype = "cat")
  
  table_subdiet = cbind(table_subdiet,output[[1]])
  
  pvals_subdiet = cbind(pvals_subdiet,output[[2]])
  
}
colnames(pvals_subdiet) = paste0("P.",subdiet_vars)


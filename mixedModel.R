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
# log: logical - log-transform response variable? default = TRUE
# pred_cut: logical - should pred be transformed? if TRUE, if >= 50% of value of pred are nonzero, cuts it into quartiles, otherwise cuts it into zero/nonzero
mixedModel = function(df, out, pred, factor = FALSE, fix, rand, ci = TRUE, shapiro = TRUE, log = TRUE, pred_cut = FALSE){
  
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
  
  if (factor == TRUE){
    out = as.data.frame(coeffs)
    if (ncol(out) == 1) out = as.data.frame(t(out))
    out$pred = rownames(coef(summ))[grep(pred,rownames(coef(summ)))]
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
  
  rownames(out) = NULL
  
  return(out)
  
}
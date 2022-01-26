### generates crosstabulations for categorical and continuous variables
### summary statistics: mean/sd or median/IQR or counts/proportions + p values for comparisons across cross-tabulation categories
# arguments:
# df: data source
# cols: optional name of categorical variable to cross-tabulate by
# rows: name or vector of names of variable(s) to evaluate. categorical or continuous. type is detected for each.
# parametric: for continuous 'rows' variables only. test for differences in distribution using parametric (one-way ANOVA) or non-parametric (Kruskal-Wallis) methods
#             parametric = TRUE calculates means and SDs. parametic = FALSE calculates medians and IQRs
#             input is ignored for categorical 'rows' variables and fisher's exact test used to test for difference in proportions


# todo: input validation
# check that cols is factor or character

# todo: check how logicals are handled

# todo: actually add support for optional cols argument

# todo: fix row names for categorical vars

# todo: write wrapper to apply helper function across multiple row inputs

# helper function for single variable
crossTab = function(df,cols,row,parametric = TRUE){
  
  class = class(df[,row])
  if (class %in% c("character","logical","factor")){
    dtype = "categorical"
  } else if (class %in% c("numeric","integer")){
    dtype = "continuous"
  }
  
  if (dtype == "continuous" && parametric == TRUE ){
    mean = aggregate(df[,row], by = list(df[,cols]), mean, na.rm = TRUE)
    sd = aggregate(df[,row], by = list(df[,cols]), sd, na.rm = TRUE)
    
    out.table = cbind(as.data.frame(mean[,2]),as.data.frame(sd[,2]))
    colnames(out.table) = paste0(row,c(".Mean",".SD"))
    
    out.table$Category = paste0(cols,unique(df[,cols]))
    out.table = out.table[,c(3,1,2)]
    
    anova = summary(aov(df[,row] ~ df[,cols]))
    out.p = anova[[1]][1,5]
  } else if (dtype == "continuous" && parametric == FALSE ){

    out.table = aggregate(df[,row], by = list(df[,cols]), FUN = quantile, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
    
    out.table = do.call(data.frame,out.table)
    out.table$Group.1 = as.character(out.table$Group.1)
    
    colnames(out.table) = c("Category",paste0(row,c(".25",".50",".75")))
    out.table$Category = paste0(cols,out.table$Category)
    
    kruskal = kruskal.test(df[,row], g = df[,cols])
    out.p = kruskal$p.value

  } else if (dtype == "categorical"){
    counts = table(df[,cols], df[,row])
    cats = colnames(counts)
    colnames(counts) = paste0(row,".",cats,".Count")
    
    props = prop.table(counts,1)
    colnames(props) = paste0(row,".",cats,".Prop")
    
    out.table = cbind(counts, props)
    
    out.
    
    fisher = fisher.test(counts, simulate.p.value = TRUE)
    out.p = fisher$p.value
    
  }
  
  toReturn = list(summstats = out.table, pvals = out.p)
  
  return(toReturn)
  
}
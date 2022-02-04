### generates crosstabulations for categorical and continuous variables
### summary statistics: mean/sd or median/IQR or counts/proportions + p values for comparisons across cross-tabulation categories
# arguments:
# df: data source
# cols: optional name of categorical variable to cross-tabulate by
# rows: name or vector of names of variable(s) to evaluate. categorical or continuous. type is detected for each.
# parametric: for continuous 'rows' variables only. test for differences in distribution using parametric (one-way ANOVA) or non-parametric (Kruskal-Wallis) methods
#             parametric = TRUE calculates means and SDs. parametic = FALSE calculates medians and IQRs
#             input is ignored for categorical 'rows' variables and fisher's exact test used to test for difference in proportions

# todo: add support for optional cols argument

crossTab = function(df,cols,rows,parametric = TRUE){
  levels = unique(df[,cols])
  summstats = vector(mode = "list",length = length(rows))
  pvals = as.data.frame(matrix(nrow = length(rows),ncol = 2))
  colnames(pvals) = c("p","test")
  
  for (i in 1:length(rows)){
    out = crossTabSingle(df = df,cols = cols,row = rows[i],parametric = parametric)
    summstats[[i]] = out$summstats
    pvals[i,] = out$pvals
  }
  rownames(pvals) = rows
  
  summstats = do.call("rbind",summstats)
  
  toReturn = list(summstats = summstats, pvals = pvals)
  
  return(toReturn)
  
}



# helper function for single variable
crossTabSingle = function(df,cols,row,parametric = TRUE){
  
  colClass = class(df[,cols])
  if (!(colClass %in% c("character","logical","factor"))){
    stop("Can only cross-tabulate by a single categorical variable. Try again with a different value for cols")
  }
  
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
    
    out.table = t(out.table)
    colnames(out.table) = paste0(cols,unique(df[,cols]))

    anova = summary(aov(df[,row] ~ df[,cols]))
    out.p = as.data.frame(anova[[1]][1,5])
    colnames(out.p) = "p"
    rownames(out.p) = row
    out.p$test = "anova"
    
  } else if (dtype == "continuous" && parametric == FALSE ){

    out.table = aggregate(df[,row], by = list(df[,cols]), FUN = quantile, probs = c(0.5, 0.25, 0.75), na.rm = TRUE)
    
    out.table = do.call(data.frame,out.table)
    out.table$Group.1 = as.character(out.table$Group.1)
    
    colnames(out.table) = c("Category",paste0(row,c(".50",".25",".75")))
    cats = paste0(cols,out.table$Category)
    
    out.table = t(out.table[,-1])
    colnames(out.table) = cats
    
    kruskal = kruskal.test(df[,row], g = df[,cols])
    out.p = as.data.frame(kruskal$p.value)
    colnames(out.p) = "p"
    rownames(out.p) = row
    out.p$test = "kruskal"
    
  } else if (dtype == "categorical"){
    counts = table(df[,cols], df[,row])
    cats = colnames(counts)
    colnames(counts) = paste0(row,".",cats,".Count")
    
    props = prop.table(counts,1)
    colnames(props) = paste0(row,".",cats,".Prop")
    
    out.table = rbind(t(counts), t(props))
    colnames(out.table) = paste0(cols,colnames(out.table))
    
    fisher = fisher.test(counts, simulate.p.value = TRUE)
    out.p = as.data.frame(fisher$p.value)
    colnames(out.p) = "p"
    rownames(out.p) = row
    out.p$test = "fisher"
    
  }
  
  toReturn = list(summstats = out.table, pvals = out.p)
  
  return(toReturn)
  
}
# make a dummy dataset for demonstrating/testing crossTab
dat = as.data.frame(matrix(nrow = 600, ncol = 0))
dat$cat = c("A","B","C")
dat$cat2 = c("C","B","A")
dat[dat$cat == "A","num"] = rnorm(200, mean = 1.5)
dat[dat$cat == "B","num"] = rnorm(200, mean = 2.5)
dat[dat$cat == "C","num"] = rnorm(200, mean = 5.5)
dat$num2 = rnorm(600,mean = 10, sd = 5)
dat$factor = c("Red","Yellow","Blue","Blue","Red","Blue")
dat$factor = as.factor(dat$factor)
dat$int = sample.int(600)
dat$logi = c(TRUE,FALSE,TRUE,TRUE,FALSE)


source("crossTab.R")

# examples
crossTab(df = dat, cols = "factor",rows = "logi")
crossTab(df = dat, cols = "factor",rows = "num2")
crossTab(df = dat, cols = "logi", rows = c("cat2","num","num2"))
crossTab(df = dat, cols = "cat", rows = c("cat2","num","num2","factor","int","logi"))
crossTab(df = dat, cols = "cat", rows = c("cat2","num","num2","factor","int","logi"), parametric = FALSE)
crossTab(df = dat, cols = "num", rows = c("cat2","num","num2","factor","int","logi"))
crossTab(df = dat, cols = c("cat","factor"), rows = c("cat2","num","num2","factor","int","logi"))
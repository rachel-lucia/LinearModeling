setwd("C:/Users/Rachel/Documents/R/Scripts/LinearModeling")


# make a dummy dataset for testing

dat = as.data.frame(matrix(nrow = 600, ncol = 0))
dat$cat = c("A","B","C")
dat$cat2 = c("C","B","A")
dat[dat$cat == "A","num"] = rnorm(200, mean = 1.5)
dat[dat$cat == "B","num"] = rnorm(200, mean = 2.5)
dat[dat$cat == "C","num"] = rnorm(200, mean = 5.5)
dat$num2 = rnorm(600,mean = 10, sd = 5)
dat$factor = c(3,2,1,2,3,1)
dat$factor = as.factor(dat$factor)
dat$int = sample.int(600)
dat$logi = c(TRUE,FALSE,TRUE,TRUE,FALSE)

write.csv(dat,file = "DummyData.csv",row.names = FALSE)

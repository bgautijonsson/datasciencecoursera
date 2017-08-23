setwd('/Users/notandi/Data Science Specialization/R Programming/Week 2')
list.files()
source('pollutantmean.R')
source('complete.R')
source('corr.R')




pollutantmean('specdata','nitrate')
corr('specdata', threshold = 1000)


set.seed(42)
cc <- complete('specdata', 332:1)
use <- sample(332, 10)
print(cc[use, 'nobs'])

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

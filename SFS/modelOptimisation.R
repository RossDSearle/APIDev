set.seed(7)

library(mlbench)
library(caret)

# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

version='3'
dataRoot <- 'C:/Projects/SMIPS/SFS/regionalSM/data'
modDf <- read.csv(paste0(dataRoot, version, '/Drills/rawTraining.csv'), stringsAsFactors = F)

sub <- modDf[sample(nrow(modDf), 200000), ]
y <- sub$probeVal
x <- sub[5:ncol(sub)]


control <- rfeControl(functions=rfFuncs, method="cv", number=3)
subsets <- c(1:10, 12, 15, 20, 25)
results <- rfe(x, y, sizes=subsets, rfeControl=control)
# summarize the results
print(results)
str(x)

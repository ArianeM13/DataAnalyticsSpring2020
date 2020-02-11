#KNN Aabalone

abalone <- read.csv(file.choose(), header = T, sep = ',')
#colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
summary(abalone)
str(abalone)
summary(abalone$Rings)
# breaking rings into 3 levels" “young” for abalones less than 8, “adult” for abalones between 8-11,and “old” for abalones older than 11.
abalone$Rings <- as.numeric(abalone$Rings)
abalone$Rings
abalone$Rings <- cut(abalone$Rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$Rings
abalone$Rings <- as.factor(abalone$Rings)
abalone$Rings
summary(abalone$Rings)
# remove the "sex" varialbe in abalone, because KNN requires all numeric variables for prediction
# z <- abalone
aba <- abalone
aba$sex <- NULL
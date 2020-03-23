#Class Assignment 6 - PCA

#PCA US ARRESTS
data(USArrests)
states = row.names(USArrests) 
states
names(USArrests)
#mean using apply function
#1 - rows, 2 - columns
apply(USArrests,2, mean)
apply(USArrests,1, mean) 
#var with apply function
apply(USArrests , 2, var)
apply(USArrests , 1, var)

#have to scale variables to standardise and have a mean of 0
#and std. dev of 1 before doing PCA

#prcomp performs PCA
# By default, the prcomp() function centers the variables to have mean zero. By using the option scale=TRUE, 
# we scale the variables to have standard deviation one.
# The output from prcomp() contains a number of useful quantities
pr.out = prcomp(USArrests, scale=TRUE)
names(pr.out)

#center - mean, scale - std dev -> variables used prior to scaling
pr.out$center
pr.out$scale

#rotation- the principal component loadings
#each column of contains the corresponding principal component loading vector

pr.out$rotation

dim(pr.out$x)

#plotting first 2 principal components
biplot(pr.out, scale=0)
biplot(pr.out, scale=5) #has to be between 0 and 1
biplot(pr.out, scale=0.5)

pr.out$sdev

pr.var = pr.out$sdev^2
pr.var

#proportion of variance explained by each PC
pve = pr.var/sum(pr.var)
pve

#PCA IRIS
data ('iris')
head(iris)
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)
help("princomp")
#cor = True, uses correlation matrix rather than covariance
principal_components <- princomp(irisdata1, cor = TRUE, score = TRUE)
#each PC is calculated - 4 PC's
summary(principal_components)
plot(principal_components)
#line graph
plot(principal_components, type = 'l')
biplot(principal_components)
biplot(principal_components, scale =0)
biplot(principal_components, scale =1)


#PCA Boston
install.packages('MASS')
data(Boston, package="MASS")
pca_out <- prcomp(Boston,scale. = T)
pca_out
plot(pca_out)
plot(pca_out, type = 'l')
biplot(pca_out, scale = 0)
boston_pc <- pca_out$x
boston_pc
head(boston_pc)
summary(boston_pc)









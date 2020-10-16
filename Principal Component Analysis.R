library(ggplot2)
install.packages("cowplot")
library(cowplot) 
theme_set(theme_bw(base_size=12)) # set default ggplot2 theme
install.packages("dplyr")
library(dplyr)
install.packages("grid")
library(grid)

myiris<- read.table(file.choose(),sep=",", header=TRUE)
myirisnum <- myiris[1:4]
myirisnum  

covmatiris <- cov(myirisnum) 
print(round(covmatiris,2)) 

myPCA <- prcomp(myirisnum,center=T,scale.=T)
myPCA
myPCA_data <- data.frame(myPCA$x, class=myiris$class)
head(myPCA_data)
head(myPCA)
x <- myPCA$x[,1]
y <- myPCA$x[,2]
plot(x, y, xlab="PC1", ylab="PC2", main="Principal Component Analysis")
text(x, y, labels=row.names(myPCA), pos=4)

x <- myPCA$x[,1]
y <- myPCA$x[,2]
z <- myPCA$x[,3]
scatterplot3d(x, y, z, xlab="PC1", ylab="PC2", zlab="PC3", main="Principal Component Analysis")
text(x, y, z, labels=row.names(myPCA), cex = 0.7)

scatterplot3d(x, y, z, xlab="PC1", ylab="PC2", zlab="PC3", angle = 70, main="Principal Component Analysis")
text(x, y, z, labels=row.names(myPCA), cex = 0.7) 

covmatPCA <- cov(myPCA$x) 
sum(diag(covmatPCA)) 
covmatPCA
sum(diag(covmatiris)) 
summary(myPCA)

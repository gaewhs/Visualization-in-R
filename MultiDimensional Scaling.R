myiris<- read.table(file.choose(),sep=",", header=TRUE)

myirisnum <- myiris[1:4]
myirisnum   
str(myirisclass)

str(myirisnum)
#normalization
minimum <- apply(myirisnum,2,min) 
maximum <- apply(myirisnum,2,max)
myirisNORM <- scale(myirisnum,center=minimum,scale=(maximum-minimum)) 
mymyirisNORM <- as.data.frame(myirisNORM) 
summary(mymyirisNORM)
with(mymyirisNORM, pairs(mymyirisNORM))

set.seed(57)
myMDS <- cmdscale(dist(mymyirisNORM), 2, eig=TRUE)


x <- myMDS$points[,1] 
y <- myMDS$points[,2]
plot(x,  y,  xlab="Representative's  Coordinate  1",  ylab="Representative's  Coordinate  2", main="MDS")
text(x, y, labels=row.names(mymyirisNORM), pos=4, cex = 0.7)

#manhattan distance
myMDSManhattan <- cmdscale(dist(mymyirisNORM,method= "manhattan"), 2, eig=TRUE) 
x <- myMDSManhattan$points[,1]
y <- myMDSManhattan$points[,2]
plot(x,  y,  xlab="Repres ntative's  Coordinate  1",  ylab="Representative's  Coordinate  2",
     main="MDSManhattan")
text(x, y, labels=row.names(mymyirisNORM), pos=4, cex = 0.7)

#maximum distance
myMDSMaximum <- cmdscale(dist(mymyirisNORM,method= "maximum"), 2, eig=TRUE) 
x <- myMDSMaximum$points[,1]
y <- myMDSMaximum$points[,2]
plot(x,  y,  xlab="Representative's  Coordinate  1",  ylab="Representative's  Coordinate  2", main="MDSMaximum")
text(x, y, labels=row.names(mymyirisNORM), pos=4, cex = 0.7)


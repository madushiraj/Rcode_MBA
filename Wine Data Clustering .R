View(wine)      # view wine data after improting the file
winef1=wine    #duplicate wine data to winef1
View(winef1)   # view duplicated winef1 file
winef1$Class <- NULL                  #make first column null as it contains wine classes before normalizing data 
winef1.stand <- scale(winef1[-1])      # since inputs are in decimals and integers need to normalize values and assigned it to wine.stand
View(winef1.stand)                    # view the normalize data table winef1.stand
plot(winef1[,1:7])  
plot(winef1[,1:4])    #plot several columns to identify whether there are any patterns 
par(mfcol=c(1,2))     # plot 2 pgraphs to see if there any clustering opportunity
plot(wine.f1[,2:3],cex.main=0.75)
plot(wine.f1[,3:4],cex.main=0.75)
kc=kmeans(winef1[,2:7],3) 
plot(winef1[,2:3],col=kc$cluster,cex.main= 0.75)
kc
plot(wine[,2:4],cex.main=0.75)
kc=kmeans(wine[2:4],3)
plot(wine[,2:4],cex.main=0.75)
plot(wine[,2:3],col=kc$cluster)
points(kc$centers[,2:3],col=1:3, pch=8, cex=6)
table(wine$Class,kc$cluster)
attributes(kc)
kc$centers
kc
kc=kmeans(wine[2:6],3)
table(wine$Class,kc$cluster)

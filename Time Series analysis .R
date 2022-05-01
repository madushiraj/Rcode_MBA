## Age of Kings 
kings = scan("http://robjhyndman.com/tsdldata/misc/kings.dat", skip=3) #Skip first 3 lines
head(kings)
plot(kings) # R reads data set as a vector thus plotted a scatter plot 
View(kings)
tsKings = ts(kings) 
plot(tsKings)

## Birth patterns 
births = scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") 
head(births)
View(births)
tsBirths = ts(births, frequency=12, start=1946) #Start from 1946 repeat for 12 months 
plot(tsBirths, ylab="NYC births")
tsBirths2 = ts(births, frequency=6, start=1946) #Start from 1946 repeat for 12 months 
plot(tsBirths2, ylab="NYC births2")
tsBirths3 = ts(births, start=1946) #Start from 1946 repeat for 12 months 
plot(tsBirths3, ylab="NYC births3")
tsBirths1950 = window(tsBirths, start=c(1950,1), end=c(1955,12), frequency=12)
monthplot(tsBirths1950, ylab="NYC births between 1950 & 1955")

## Sales data in Queensland 
souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") 
tsSouv = ts(souvenir, frequency=12, start=c(1987,1))
tsLogSouv = log(tsSouv)
par(mfrow=c(1, 2))
plot(tsSouv, ylab="Souvenir sales") 
plot(tsLogSouv, ylab="Souvenir sales (log)")
fitBirth = decompose(tsBirths)
str(fitBirth)
plot(fitBirth, cex.main = 0.75)
tsBirthsNoRand = tsBirths - fitBirth$random #Remove random component 
tsBirthsNoSea = tsBirths - fitBirth$seasonal #Remove seasonal component 
par(mfrow=c(1, 2))
plot(tsBirthsNoRand, ylab="NYC births without random component")
plot(tsBirthsNoSea, ylab="NYC births without seasonal component")
par(mfrow=c(1, 1))
tsBirthsNoRandSea = tsBirths - fitBirth$random -fitBirth$seasonal #Remove random & seasonal components 
plot(tsBirthsNoRandSea, ylab="NYC births without random & seasonal components")
fitSouv = decompose(tsSouv) 
fitLogSouv =decompose(tsLogSouv) 
plot(fitSouv, cex.main = 0.75)
plot(fitLogSouv, cex.main = 0.75)


library(dplyr)
#Example from http://www.onthelambda.com/2014/02/10/how-dplyr-replaced-my-most-common-r-idioms/

#Load the data set:
setwd("C:\\Users\\Tobi\\Desktop\\R üben")
dat <- read.csv2("CrimeStatebyState.csv", header=T, sep=",")

#Structure/possible values:
str(dat) #Or to show really all possible values of all variables:

sapply(dat, levels) #base R
dat %>% sapply(levels) #dplyr

#Crimes in New York 2005:
ny_2005 <- dat[dat$Year==2005 & dat$State=="New York",] #base R

attach(dat) #base R, more elegant
ny_2005_2 <- dat[Year==2005 & State=="New York",]
identical(ny_2005, ny_2005_2) #TRUE
detach(dat)

ny_2005_3 <- subset(dat, Year==2005 & State=="New York") #base R, even more elegant

ny_2005_4 <- filter(dat, Year==2005, State=="New York") #dyplr. No real difference.

#Select columns:
ny_2005_type_count <- ny_2005[,c("Type.of.Crime","Count")] #base R
library(Hmisc)
ny_2005_type_count_2 <- ny_2005[Cs(Type.of.Crime, Count)] #Hmisc
ny_2005_type_count_3 <- select(ny_2005, Type.of.Crime, Count) #dplyr

#Create new variables based on existing ones:
ny_2005$Prop <- ny_2005$Count / sum(ny_2005$Count) #base R
ny_2005_4 <- mutate(ny_2005_4, Prop=Count/sum(Count)) #dplyr

#Rename variables:
#base R:
names(ny_2005_base)[names(ny_2005_base)=="Type.of.Crime"] <- "Type_of_Crime"

#dplyr:
rename(ny_2005_dplyr, Type_of_Crime=Type.of.Crime)

#Aggregate data:
summary1 <- aggregate(Count ~ Type.of.Crime, 
						data=ny_2005, 
						FUN=sum)
summary2 <- aggregate(Count ~ Type.of.Crime, 
						data=ny_2005, 
						FUN=length)
summary.crime.ny.2005 <- merge(summary1, summary2, by="Type.of.Crime") #base R

by.type <- group_by(ny_2005_4, Type.of.Crime)
summary.crime.ny.2005 <- summarise(by.type, num.types=n(), counts=sum(Count)) #dplyr.

#All together:

#base R:
dat <- read.csv2("CrimeStatebyState.csv", header=T, sep=",")
ny_2005_base <- subset(dat, Year==2005 & State=="New York")
ny_2005_base$Prop <- ny_2005_base$Count / sum(ny_2005_base$Count)
summary1 <- aggregate(Count ~ Type.of.Crime, 
						data=ny_2005_base, 
						FUN=sum)
summary2 <- aggregate(Count ~ Type.of.Crime, 
						data=ny_2005_base, 
						FUN=length)
final_base <- merge(summary1, summary2, by="Type.of.Crime")

#dplyr:
dat <- read.csv2("CrimeStatebyState.csv", header=T, sep=",")
ny_2005_dplyr <- filter(dat, Year==2005, State=="New York")
final_dplyr <- ny_2005_dplyr %>%
				filter(State=="New York", Year=="2005") %>%
				select(Type.of.Crime, Count) %>%
				mutate(Propotion = Count / sum(Count)) %>%
				group_by(Type.of.Crime) %>%
				summarise(num.types=n(), counts=sum(Count))

library(ggplot2)
library(GGally)
library(dplyr)
install.packages("stringi", repos="http://cran.rstudio.com/", dependencies=TRUE)
library(tidyverse)
library(tibble)
library(tidyr)
library(readr)


Batting_data <- read.csv("C:\\Users\\Dr. Suresh babu\\Desktop\\Prob Project\\English_batsmen_statistics.csv")

Batting_data

str(Batting_data)

#clean data
new_batting_data <- na.omit(Batting_data)

str(new_batting_data)

Experience <- new_batting_data$Career.End - new_batting_data$Career.Start

Experience

#appending experience
new_batting_data$experience = Experience 

str(new_batting_data)

new_batting_data <- filter(new_batting_data, new_batting_data$experience != 0)

#all batsmen having 3 years or more experience
experienced_batsmen <- new_batting_data[new_batting_data$experience > 3, ]

experienced_batsmen

str(experienced_batsmen)

Playing_Intensity <- experienced_batsmen$Matches.Played/experienced_batsmen$experience
Playing_Intensity

experienced_batsmen$playing_intensity = Playing_Intensity

experienced_batsmen
str(experienced_batsmen)

batting_success <- (experienced_batsmen$Hundreds.Scored + experienced_batsmen$Scores.Of.Fifty.Or.More 
                    - experienced_batsmen$Ducks.Scored) * experienced_batsmen$playing_intensity

experienced_batsmen$batting_sucess = batting_success

str(experienced_batsmen)

#remove everyone with negative batting success
experienced_batsmen_with_positive_batting_success <- filter(experienced_batsmen, experienced_batsmen$batting_sucess > 0)

str(experienced_batsmen_with_positive_batting_success)

consistency_avg <- (experienced_batsmen_with_positive_batting_success$Batting.Avg * experienced_batsmen_with_positive_batting_success$playing_intensity)
consistency_avg

experienced_batsmen_with_positive_batting_success$consistency_avg = consistency_avg

final_data_set <- experienced_batsmen_with_positive_batting_success

str(final_data_set)

final_data_set_sorted <- final_data_set[order(final_data_set$batting_sucess),]

str(final_data_set_sorted)

final_data_set_sorted

write.csv(final_data_set_sorted, file = 'final_data_set.csv')


actual_set <- read.csv("C:\\Users\\Dr. Suresh babu\\Desktop\\Prob Project\\final_data_set.csv")

actual_set$batting_sucess

actual_set <- actual_set[order(actual_set$batting_sucess, decreasing = TRUE ),]

actual_set$batting_sucess

top_20<-actual_set[1:20,]
#top_100<-actual_set[1:100,]

xaxis <- top_20[,'batting_sucess']
xaxis_names <-top_20[,'Player']
class(xaxis)
xaxis
xaxis_names

#barplot(xaxis, col = 'blue', xlab = xaxis_names)

op <- par(mar=c(11,4,2,2))
barplot(top_20$batting_sucess, names.arg = top_20$Player, las = 2, col = "lightblue", ylab = 'Batting Success'
        , main = "Best Batsman Of All Time")
rm(op)


meanOfBattingSuccess<-mean(actual_set$batting_sucess)
meanOfBattingSuccess

meanOfConsistencyAverage <- mean(actual_set$consistency_avg)
meanOfConsistencyAverage

#venn(2, universe=NA, small=0.7, showSetLogicLabel=FALSE,
#     simplify=FALSE, show.plot=TRUE, intersections=TRUE)


source("http://www.bioconductor.org/biocLite.R")
biocLite("limma")
library(limma)
library(ggforce)

Corr <- actual_set %>%
      select(Batting.Avg, consistency_avg)

ggpairs(Corr)

#ggplot(actual_set, aes(Batting.Avg, consistency_avg)) + geom_point()

H0 <- "Consistency Average of Indian Players is less than or equal to that 
of the consistency average of the rest of the world players"

H1 <- "Consistency Average of Indian Players is more than that of 
consistency average of the rest of the world players"

restOfTheWorldPlayers <- filter(actual_set, actual_set$Country != 'India')
IndianPlayers <- filter(actual_set, actual_set$Country == 'India')

meanOfRestOftheWorldCOnsistency <- mean(restOfTheWorldPlayers$consistency_avg)

IndianPLayersSample <- IndianPlayers[sample(nrow(IndianPlayers),25),]

meanOfIndianPLayersSampleConsistencyAvg <- mean(IndianPLayersSample$consistency_avg)
standardDeviationOfThePopulation <- sd(restOfTheWorldPlayers$consistency_avg)

standardDeviationOfSample <- sd(IndianPLayersSample$consistency_avg)
samplePopulationSize = 25

Zcalc <- (meanOfIndianPLayersSampleConsistencyAvg - meanOfRestOftheWorldCOnsistency)/ 
  (standardDeviationOfThePopulation/sqrt(samplePopulationSize))
Zcalc

pvalue <- 2 * pnorm(-abs(Zcalc))

tcalc <- (meanOfIndianPLayersSampleConsistencyAvg - meanOfRestOftheWorldCOnsistency)/ (standardDeviationOfSample/sqrt(samplePopulationSize))

pvalue_t <- 2 * pt(-abs(tcalc), 24)


#x <- actual_set$consistency_avg 
#class(x)

#h <- hist(x, breaks = 10, col = 'lightgreen', )
#xfit<-seq(min(x),max(x),length=40) 
#yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
#yfit <- yfit*diff(h$mids[1:2])*length(x) 
#lines(xfit, yfit, col="blue", lwd=2)

#d<- density(x)
#plot(d, main = )


#ANOVA

#INDIA <- subset(actual_set$consistency_avg, actual_set$Country == 'India')

#englandPlayers <- filter(actual_set, actual_set$Country == 'England')
#EnglandPlayerSample <- englandPlayers[sample(nrow(englandPlayers),43),]
#ENGLAND <- subset(EnglandPlayerSample$consistency_avg, EnglandPlayerSample$Country == 'England')

#aussiePlayers <- filter(actual_set, actual_set$Country == 'Australia')
#aussiePlayerSample <- aussiePlayers[sample(nrow(aussiePlayers),43),]
#AUSTRALIA <- subset(aussiePlayerSample$consistency_avg, aussiePlayerSample$Country == 'Australia')

#combined_groups <- data.frame(cbind(INDIA, ENGLAND, AUSTRALIA))
#combined_groups


#boxplot(combined_groups$INDIA ~ combined_groups$ENGLAND )
#boxplot(combined_groups$INDIA ~ combined_groups$AUSTRALIA )
#Stacked_Groups <- stack(combined_groups)
#Stacked_Groups

#Anova_results <- aov(values ~ ind, data = Stacked_Groups)
#Anova_results
#summary(Anova_results)


#TukeyHSD(Anova_results)

#plot(TukeyHSD(Anova_results))

df <- actual_set[,c("Country", "consistency_avg")]
df <- df[order(df$Country),]
df


plot(df$consistency_avg ~ df$Country, data = df, las = 2, xlab = "Countries", ylab = "Consistency of Averages",
     col = "orange", main = "Box Plot showing all the means")

cricket.aov <- aov (df$consistency_avg ~ df$Country, data = df)

summary(cricket.aov)

model.tables(cricket.aov, "mean")

TukeyHSD(cricket.aov)

plot(TukeyHSD(cricket.aov), las = 2 )





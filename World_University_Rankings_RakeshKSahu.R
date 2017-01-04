#Libraries

library(ggplot2) # Data visualization
library(dplyr)
library(rworldmap)
library(corrgram)
library(corrplot)
library(ggthemes)

#Uploading and formatting data
timesData <- read.csv('D:/Materials/Data Science/UpxAcademy/Project/world-university-rankings/timesData.csv')

#Functions

#Function to replace Inf va,ues with NA
impute.inf <- function(x) {
  if(any(is.infinite(x))){
    x[is.infinite(x)] <- NA
  } else {
    x
  }
  return(x)
}

#Function to replace NA with mean
impute.mean <- function(x) {
  z <- mean(x, na.rm = TRUE)
  if(is.numeric(x) & any(is.na(x))){
    x[is.na(x)] <- z
  } else {
    x
  }
  return(x)
}

#Calculate NA count
na_count <-sapply(timesData, function(y) sum(length(which(is.na(y)| y =="NA"| y ==""))))
na_count

#Convert factors to numeric variables
timesData$international = as.numeric(gsub("-", "", timesData$international))
timesData$income = as.numeric(gsub("-", "", timesData$income))
timesData$total_score = as.numeric(gsub("-", "", timesData$total_score))
timesData$num_students = as.numeric(sub(pattern=",", replacement="",timesData$num_students))
timesData$international_students = as.numeric(gsub("%", "", timesData$international_students))
female_male_ratio_list = strsplit(as.character(timesData$female_male_ratio), ":")

#convert female_male_ratio column which is currently in : format to Female/Male
female_male_ratio_1 = list()
for (i in 1:length(female_male_ratio_list)){
  female_male_ratio_1[[paste0("element", i)]] = as.numeric((as.numeric(female_male_ratio_list[[i]][1]))/ (as.numeric(female_male_ratio_list[[i]][2])))
  i = i+1
}
timesData$female_male_ratio = as.numeric(female_male_ratio_1)

#Calling impute.inf function to replace Inf values with NA
timesData$female_male_ratio <- impute.inf(timesData$female_male_ratio)

#Calling impute.mean function to replace NAs with respective mean values
timesData$international <- impute.mean(timesData$international)
timesData$income <- impute.mean(timesData$income)
timesData$num_students <- impute.mean(timesData$num_students)
timesData$student_staff_ratio <- impute.mean(timesData$student_staff_ratio)
timesData$international_students <- impute.mean(timesData$international_students)
timesData$female_male_ratio <- impute.mean(timesData$female_male_ratio)
timesData$female_male_ratio <- round(timesData$female_male_ratio, digits=2)

#Calculation of total scores using the Times methodology
timesData$total = timesData$teaching *0.3 + timesData$research*0.3 +timesData$citations*0.3 + timesData$international *0.075 + timesData$income*0.025
timesData$total_score <- ifelse(is.na(timesData$total), timesData$total_score, timesData$total)
timesData$total <- NULL

timesData1 <- timesData

attach(timesData1)

#Outliers detection and correction
#Before outlier removal
ggplot(timesData1, aes(num_students))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)
ggplot(timesData1, aes(female_male_ratio))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)
ggplot(timesData1, aes(student_staff_ratio))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)

#There are some extreme values in both num_students, female_male_ratio and student_staff_ratio. Therefore, it is very important to, at least, delete extreme values. 
timesData1 <- timesData1[timesData1$num_students<90000 & timesData1$female_male_ratio<2.5 & timesData1$student_staff_ratio < 90,]

#After outlier removal
ggplot(timesData1, aes(num_students))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)
ggplot(timesData1, aes(female_male_ratio))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)
ggplot(timesData1, aes(student_staff_ratio))+ geom_density(alpha = 0.3, fill = "yellow", colour = "black", size = 0.9)

#Correaltion between features
corrgram(timesData1[,c(4:13)], lower.panel = panel.pie, upper.panel = panel.pts, main="Correlogram")

corr<-cor(timesData1[,c(4:13)])
corrplot(corr, type="upper")

# Top 25 countries by World Rank
for (i in unique(timesData1$year)){
  plot1 <- ggplot(timesData1[timesData1$year == i,][1:25,], 
                  aes(x=reorder(as.factor(world_rank)), y=university_name, color=country))+
    geom_point(aes(colour = factor(country))) +
    geom_point(size = 4) +
    labs(list(title = paste("Top 25 countries by World Rank in ", i), x="World Rank", y="University Name")) +
    theme_pander() +
    scale_colour_pander()
  print(plot1)
}

# Top 25 countries by International Students
for (i in unique(timesData1$year)){
  plot2 <- ggplot(timesData1[timesData1$year == i,][1:25,],
                  aes(x=reorder(as.factor(international_students)), y=university_name, color=country))+
    geom_point(aes(colour = factor(country))) +
    geom_point(size = 4) +
    labs(list(title = paste("Top 25 countries by International Students in ", i), x = "International Students %", y = "University Name")) +
    theme_pander() +
    scale_colour_pander()
  print(plot2)
}

# Top 25 countries by Female students
for (i in unique(timesData1$year)){
  plot3 <- ggplot(timesData1[timesData1$year == i,][1:25,],
                  aes(x=as.factor(female_male_ratio), y=university_name, color=country))+
    geom_point(aes(color = factor(country)))+
    geom_point(size = 4) +
    labs(list(title = paste("Top countries by Female students in ", i), x = "Female/Male ratio", y = "University Name")) +
    theme_pander() +
    scale_colour_pander()
  print(plot3)
}

# Top 10 Countries with Most Universities Ranked each year 2011-2016
for (yr in unique(timesData1$year)) {
  sub <- subset(timesData1, year==yr) %>% group_by(year, country) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>%head(10)  
  print(ggplot(sub, aes(x = reorder(country, -Count), y = Count)) + geom_bar(stat = "identity", fill="steelblue") + xlab(yr) + ylab("Number of Universities Ranked")+ geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5) + theme_pander() +     scale_colour_pander())
}

# create a data frames with the all universities in TimesData and count them by country
timesCount <- data.frame(table(timesData1$country, timesData1$year))
colnames(timesCount) <- c("country","year","count")
timesCount$year <- as.numeric(as.character(timesCount[,'year']))

#Countries by number of universities in TimesData
for (i in 2011:2016){
  p1 = ggplot(timesCount[timesCount$year == i,], aes(x=reorder(country, -count), y=count, fill=country))
  p1 = p1 + geom_bar(stat="identity") + coord_flip() + labs(x="Country",y="Count") + ggtitle(paste("Countries by number of universities in TimesData in ", i)) + theme(legend.position="none")
  print(p1)
}

# World distribution of countries(in World map)
pdf("D:/Materials/Data Science/UpxAcademy/Project/world-university-rankings/Output/Country_Map.pdf", onefile = TRUE, width=11, height=8)
country_map <- joinCountryData2Map(timesCount, joinCode="NAME", nameJoinColumn="country")
mapParams <- mapCountryData(country_map,addLegend = T, 
                            nameColumnToPlot="count",catMethod="fixedWidth",missingCountryCol="white",oceanCol="lightsteelblue2",colourPalette="rainbow",mapTitle="World Distribution of Universities")

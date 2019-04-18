#Step1 : Show the total number of rows, the structure of the data frame, and first 10 rows of the data frame containing all of the NIPostcode data.

setwd("~/CA2")
#Reading the  NI postcode dataset
NIpostcodesdata <- read.csv("postcodes.csv", header = FALSE)
#Shows the total number of rows in NI postcode dataset
nrow(NIpostcodesdata)
#Shows the structure of NI postcode dataset
str(NIpostcodesdata)
#Shows the first ten rows of the NI postcode dataset
head(NIpostcodesdata, n=10)

#Step2 : Add a suitable title for each attribute of the data.
#Assigning Column names to the dataset
colnames(NIpostcodesdata) <- c("Organisation_Name", "Sub-building_Name", "Building_Name", "Number", "Primary_Thorfare", "Alt_Thorfare", "Secondary_Thorfare", "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary_key(identifier)")
#Displaying the dataset
head(NIpostcodesdata, n=10)
str(NIpostcodesdata)


#Step3 : Remove or replace missing entries with a suitable identifier. Decide whether it is best to remove missing data or to recode it.
#The NIpostcodes dataset has many blank values in most of the cloumns. Removing these blanks will omit 95% of the data. Replacing the empty values with 'NA' is the best option.
#Replacing the empty values with 'NA'.
NIpostcodesdata[NIpostcodesdata==""] <- NA
head(NIpostcodesdata)
str(NIpostcodesdata)

#Lets remove the all the rows with 'NA' values in postcode column of the dataset
table(is.na(NIpostcodesdata$Postcode))
NIpostcodesdata <- NIpostcodesdata[!is.na(NIpostcodesdata$Postcode), ]
table(is.na(NIpostcodesdata$Postcode))

#Step4 : Show the total number and mean missing values for each column in the postcode data frame.

#Showing the total number of NA values in each column of the postcode data frame.
sum(is.na(NIpostcodesdata$Organisation_Name))
sum(is.na(NIpostcodesdata$`Sub-building_Name`))
sum(is.na(NIpostcodesdata$Building_Name))
sum(is.na(NIpostcodesdata$Number))
sum(is.na(NIpostcodesdata$Primary_Thorfare))
sum(is.na(NIpostcodesdata$Alt_Thorfare))
sum(is.na(NIpostcodesdata$Secondary_Thorfare))
sum(is.na(NIpostcodesdata$Locality))
sum(is.na(NIpostcodesdata$Townland))
sum(is.na(NIpostcodesdata$Town))
sum(is.na(NIpostcodesdata$County))
sum(is.na(NIpostcodesdata$Postcode))
sum(is.na(NIpostcodesdata$`x-coordinates`))
sum(is.na(NIpostcodesdata$`y-coordinates`))
sum(is.na(NIpostcodesdata$`Primary_key(identifier)`))
sum(is.na(NIpostcodesdata))
#Calculaing the mean of NA values in each column of the postcode data frame.
mean(is.na(NIpostcodesdata$Organisation_Name))
mean(is.na(NIpostcodesdata$`Sub-building_Name`))
mean(is.na(NIpostcodesdata$Building_Name))
mean(is.na(NIpostcodesdata$Number))
mean(is.na(NIpostcodesdata$Primary_Thorfare))
mean(is.na(NIpostcodesdata$Alt_Thorfare))
mean(is.na(NIpostcodesdata$Secondary_Thorfare))
mean(is.na(NIpostcodesdata$Locality))
mean(is.na(NIpostcodesdata$Townland))
mean(is.na(NIpostcodesdata$Town))
mean(is.na(NIpostcodesdata$County))
mean(is.na(NIpostcodesdata$Postcode))
mean(is.na(NIpostcodesdata$`x-coordinates`))
mean(is.na(NIpostcodesdata$`y-coordinates`))
mean(is.na(NIpostcodesdata$`Primary_key(identifier)`))
mean((is.na(NIpostcodesdata)))

#Step5 : Modify the County attribute to be a categorising factor.
class(NIpostcodesdata$County)
NIpostcodesdata$County <- factor(NIpostcodesdata$County)
class(NIpostcodesdata$County)
str(NIpostcodesdata)

#Step6 : Move the primary key identifier to the start of the dataset.
head(NIpostcodesdata)
NIpostcodesdata<-NIpostcodesdata[,c(15, 1:14)]
head(NIpostcodesdata)
str(NIpostcodesdata)

#Step7 : Create a new dataset called Limavady_data. Store within it only information that has 
#locality, townland and town containing the name Limavady. Store this information in an 
#external csv file called Limavady.

library(data.table)
#creating limavady dataset 
Limavady_data <- fread("postcodes.csv", select = c(8,9,10), header = FALSE)
head(Limavady_data)
colnames(Limavady_data) <- c("Locality", "Townland", "Town")
head(Limavady_data)
nrow(Limavady_data)

#Filtering out the data with Limavady town.
Limavady_data <- Limavady_data[Limavady_data$Town == 'LIMAVADY',]
nrow(Limavady_data)
head(Limavady_data)
str(Limavady_data)

#exporting the Limavady dataset.
write.csv(Limavady_data, "Limavady.csv")

#Step8 : Save the modified NIPostcode dataset in a csv file called CleanNIPostcodeData. 
head(NIpostcodesdata)
write.csv(NIpostcodesdata, "CleanNIPostcodeData.csv")
str(NIpostcodesdata)

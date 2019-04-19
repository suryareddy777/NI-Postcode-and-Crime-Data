#Step1 : Using R, amalgamate all of the crime data from each csv file into one dataset. Save this dataset into a csv file called AllNICrimeData. Count and show the number of rows in the AllNICrimeData dataset.

#Extarcting all the NI Crime datasets into on 'NI Crime Data' folder
zipfile <- "Ni Crime Data.Zip"
unzip(zipfile)

#Setting the working directory to read all the NI crime data datasets
setwd("/Users/suryareddy/CA2/NI Crime Data")

#To list all the NI crime datasets
csv_files <- list.files(full.names = TRUE, recursive = TRUE)
csv_files

#combining the all the crime datasets
AllNICrimeData <- Reduce(rbind, lapply(csv_files, read.csv)) 

#Setting the working directory back to Previous
setwd(("/Users/suryareddy/CA2"))
getwd()

#Displaying the combined 'AllNICrimeData' dataset
head(AllNICrimeData)
nrow(AllNICrimeData) 

#Saving "AllNICrimeData" file to the working directory
write.csv(AllNICrimeData, "/Users/suryareddy/CA2/AllNICrimeData.csv")

str(AllNICrimeData)

#Step2 :  Modify the structure of the newly created AllNICrimeData csv file and remove the following attributes: CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context. Show the structure of the modified file.

#To View the total number of colunms in the dataset
ncol(AllNICrimeData)
#To filter out the intended coulmns
AllNICrimeData <- AllNICrimeData[ -c(1, 3, 4, 8, 9, 11, 12) ]
head(AllNICrimeData)
ncol(AllNICrimeData)
str(AllNICrimeData)

#Step3 : Factorise the Crime type attribute. Show the modified structure.
class(AllNICrimeData$Crime.type)
AllNICrimeData$Crime.type <- factor(AllNICrimeData$Crime.type, order = FALSE)
class(AllNICrimeData$Crime.type)
str(AllNICrimeData)

#Step4 :  Modify the AllNICrimeData dataset so that the Location attribute contains only a street name. For example, the attribute value “On or near Westrock Square” should be modified to only contain “Westrock Square”. Modify the resultant empty location attributes with a suitable identifier.

#Modifying the Location attribute
AllNICrimeData$Location <- gsub("On or near ", "", AllNICrimeData$Location)
head(AllNICrimeData)
#Checking the total number of rows in Location column
nrow(AllNICrimeData[4])
#Checking the total number of blank elements in Location column
sum(AllNICrimeData$Location == "")
#Filling the Blank elements in Location column with "NA"
AllNICrimeData$Location[AllNICrimeData$Location == ""] <- NA
sum(AllNICrimeData$Location == "")
head(AllNICrimeData)
#Chaniging the Location attribute to Uppercase.
AllNICrimeData$Location <- toupper(AllNICrimeData$Location)
head(AllNICrimeData)

#Step5 : Create random_sample_crime dataset from 1000 random samples of AllNICrimeData. Then creating a function that takes as an input each location attribute from random_crime_sample and finds a suitable postcode value from the postcode dataset.

sum(is.na(AllNICrimeData$Location))
#Deleting the rows with NA's in the Location column
AllNICrimeData <- AllNICrimeData[!(is.na(AllNICrimeData$Location)),]
sum(is.na(AllNICrimeData$Location))
sum(AllNICrimeData$Location == "")
head(AllNICrimeData)

#Creating a dataset with 1000 random samples from 'AllNICrimeData'.
random_crime_sample <- AllNICrimeData[sample(nrow(AllNICrimeData), 1000),]
#random_crime_sample <- na.omit(random_crime_sample)
nrow(random_crime_sample)
head(random_crime_sample)
sum(is.na(random_crime_sample$Location))

#Reading postcode CSV file
CleanNIPostcode <-read.csv("CleanNIPostcodeData.csv", stringsAsFactors = FALSE)
head(CleanNIPostcode)
ncol(CleanNIPostcode)

install.packages("dplyr")
library(dplyr)

#Creating a new dataset with Primary_Thorfare and Postcode columns of CleanNIPostcode dataset.
postcode_loc <- select(CleanNIPostcode, Primary_Thorfare, Postcode)
head(postcode_loc)
str(postcode_loc)
#Creating a new dataset using tibble R's 'tbl_df' object for providing a nicer printing method.
Data_thpost <- tbl_df(postcode_loc)
str(Data_thpost)
head(Data_thpost)

#Creating Postcode function to filter thr rows of NI_Thor_Postcode

find_a_postcode <- lapply(random_crime_sample$Location, function(Location) {
  matchdata <- filter(Data_thpost, Primary_Thorfare == Location)
  result <- names(which.max(table(matchdata$Postcode)))
  return(result)
})

#Step6 :Append the data output from the function to the random_crime_sample dataset and save it as csv file.

result <- as.character(find_a_postcode)
random_crime_sample$Postcodes <- result
head(random_crime_sample)
nrow(random_crime_sample)
str(random_crime_sample)
#Saving the modified dataset as csv file
write.csv(random_crime_sample, "random_crime_sample.csv")


#Step7 :update the random sample so that it contains only the following items
#Creating a updated random_crime_sample dataset
updated_random_sample <- subset(random_crime_sample, select = c(Month, Longitude, Latitude, Location, Crime.type, Postcodes))
head(updated_random_sample)
#Creating Chart_data datset by sorting the postcodes and crime types in order.

chart_data <- updated_random_sample[order(updated_random_sample$Postcodes),]
chart_data <- updated_random_sample[order(updated_random_sample$Crime.type),]
head(chart_data)
summary(chart_data$Crime.type)


#Step8 : Bar Plot for Crime Types of Chart_data.

library(tidyr)
#Preprocessing the data to create a bar garph
stack(summary(chart_data$Crime.type))
final_data <- stack(summary(chart_data$Crime.type))
colnames(final_data) <- c( "Counts", "Crime.Types")
final_data <- final_data[c(2,1)]
head(final_data, n=15)
ncol(final_data)

#Starting the necessary libraries
library(data.table)
library(ggplot2)
library(lubridate)
library(plotly)

#Plotting the Bar graph for the final output.
dev.off()
ggplot(final_data,aes(Crime.Types,Counts)) + geom_bar(stat = "identity", fill="cadetblue") + 
  scale_x_discrete(limits=final_data$Crime.Types, labels=final_data$Crime.Types) + coord_flip() + 
  ggtitle("CrimeCounts")














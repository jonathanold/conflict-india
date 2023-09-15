install.packages("ggmap")
install.packages("readxl")
library("readxl")
library("ggmap") 
library(tidyr) 

#Now you can give city name or country name individually
register_google(key = "AIzaSyBQLZ-BWrndxJw4bj0rGFrPdAVlEl9DKko", write = TRUE)

setwd("/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Econ History/Resource Curse/Data/")

data_1910 = read_excel("./Coal by Mine/1910.xlsx")


# data_1910 <- subset (data_1910, select = -(Geocode))

data_1910 = separate(data_1910, 'Geocode', paste("Geocode", 1:2, sep=","), sep=",", extra="drop")
data_1910$lat_manual = data_1910$"Geocode,1"
data_1910$long_manual = data_1910$"Geocode,2"
drops <- c("Geocode,1","Geocode,2")
data_1910 = data_1910[ , !(names(data_1910) %in% drops)]



# data_1910$geo = paste(data_1910$District, data_1910$Mine)
data_1910$geo = paste(data_1910$District, "District,",data_1910$Mine)


data_1910$coordinates = geocode(data_1910$geo, 
                                 inject=c("region=in&components=country:IN"))


# administrative_area:Helsinki|
write.csv(data_1910, file="./Coal by Mine/1910_geocoded.csv")







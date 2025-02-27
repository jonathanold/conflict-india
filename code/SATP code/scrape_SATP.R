library(rvest)
library(httr)

# Set up
months = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
years = seq(2001,2023,1)
letters_to_delete <- c("“", "”", "‘", "’","'","\nRead less...")
pattern <- paste0(letters_to_delete, collapse = "|")

#"\\(","\\)", 

# Download data from year 2000
year = 2000
download_data <- function(month) {
  # Generate a dataset using the string
  theurl = paste0("https://www.satp.org/terrorist-activity/india-",month,"-",year)
  t1  = html_table(read_html(GET(url=theurl)))[[1]]
  colnames(t1) <- c("date", "short", "long")
  t1$long = NULL
  t1$date = gsub("&nbsp", "", t1$date)
  t1$date = gsub(" - ", " ", t1$date)
  t1$short = gsub("\nRead less...", "", t1$short)
  t1$short = sub(".*\nRead more...\n\n\n", "", t1$short)
  t1$short = gsub(pattern, "", t1$short)
  
  t1$year = year
  return(t1)
}


datasets = lapply(months, download_data)
dataset_satp <- do.call(rbind, datasets)
colnames(dataset_satp) <- c("date","description", "year" )

dataset_satp_complete = dataset_satp



# Download data for all other years and append to 2000 data

for (year in years) {
# Function to scrape and superficially clean data
download_data <- function(month) {
  # Generate a dataset using the string
  theurl = paste0("https://www.satp.org/terrorist-activity/india-",month,"-",year)
  t1  = html_table(read_html(GET(url=theurl)))[[1]]
  colnames(t1) <- c("date", "short", "long")
  t1$long = NULL
  t1$date = gsub("&nbsp", "", t1$date)
  t1$date = gsub(" - ", " ", t1$date)
  t1$short = gsub("\nRead less...", "", t1$short)
  t1$short = sub(".*\nRead more...\n\n\n", "", t1$short)
  t1$short = gsub(pattern, "", t1$short)

  t1$year = year
  return(t1)
}


datasets = lapply(months, download_data)
dataset_satp <- do.call(rbind, datasets)
colnames(dataset_satp) <- c("date","description", "year" )

dataset_satp_complete = rbind(dataset_satp, dataset_satp_complete)
print(year)
}


dataset_satp_complete$description = gsub(";", ".", dataset_satp_complete$description)
dataset_satp_complete$date = NULL
write.table(dataset_satp_complete,
            quote=FALSE,
            sep=";",
            row.names=FALSE,
          file="/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/dataset_satp_complete.csv",
          fileEncoding = "UTF-8")
dataX = dataset_satp_complete
dataX$description = gsub(pattern, "", dataX$description)
dataX$description = gsub("&", "", dataX$description)
dataX$description = gsub("-", " ", dataX$description)


save(dataX,         
     file="/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/dataset_satp_complete.RData")

load(file="/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/dataset_satp_complete.RData")


dataX$description = gsub(";", ",", dataX$description)

data2019 = subset(dataX, dataX$year==2019)

write.table(data2019,
            quote=FALSE,
            sep=";",
            row.names=FALSE,
            file="/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Github/conflict-india/data/dataset_satp_2019.csv",
            fileEncoding = "UTF-8")


library(data.table)
library(dplyr)
library(writexl)
set.seed(20231017)
load("/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/dataset_satp_complete.RData")
dataX$id_satp <- paste0("ID_", 1:nrow(dataX))

dataX <- subset(dataX, year != 2023)

dataX$note <- tolower(dataX$description)
# Remove special characters, periods, commas, etc.
dataX$note <- gsub("-", " ", dataX$note)
dataX$note <- gsub("[^a-z0-9 ]", "", dataX$note)

string_vector <- c(" abduct", " ambush", " arrest", " assassin", " assault", " attack", " attempt", 
                   " beat up", " beaten", " beaten up", " blast", " blew up", " blow up", " blowing up", 
                   " blown up", " bomb", " bombard", " boycott", " brand", " burn", " burnt", " burnt down", 
                   " bust", " carried out", " claim", " clash", " comb", " damag", " defus", " demolish", 
                   " desert", " detain", " deton", " encount", " ensu", " erupt", " escap", " execut", 
                   " explod", " extort", " fight", " fire", " fled", " flee", " gunned down", " hijack", 
                   " hit", " hurl", " hurt", " imprison", " improvis", " infiltr", " injur", " intimid", 
                   " kidnap", " kill", " laid", " laid down", " launch", " lob", " loot", " lynch", " massacr", 
                   " murder", " neutral", " neutralis", " propag", " protest", " raid", " rape", " recov", 
                   " retali", " rob", " seiz", " set", " set ablaz", " shell", " shot", " shot down", " shut down", 
                   " slit", " smash", " stab", " storm", " strike", " struck", " struggl", " succumb", " suffer", 
                   " surrend", " sustain", " threaten", " tipped off", " torch", " tortur", " trap", " trigger", 
                   " went off", " wound", " agitat", " deploy", "  vandalis", " dead")

# Filter rows in dataX
filtered_dataX <- dataX[sapply(dataX$note, 
                               function(desc) any(sapply(string_vector, grepl, x=desc))), ]


# Find rows in dataX that are not in filtered_dataX
difference_data <- anti_join(dataX, filtered_dataX)



# Random sampling.
sampled_data <- data.frame()

for (year in 2000:2022) {
  subset_data <- dataX[dataX$year == year, ]
  
  # Sample rows containing " caste" and " mla"
  caste_row <- subset_data[sample(which(grepl(" caste", subset_data$note, ignore.case = TRUE)), 1), ]
  mla_row <- subset_data[sample(which(grepl(" mla", subset_data$note, ignore.case = TRUE)), 1), ]
  pan_row <- subset_data[sample(which(grepl(" panchay", subset_data$note, ignore.case = TRUE)), 1), ]
  pol_row <- subset_data[sample(which(grepl(" politic", subset_data$note, ignore.case = TRUE)), 2), ]
  ele_row <- subset_data[sample(which(grepl(" elect", subset_data$note, ignore.case = TRUE)), 2), ]
  
  # Exclude the already selected rows for further random sampling
  remaining_data <- subset_data[!(rownames(subset_data) %in% rownames(rbind(caste_row, mla_row, pan_row, pol_row, ele_row))), ]
  
  if (year <= 2013) {
    n_samples <- 45-7  # 45 - 2 (since we've already selected 2 rows)
  } else {
    n_samples <- 30-7  # 30 - 2
  }
  
  # Ensure we don't try to sample more rows than are available
  n_samples <- min(n_samples, nrow(remaining_data))
  
  sampled_subset <- remaining_data[sample(1:nrow(remaining_data), n_samples, replace = FALSE), ]
  
  sampled_data <- rbind(sampled_data, caste_row, mla_row, pan_row, pol_row, ele_row, sampled_subset)
  cat(sprintf("Year: %d, Sampled: %d\n", year, nrow(sampled_data)))  # +2 for "caste" and "mla" rows
  
}

# The sampled_data dataframe now contains the randomly sampled observations
sampled_data$relevant_event = ""
sampled_data$multiple_events = ""
sampled_data$internal_conflict = ""
sampled_data$number_casualties = ""
sampled_data$number_injuries = ""
sampled_data$pacification_event = ""
sampled_data$economic_conditions = ""
sampled_data$political_demands = ""
sampled_data$communal_conflict = ""
sampled_data$caste_conflict = ""
sampled_data$naxalite_maoist = ""
sampled_data$organized_crime = ""

# Names of the people
people <- c("Varun", "Vaidehi", "Jonathan", "Jade", "Christine", "Umar")

# Assign 100 rows to each person
assignments_individual <- rep(people, each = 100)

# Create all possible combinations of two people
combinations <- combn(people, 2)


# Randomly assign the remaining 300 rows to these combinations
assignments_combinations <- rep(1:ncol(combinations), each = 300/ncol(combinations)) # 300/6 = 50
assignments_combinations <- sample(assignments_combinations) # Shuffle the assignments

# Convert the combination indices to actual names
assignments_combinations_names <- apply(combinations[, assignments_combinations], 2, paste, collapse = " & ")

# Combine individual and combination assignments
all_assignments <- c(assignments_individual, assignments_combinations_names)

shuffled_assignments <- sample(all_assignments, 900)

# Add the assignments to the sample data
sampled_data$assignment <- shuffled_assignments

sampled_data$note=NULL
sampled_data$note = ""

filename <- paste0("/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/humancoding/allevents.xlsx")
write_xlsx(sampled_data, filename)


# Loop through each person's name
for (person in people) {
  # Subset rows that contain the person's name in the assignment column
  subset_data <- sampled_data[grep(person, sampled_data$assignment), ]
  subset_data$assignment=NULL
  subset_data$year=NULL
  
  sampled_data <- sampled_data[sample(nrow(sampled_data)), ]
  
  # Store the subset data in the list with the person's name as the key
  filename <- paste0("/Users/jonathanold/Library/CloudStorage/GoogleDrive-jonathan_old@berkeley.edu/My Drive/_Berkeley Research/Reservations and Conflict/Data/_gen/humancoding/sample_events_", person, ".xlsx")
  
  # Export the dataset to the Excel file
  write_xlsx(subset_data, filename)
}


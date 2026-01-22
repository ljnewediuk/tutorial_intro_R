
### Tasks for them to do

# set a working directory

# load the phys_data.csv file from their working directory

# create a new folder in their existing working directory called "data", and 
# save the data there as a .csv

# Print the first 11 rows of the phys data using the head() function
head(phys_data, 11)

# Create a new data frame "mouse_data" with only Mus musculus
mouse_data <- phys_data[phys_data$species == "Mus musculus" ,]

# Create a new data frame "terrestrial_data" without Oncorhynchus mykiss
terrestrial_data <- phys_data[phys_data$species != 'Oncorhynchus mykiss' ,]

# Use code to print out:

# a) the metabolic rate of Parus major B08
phys_data[24,][4]

# b) All data from individual F02
phys_data[phys_data$individual_id == "F02" ,]
# OR
phys_data[9,]

# c) The first six entries in the sprint speed column
head(trial_data[trial_data$sprint_speed_cm_s ,])



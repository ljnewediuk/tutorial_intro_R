
### Scrap code--tasks they will use in their data cleaning

library(tidyverse)

tusks <- read_csv('datasets/elephant_tusks.csv')

# They will be told their task is to:
# Compare the mean and standard deviation of tusk lengths and circumferences for
# ADULT (≥ 12 years for females, 10 for males) males and females separately between the two sampling 
# periods.

# We'll work on basic data cleaning tasks. They will also do some basic pivoting 
# and selecting (remove age and shoulder height, get the lengths and 
# circumferences of tusks into a single column)

# Remove the NAs (use verb "any_of" to remove only NAs related to tusks)
tusks_no_na <- tusks %>%
  drop_na(any_of(c('tusk_length', 'tusk_circumference')))

# They will check which rows are duplicates (I duplicated three rows)
elephant_dups <- tusks_no_na %>%
  group_by(sample_period, 
           sex, 
           estimated_age, 
           shoulder_height, 
           tusk_length, 
           tusk_circumference) %>%
  filter(n() > 1)

# Look at the duplicates, and now that they understand the problem (duplicate
# data entry), they can remove them
tusks_distinct <- tusks_no_na %>%
  distinct()

# Removed three rows, which were the number of duplicates (6)

# Mutate to add column for adult elephants (only those 12 or older)
# Also tell them this is a convenient way to rename values in a column that 
# don't fit the naming pattern in the rest of the column
tusks_with_age_class <- tusks_distinct %>%
  mutate(age_class = case_when(sex == 'f' & estimated_age >= 12 ~ 'ADULT',
                               sex == 'f' & estimated_age < 12 ~ 'JUVENILE',
                               sex == 'm' & estimated_age >= 10 ~ 'ADULT',
                               sex == 'm' & estimated_age < 10 ~ 'JUVENILE'))

# They need to filter only the adults
tusks_adults <- tusks_with_age_class %>%
  filter(age_class == 'ADULT')

# Select out shoulder heights and estimated ages; at this point we have our final
# cleaned dataset
tusks_cleaned <- tusks_adults %>%
  select(! c(estimated_age, shoulder_height, age_class))

# Pivot longer (We will go through the parts of the pivot)
tusks_long <- tusks_cleaned %>%
  pivot_longer(cols = c('tusk_length', 'tusk_circumference'),
               names_prefix = 'tusk_',
               names_to = 'tusk_measurement',
               values_to = 'cm')

# Next we will group by and summarize the mean and sd
tusks_long %>%
  group_by(sample_period, sex, tusk_measurement) %>%
  summarize(mean_cm = mean(cm),
            sd_cm = sd(cm))

# ALSO NEED TO ADD:
#   RENAME
#   ARRANGE

# These will just be trivial operations


# Generate mock data on an oil spill

set.seed(42)

# Define structure
years <- 2012:2024
species <- c("Cod", "Herring", "Flounder", "Mackerel")

dat <- expand.grid(
  year = years,
  species = species
)

# Spill indicator
dat$spill <- ifelse(dat$year < 2018, "before", "after")
dat$spill <- factor(dat$spill)

# Oil amount (low before spill, spike after, then decay)
dat$oil_amount <- ifelse(
  dat$year < 2018,
  rnorm(nrow(dat), mean = 2, sd = 0.5),
  rnorm(nrow(dat),
        mean = 15 * exp(-(dat$year - 2018) / 3),
        sd = 1.5)
)

dat$oil_amount <- pmax(dat$oil_amount, 0.1)  # prevent negatives

# Fishing effort (slow increase over time)
dat$effort <- rnorm(nrow(dat), mean = 50, sd = 5) +
  (dat$year - min(dat$year)) * 0.5

# Species baselines
baseline <- c(
  Cod = 120,
  Herring = 200,
  Flounder = 90,
  Mackerel = 160
)

# Species sensitivity to oil
sensitivity <- c(
  Cod = 0.8,
  Herring = 0.3,
  Flounder = 0.6,
  Mackerel = 0.2
)

# Generate catch (tons landed)
dat$catch <- baseline[dat$species] +
  0.9 * dat$effort -                  # effort increases catch
  sensitivity[dat$species] * 
  dat$oil_amount * 5 +               # oil reduces catch
  rnorm(nrow(dat), mean = 0, sd = 10)  # noise

# Clean up
dat$catch <- round(dat$catch, 1)

head(dat)

# Variable meaning:
# 
# effort: Vessels/day
# catch: Kilograms fish landed
# oil_amount: Kilometers squared coverage
# spill: Before or after
# year: Year of measurement
# species: Commercial fishery species

# Save the data
write.csv(dat, "datasets/oil_spill.csv")

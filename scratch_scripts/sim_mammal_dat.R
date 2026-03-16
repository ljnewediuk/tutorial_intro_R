
# Simulate dataset of Pika (Ochotona princeps) in Waterton Lakes National Park
# 
# Hypothesis: Pika fitness (survival and reproductive success) is driven by health and resource availability.
# 
# data are: 
# 
#   habitat_quality (factor)
#   sex (factor)
#   age (integer)
#   territory_size (m^2)
#   body_mass (g)
#   stress_hormone (ng/g glucocorticoid metabolites)
#   parasite_load (count)
#   offspring_count (count)
#   foraging_time (seconds)
#   survived (binomial 1/0)
#   
# 
# Relationships in data are:
# 
# Higher habitat quality = higher body mass (LM)
# Parasites reduce offspring count (NB)
# Survival increases with body mass (logistic)
# Older animals have more parasites (Poisson/NB)
# Habitat quality affects stress hormone (Gamma)
# Territory size predicts body mass (LM)

set.seed(123)

n <- 180

habitat_quality <- factor(sample(c("low","medium","high"), n, replace=TRUE))
sex <- factor(sample(c("F","M"), n, replace=TRUE))

age <- rnorm(n, 3, 1)

territory_size <- rnorm(n, 50, 10)

body_mass <- 20 +
  3*territory_size +
  4*(habitat_quality=="high") +
  2*(habitat_quality=="medium") +
  0.5*age +
  rnorm(n,0,20)

stress_hormone <- rgamma(n, shape=2,
                         scale = 2 +
                           1.5*(habitat_quality=="low"))

parasite_load <- rnbinom(
  n,
  mu = 5 + 2*(habitat_quality=="low") + 1*age,
  size = 1.5
)

offspring_count <- rnbinom(
  n,
  mu = 3 +
    0.2*body_mass -
    0.3*parasite_load +
    1*(habitat_quality=="high"),
  size = 2
)

foraging_time <- rgamma(
  n,
  shape = 3,
  scale = 5 +
    2*(habitat_quality=="low") +
    0.5*parasite_load
)

survived_prob <-
  plogis(
    -70 +
      0.5*body_mass -
      0.1*parasite_load +
      0.5*(habitat_quality=="high")
  )

survived <- rbinom(n,1,survived_prob)

dat <- data.frame(
  habitat_quality,
  sex,
  age = ceiling(age),
  territory_size = floor(territory_size*10),
  body_mass = round(body_mass, 1),
  stress_hormone = round(stress_hormone, 2),
  parasite_load,
  offspring_count = floor(offspring_count*0.1),
  foraging_time = round(foraging_time, 1),
  survived
)

write.csv(dat,"datasets/pika_data.csv",row.names=FALSE)

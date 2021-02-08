library("tidyverse")
penguins_sample <- read_csv("/home/rstudio/lab/SHW2/penguins_sample.csv")
glimpse(penguins_sample)

# Collect the count of the different species
penguins_sample %>% group_by(species) %>% count()

# Collect the count of the different genders
penguins_sample %>% group_by(sex) %>% count()

# Average bill length in millimeters
mean(penguins_sample$bill_length_mm, na.rm = TRUE)

# Median bill length in millimeters
median(penguins_sample$bill_length_mm, na.rm = TRUE)

# Median body mass in grams
median(penguins_sample$body_mass_g, na.rm = TRUE)

# Average body mass in grams
mean(penguins_sample$body_mass_g, na.rm = TRUE)

# Average bill length in millimeters filtered by gender
penguins_sample %>%
  group_by(sex) %>%
  summarize(mu = mean(bill_length_mm, na.rm = TRUE))

# Median bill length in millimeters filtered by gender
penguins_sample %>%
  group_by(sex) %>%
  summarize(mu = median(bill_length_mm, na.rm = TRUE))

# Median body mass in grams filtered by species
penguins_sample %>%
  group_by(species) %>%
  summarize(mu = median(body_mass_g, na.rm = TRUE))

# Mean body mass in grams filtered by species
penguins_sample %>%
  group_by(species) %>%
  summarize(mu = mean(body_mass_g, na.rm = TRUE))

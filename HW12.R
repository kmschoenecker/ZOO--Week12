#### Homework Week 12 ----

# Sea bass data

# Load Libraries

library(readxl)

# load in the data

sea_bass <- read_excel("BSB_tagging_data.xlsx")

# Problem
# Black sea bass are protogynous hermaphrodite
# Change sex from female to male, and some are primary males

# Objective 1

# (A) Plot a probability density function (prob density vs proportions from 0 to 1)
# for the proportion of female black sea bass that changed sex out of all those
# which were recaptured after the end of the spawning season (i.e., after July)

# we need to subset our data to deal with only female fish, and then look at how many
# of those female fish were male at the recapture data, we also need to subset by date

# subset datasheet for female fish at capture

subset_sea_bass <- sea_bass %>%
  filter(Sex_at_capture == "F", month(Date_at_recapture) > 7)

# we get 29 observations of fish that were female at capture and were recaptured after July
# now, how many of these fish changed sex (so either male or intersex at recapture?)

change <- subset_sea_bass %>% # we can subset our data frame again and then count the rows
  filter(Sex_at_recapture != "F") %>% nrow()

# so 9 out of the 29 changed sex, save as a proportion

prop_sex_change <- 9/29

# (B) Give the 95% CI for the probability of sex change for these individuals in A above.

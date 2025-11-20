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

# (B) Give the 95% CI for the probability of sex change for these individuals in A above.

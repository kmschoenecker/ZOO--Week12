# Week 12 Elizabeth Notes 


#### Homework Week 12 ----

# Sea bass data

# Load Libraries

library(readxl)       # read in excel files 
library(tidyverse)    # adding in dplyr and pipes
library(janitor)      # cleaning dataframes 

# load in the data

sea_bass <- read_excel("BSB_tagging_data.xlsx")

# Convert column names to snake case
sea_bass <- sea_bass %>%
  clean_names()

# Problem
# Black sea bass are protogynous hermaphrodite
# Change sex from female to male, and some are primary males

#################################################
# Objective 1
#################################################

# note: this part 1 isn't using binomial distributions, we are switching to Bayesian statistics and a Beta distribution 


# (A) Plot a probability density function (prob density vs proportions from 0 to 1)
# for the proportion of female black sea bass that changed sex out of all those
# which were recaptured after the end of the spawning season (i.e., after July)

# First let's subset out hte ones that were recaptured after July 

sea_bass = sea_bass %>% 
  filter(sex_at_capture == "F", month(date_at_recapture) > 7)

sum(sea_bass$sex_at_recapture == "M")
sum(sea_bass$sex_at_recapture == "Intersex")

# Total = 9 / 29 fish = 0.31 
9/29  

#asked chat gpt to help me fill in a new column, I want to make a new column with changed sex as a 0 or 1 option 

sea_bass <- sea_bass %>%
  mutate(
    sex_change = if_else(
      sex_at_recapture != "F",   # yay! i made this bit myself! 
      1, 
      0
    )
  )
# oh hey this is like what we did in a n earlier homework assignment

# So basically we want to bootstrap out of this proportoin? 

set.seed(42) 
data = sea_bass$sex_change
boot_props = replicate(1000, { 
  samp = sample(data, size = length(data), replace = TRUE)
  mean(samp)
  })

plot(density(boot_props), 
     xlab = "Proportion", 
     main = "Bootstrap PDF (proabiblity density function) of proportion \n
     out of 1000 simulated samples")

# Apparently this is not correct, I need to use dbeta 

x = seq(from = 0, to = 1, by = 0.05)
ploty = dbeta(x, shape1 = 10, shape2 = 24, log = FALSE )
plot(ploty)
# What hte heck is this doing? 
# x is a proportion from 0 to 1, which we want to check in intervals for mysterious reasons 
# If we evaluate dbeta() at x over many values, we get a smooth curve, which is the probabiilty density function (PDF) of hte beta distribution 
# a = shape 1 = # of successes + 1 
# b = shape 2 = # of failures + 1 
# Why the + 1? According to the Oracle ie Claude, thi sis due to Bayesian magic. It's related to prior hypotheses, we originally assume that both success and failure is equally likely (distribution 1,1) 


# (B) Give the 95% CI for the probability of sex change for these individuals in A above.

ci_low = qbeta(0.025, shape1 = 10, shape2 = 24) # lower bound of 95% CI 
ci_high = qbeta(0.975, shape1 = 10, shape2 = 24) # upper bound of 95% CI 
c(ci_low, ci_high)
# I interpreted this as 'We are 95% confident that the probability of sex change for these individual fish lies between 15 - 45%?'
# Claude corrected me nad said, "There is a 95% probability that the true probability of sex change is between 0.11 and 0.49."

########################################
# Objective 2 
########################################

# here we switch back to binomial I am pretty sure 

# A 
# ---------------------------------

# Fit logistic regression
logit_model = glm(sex_change ~ length_at_capture, family = binomial, data = sea_bass)
summary(logit_model)
# p value = 0.1122 
# Nope length at capture doesn't seem to matter 

# B
# -----------------------------------
coef(logit_model)
# Log odds scale: For each additional milimeter, the log-odds of changing sex increases by 0.045

exp(0.045)  # just for funsies, converting to odds
# Each additional milimeter multiplies the odds of switching sex by 1.046. Not very much. 

# C 
# ------------------------------------

# first build hte S-curve 
bass_predictions = predict.glm(logit_model) 
# predict() and predict.glm() are the same here, R kind of just chooses the right one for us, but predict.glm() is more precise 
# predict.glm(model, newdata, type = "response") 
# predict.glm() is a function that tells teh computer to make predictions based on a model 
# model = our model, the model = glm(sex change ~ length, family = binomial, data = sea_bass) we made before 
# newdata = a dataframe containing the predictor values where we want to make predictions 
# It's pretty normal to make a new dataframe with a bunch of rows for multiple predictions, like a sequence of them, but make sur ethe column names exactly match the predicotr names in the model 
# If we don't provide newdata, it justs predicts at hte original data points 
# Let's be lazy and not provide newdata 

library(tidyverse)

p = ggplot(sea_bass, aes(x = length_at_capture, y = sex_change)) +
  geom_point() + 
  labs(title = "Logistic Regression Curve for Likelihood of Seabass Sex change \n Based on size", x = "Length at capture (mm)", y = "Probability of Sex Change") 
p

plot(length_at_capture, sex_change, xlab = "Length at capture (mm)", ylab = "Sex change (0/1)", data = sea_bass)

sea_bass$length_at_capture
# Plot the relationship
plot(study_hours, passed, xlab = "Study Hours", ylab = "Passed (0/1)")

# Add the fitted logistic curve
hours_seq = seq(0, 5, 0.1)
pred_prob = predict(logit_model, 
                    newdata = data.frame(study_hours = hours_seq),
                    type = "response")
lines(hours_seq, pred_prob, col = "red", lwd = 2)



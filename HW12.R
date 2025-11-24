#### Homework Week 12 ----

# Sea bass data

# Load Libraries

library(readxl)
library(tidyverse)

# load in the data

sea_bass <- read_excel("BSB_tagging_data.xlsx")

# Problem
# Black sea bass are protogynous hermaphrodite
# Change sex from female to male, and some are primary males

#### Objective 1 ----

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

# save the parameters for our model

successes <- 9
failures <- 29 - 9 # since we had 29 observations that we were originally looking at 

# plotting the density function, start with the proportions
# the problem specifies that we want proportions from 0 to 1

proportions <- seq(from = 0, to = 1, by = 0.05)

# I looked at Nathan's notes for this part, but I am confused why we add 1 to the successes and failures
alpha <- successes + 1
beta <- failures + 1

beta_densities <- dbeta(x = proportions, shape1 = alpha, shape2 = beta) #also not sure why we are using proportions here rather than the proportions of success we calculated earlier
# unless that is already taken into account by specifying shape1 = alpha and shape2 = beta?

df_for_beta_plot <- data.frame(proportions = proportions, probability_density = beta_densities)

# Plotting the probability density function for proportions
ggplot(df_for_beta_plot, aes(x = proportions, y = probability_density)) + geom_line()


# (B) Give the 95% CI for the probability of sex change for these individuals in A above.
# if you are doing a 95% CI, you have 2.5% probability in either tail end
# therefore, you want to calculate qbeta() for 2.5 quantile and 97.5 (since 100 - 2.5 = 97.5)
lower <- qbeta(0.025, shape1=alpha, shape2=beta)
upper <- qbeta(0.975, shape1=alpha, shape2=beta)
proportions_95_CI <- c(lower, upper)
proportions_95_CI
# 0.173 to 0.494

#### Objective 2 ----

# (A) Does the length of a female influence its probability of sex change given 
# that it was recaptured after the end of the spawning season? 
# Give a p value to support your answer.

# problem is asking for us to do a model!
# just so my brain can think about it:
# prob_sex_change ~ female_length
# length is a predictor of sex change, and maybe longer lengths at capture
# resulted in a higher probability of sex change 

# Nathan (shout out Nathan!) recommended the Challenger ex from the Ch 2 reading 
# as being very helpful for doing this part of the problem!

# would it be easier to add a column to our subsetted dataframe of success vs failure
# and then base the model off of that?

subset_sea_bass <- subset_sea_bass %>%
  mutate(Success = if_else(Sex_at_recapture == "F", "N", "Y")) # but if we were doing this with 2
# numeric columns, you would only have 1 equal sign, I think

# should this information be saved as a factor instead of a character?
# but google says to add the column I would have had to make those factors into characters, so I am confused
# Further searching says that if we stick with the characters and then use glm()
# it will automatically convert those characters to factors, so I guess it doesn't matter

# well, on that note, let's actually model what we are looking at

# save success as a factor, because the internet lied, the model is mad if I give it a character

subset_sea_bass$Success <- as.factor(subset_sea_bass$Success)
mod_sea_bass <- glm(Success ~ Length_at_capture, family = binomial(link = "logit"), subset_sea_bass)
# unlike Nathan's example, I only used factors instead of a two-column matrix of successes and failures
# I guess this is where I have to be careful, since the first observation needs to be the "failure"
# In this case, that does happen

# maybe a stupid question, but do the link functions work like parameters in our model
# like, if the logit link is not significant, do you just remove it and rerun the model?

# let's see a summary of this model and pray I did it correctly

summary(mod_sea_bass)

# from this summary, it does not look like length at capture is a good predictor of sex change
# the p-value off of this summary is the column Pr(>|z|), right?
# if so, that is equal to 0.1122, and is not significant

# (B) By how much is the log odds of sex change predicted to change for 
# every millimeter increase in length?

# Nathan noted that this is just the prediction from the GLM?
# Reviewing the lecture slides, transformations aren't really good
# so is the point of doing this to see if the log would lead us to a different interpretation?
# I might not be understanding what the question is actually asking

# (C) Plot the relationship between the probability of sex change for these 
# individuals and length. Overlay the model estimated relationship on the data.
# Label axes appropriately and provide a figure caption

bass_predictions <- predict.glm(object = mod_sea_bass, newdata = subset_sea_bass, type = "response")
# based off of Nathan's code. So, we are using our model to predict what the result would have been
# sex change success or failure, and then comparing it to what we actually saw, right?
# I am actually pretty confused on what the predictions actually represent in this scenario

subset_sea_bass$predictions <- bass_predictions

# probably bad form, but I added a column to take those Y and N and either assign it a 0 (failure) or 1 (success)
# so that I could plot it with ggplot

subset_sea_bass <- subset_sea_bass %>%
mutate(Success_numeric = if_else(toupper(Success) == "Y", 1L, 0L))

# Plotting the relationship on the actual data
final_fig <- ggplot(subset_sea_bass, aes(x = Length_at_capture, y = predictions)) + 
  geom_line() + 
  geom_point(aes(x = Length_at_capture, y = Success_numeric)) +
  ylim(0, 1) + 
  labs(x="Length (mm)", 
       y="Probability of sex change",
       caption="This graph depicts the relationship between bass length and probability of sex change with the solid line. \nData points represent bass that did or did not change sex. As bass length increases, the probability of \nsex change increases.") + 
  theme_bw() + 
  theme(plot.caption=element_text(hjust=0))

final_fig # I don't really understand how this figure helps us
# You can see that as length gets longer, you are more likely to change sex, but our data points kind of muddy that?

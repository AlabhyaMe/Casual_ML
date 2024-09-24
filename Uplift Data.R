library(tidyverse)

#Creating a synthetic dataset for uplift modelling
#The dataset will have the following columns:
# 1. Treatment : If the subject recieved treatment
# 2. Age : Age of the subject
# 3. Score : Percentage in school level
# 4. Gender : Male or Female
# 5. Experience: If they ever worked
# 6. Previous_exp: Months of experience if they ever worked
# 7. Distance_majorcity: Distance from major city
# 8. Owns_motor: If they own a motor vehicle
# 9. Placement: If the subject is in the top 25% of the sum of scaled values of age, score, previous_exp, distance_majorcity, treatment, experience, owns_motor

rm(list=ls())
set.seed(2059)
#Index
ind <- seq(1:30000)
#Treatment
treatment <- sample(c(0,1), 30000, replace = TRUE)
# generate age integer from 17-21, 30000 samples

age <- sample(18:22, 30000, replace = TRUE)
# Percentage in school level 
score <- as.integer(rbeta(30000,  50, 15)*100)
#Gender
gender <- sample(c('M','F'),30000, replace=TRUE)
#Experience
# Conditional Experience: Higher probability of experience with higher age
experience <- sapply(age, function(a) {
  if (a == 18) {
    return(sample(c(0, 1), 1, prob = c(0.9, 0.1)))  # Lower chance of experience at age 18
  } else if (a == 19) {
    return(sample(c(0, 1), 1, prob = c(0.8, 0.2)))  # Equal chance at age 19
  } else if (a == 20) {
    return(sample(c(0, 1), 1, prob = c(0.5, 0.5)))  # Higher chance of experience at age 20
  }else {
    return(sample(c(0, 1), 1, prob = c(0.4, 0.6)))  # Highest chance higher than 20
  }
})
previous_exp <- pmin(as.integer(rbeta(30000, 6, 4)*36), 36) * experience
#Distance from major industry city
distance_majorcity <- rnorm(30000, 5, 1.5)
#Owns Motor Vehicle
owns_motor <- sample(c(0, 1), 30000, replace = TRUE, prob = c(0.4, 0.6))

#Creating the dataframe
df <- data.frame(ind, treatment,age,score,gender,experience,previous_exp,distance_majorcity,owns_motor)

#Scale

sum_scaled <- scale(age) * -0.95 + 
  scale(previous_exp) * 1.3 + 
  scale(distance_majorcity) + 
  treatment * 1.5 + 
  experience + 
  owns_motor + 
  scale(score) *1.1

#generate some random noise to make sure model is not just adding up the values
sum_scaled <- sum_scaled + rnorm(length(sum_scaled), mean = 1, sd = 1)
#Higher than 75% value
third_quant <- quantile(sum_scaled, 0.75)
df$placement <- ifelse(sum_scaled > third_quant, 1, 0)

write.csv(df,"Vocational.csv", row.names = FALSE)

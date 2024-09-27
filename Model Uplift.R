library(tidyverse)
library(readxl)
library(tidymodels)
library(vip)
library(cowplot)
library(ROCR)
library(gridExtra)

rm(list=ls())

#Import the data set
data <- read.csv("Vocational.csv")
summary(data)
#remove the first column
df<-data
df <- df[-1]
df$placement <- df$placement %>% as.factor()

#Exploratory Analysis
table(df$treatment)
table(df$age)
hist(df$score)
table(df$experience)
hist(df$previous_exp)
hist(df$distance_majorcity)

set.seed(2059)

#Part 1, Randomized Control Trial (RCT)
rct <- data
place_treat <- rct$placement[rct$treatment==1]
place_control <- rct$placement[rct$treatment==0]

t.test(place_treat,place_control,alternative = 'greater')
#t.test(rct$placement~rct$treatment, alternative = 'greater')

rct$treatment <- factor(rct$treatment, levels = c(0, 1), labels = c("Control", "Treatment"))
rct$placement <- factor(rct$placement, levels = c(0, 1), labels = c("Not Placed", "Placed"))
table(rct$treatment,rct$placement)
prop.table(table(rct$treatment,rct$placement))

#################################################################
#Part 2 : Logistic Model

rm(list=setdiff(ls(),c("df","data")))

#Split the data into training and testing
set.seed(2059)
split <- initial_split(df, prop = 0.75, strata = placement)
train <- training(split)
test <- testing(split)

rm(split)

#Making a Logistic Model using base glm function

lr <- glm(data = train,placement ~., family = binomial)
summary(lr)
vip(lr)

pred <- predict(lr, newdata=test, type = "response")
ROCRpred <- prediction(pred,test$placement)
ROCRperf <- performance(ROCRpred, "tpr",'fpr')
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.5,2))

confusionMatrix <- table(pred >0.4,test$placement)
sum(diag(confusionMatrix)) / sum(confusionMatrix)
rm(ROCRperf,ROCRpred,confusionMatrix,pred,test,train)

###########################################################3
# Two Model  Approach
treatment <- df %>%
              filter(treatment == 1)
control <- df %>%
            filter(treatment==0)
  
lr_treat <- glm(data = treatment,placement ~.-treatment, family = binomial)
lr_control <- glm(data = control,placement ~.-treatment, family = binomial)

df$pred_treat <- round(predict(lr_treat, newdata = df, type = "response"),5)
df$pred_control <- round(predict(lr_control, newdata = df, type = "response"),5)

df$diff <- df$pred_treat- df$pred_control

df <- df %>% 
        mutate(decile = 11- ntile(diff,10))

######################################################
# making the uplift
uplift <- df %>%
  group_by(decile) %>%
  summarise(
    treatment_n = sum(treatment),
    treatment_placement = sum(as.integer(as.character(placement)) * treatment),  # Only for treated
    control_n = sum(1 - treatment),
    control_placement = sum(as.integer(as.character(placement)) * (1 - treatment))  # Only for control
  )

uplift_cumulative <-uplift

uplift_cumulative $cum_treatmentnumber <- cumsum(uplift_cumulative$treatment_n)
uplift_cumulative$cum_treatmentplacement <- cumsum(uplift_cumulative$treatment_placement)
uplift_cumulative$cum_controlplacement <- cumsum(uplift_cumulative$control_placement)
uplift_cumulative$cum_controlnumber <- cumsum(uplift_cumulative$control_n)

uplift_cummulative <- uplift_cumulative%>%
  select(decile, cum_treatmentnumber, cum_treatmentplacement, cum_controlplacement, cum_controlnumber)


uplift_cumulative$incremental <- uplift_cumulative$cum_treatmentplacement - (uplift_cumulative$cum_controlplacement/uplift_cumulative$cum_controlnumber) * uplift_cumulative$cum_treatmentnumber
uplift_cumulative$incrementUplift <- round(uplift_cumulative$incremental / sum(uplift_cumulative$cum_controlnumber)*100,2)

a_plot <- ggplot() +
  geom_line(uplift_cumulative, mapping=aes(x=decile, y=incrementUplift), color='blue') +
  geom_point(uplift_cumulative, mapping=aes(x=decile, y=incrementUplift), color='darkblue') +
  geom_line(uplift_cumulative, mapping=aes(x=decile, y=incrementUplift), color='blue') +
  geom_line(aes(x=c(1, 10), y = c(first(uplift_cumulative$incrementUplift), last(uplift_cumulative$incrementUplift))), 
            linetype="dashed", color='green') +
  labs(title = "Incremental Uplift by Decile",
       x = "Decile",
       y = "Incremental Uplift",
       caption = "") +
  theme_minimal()

#### Average Effect on decile #########
uplift$incremental <- (uplift$treatment_placement/ uplift$treatment_n - uplift$control_placement/uplift$control_n) *100
b_plot <- ggplot(uplift, aes(x=as.factor(decile),y=incremental)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_text(aes(label=round(incremental,1)), vjust=-0.5,color='black')+
  labs(title = "Average Uplift by Decile",
       x = "Decile",
       y = "Average Uplift",
       caption = "Uplift in Decile") +
  theme_minimal()

grid.arrange(a_plot, b_plot, ncol=2)




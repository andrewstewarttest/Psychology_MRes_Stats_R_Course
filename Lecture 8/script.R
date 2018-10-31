library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(psych) # To generate dscriptives using the psych package
library(yarrr) # For pirateplots
library(emmeans) # Needed for pairwise comparisons

# Mixed ANOVA with one between participants factor, and two repeated
# First create the data - excuse my use of for loops...
set.seed(1234)
x1 <- rnorm(n = 16, mean = 750, sd = 25) #ppy
x2 <- rnorm(n = 16, mean = 850, sd = 30) #pny
x3 <- rnorm(n = 16, mean = 830, sd = 30) #npy
x4 <- rnorm(n = 16, mean = 770, sd = 25) #nny

x1o <- rnorm(n = 16, mean = 910, sd = 30) #ppo
x2o <- rnorm(n = 16, mean = 980, sd = 29) #pno
x3o <- rnorm(n = 16, mean = 910, sd = 29) #npo
x4o <- rnorm(n = 16, mean = 980, sd = 30) #nno

x5 <- c((1:16), (1:16), (1:16), (1:16), (17:32), (17:32), (17:32), (17:32))
x6 <- (1:128)
x7 <- x6
x8 <- x6

data <- as.integer(c (x1,x2,x3,x4, x1o, x2o, x3o, x4o))
data <- cbind(x5, x6, x7, x8, data)
data <- as.data.frame(data)

for (i in 1:32) {
  data$x6[i] = "Pos"
}

for (i in 33:64) {
  data$x6[i] = "Neg"
}

for (i in 1:16) {
  data$x7[i] = "Pos"
}

for (i in 17:32) {
  data$x7[i] = "Neg"
}

for (i in 33:48) {
  data$x7[i] = "Pos"
}

for (i in 49:64) {
  data$x7[i] = "Neg"
}

for (i in 1:64) {
  data$x8[i] = "Young"
}

for (i in 65:128) {
  data$x8[i] = "Old"
}

for (i in 65:96) {
  data$x6[i] = "Pos"
}

for (i in 97:128) {
  data$x6[i] = "Neg"
}

for (i in 65:80) {
  data$x7[i] = "Pos"
}

for (i in 81:96) {
  data$x7[i] = "Neg"
}

for (i in 97:112) {
  data$x7[i] = "Pos"
}

for (i in 113:128) {
  data$x7[i] = "Neg"
}

colnames(data) <- c("Participant", "Image", "Word", "Age", "RT")

data$Image <- factor(data$Image, levels = c("Pos", "Neg"))
data$Word <- factor(data$Word, levels = c("Pos", "Neg"))
data$Age <- factor(data$Age, levels = c("Young", "Old"))

ggplot(data, aes(x = Word:Image, y = RT, colour = Word:Image)) + 
  geom_violin() + geom_jitter(width = .1, alpha = .2) + 
  stat_summary(fun.data = "mean_cl_boot") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Age) + guides(colour = FALSE)

pirateplot (formula = RT ~ Word + Image + Age, data = data , theme = 0, # Start from scratch
            inf.f.o = .7, # Band opacity
            inf.f.col = piratepal("basel"), # Add color to bands
            point.o = 1, # Point opacity
            avg.line.o = .8, # Average line opacity
            gl.col = gray(.6), # Gridline specifications
            gl.lty = 1,
            inf.method = 'sd',
            gl.lwd = c(.5, 0),
            ylim = c(600,1100))

describeBy(data$RT, group = list (data$Word, data$Image, data$Age))

model <- aov_4(RT ~ Word * Image * Age + (1 + Word * Image | Participant), data)
summary(model)
anova(model)

emmeans(model, pairwise ~ Word * Image * Age, adjust = "Bonferroni")

# Three way interaction is significant so run two separate 2-ways
young_filter <- filter(data, Age == "Young")

model_young <- aov_4(RT ~ Word * Image + (1 + Word *Image | Participant), young_filter)
anova(model_young)

emmeans(model_young, pairwise~Word*Image, adjust = "Bonferroni")

old_filter <- filter(data, Age == "Old")

model_old <- aov_4(RT ~ Word * Image + (1 + Word * Image | Participant), old_filter)
summary(model_old)
anova(model_old)

emmeans(model_old, pairwise ~ Word * Image, adjust = "Bonferroni")

#ANCOVA
cond <- read_csv("cond.csv")
cond$Condition <- as.factor(cond$Condition)

ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + geom_point() 

describeBy(cond$Ability, group = cond$Condition)
describeBy(cond$Gaming, group = cond$Condition)

ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + geom_point() 

# Separately by Condition
ggplot(cond, aes(x = Gaming, y = Ability,  colour = Condition)) + geom_point() + 
  facet_wrap(~ Condition) + geom_smooth(method = 'lm')

# Run the ANOVA (i.e., without the covariate)- model is significant
model <- aov(Ability ~ Condition, data = cond)
anova(model)

model1 <- aov_4(Ability ~ Condition + (1 | Participant), data = cond)
anova(model1)

# Run the ANCOVA - when we add the effect of Gaming Frequency first,
# the model is now not significant
# First with aov() which uses Type 1 Sums of Squares
model_ancova <- aov(Ability ~ Gaming + Condition, data = cond)
anova(model_ancova)

# Now with aov_4() which uses Type 3 Sums of Squares 
cond$Gaming <- scale(cond$Gaming)
model_ancova <- aov_4(Ability ~ Gaming + Condition + (1 | Participant), data = cond, factorize = FALSE)
anova(model_ancova)

# Unadjusted means
describeBy(cond$Ability, group = cond$Condition)

# Report adjusted means
emmeans(model_ancova, pairwise~Condition, adjust = "none")

# ANCOVA as a special case of Regression 
ggplot(cond, aes(x = Condition, y = Ability, colour = Condition)) + geom_violin() + 
  geom_jitter(width = .1, alpha = .5) + stat_summary(fun.data = "mean_cl_boot") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + guides(colour = FALSE)

# Set up the Water level as the reference level and check the contrasts
cond$Condition <- relevel(cond$Condition, ref = 3)
contrasts(cond$Condition)

# Build a linear model just with Condition predicting Ability
lm1 <- lm(Ability ~ Condition, data = cond)
lm1

#Build a linear model with both Gaming Frequency and Condition predicting Ability
lm2 <- lm(Ability ~ Gaming + Condition, data = cond)
lm2


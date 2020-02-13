# The purpose of this code is to demonstrate 
# some of the techniques you learned in past
# classes on regression analysis in the R
# software environment.

# Begin by loading the data, which is stored
# in a .csv file called "ecls.csv". You will
# need to download the .csv file from Canvas
# and save it if you haven't already done so.

# Method 1: open a browser window
ecls <- read.csv(file = file.choose())

# Method 2: call the path directly. On my machine,
# the .csv file is in a folder called 2020 SPRING
# at the following location:
ecls <- read.csv(file = "/Users/keller/Teaching/2020 SPRING/ecls.csv")

# You will need to change the path based on where
# the ecls.csv file is located on your machine.

# Now that the data are loaded, examine them.
head(ecls)
names(ecls) # First column is row names
dim(ecls) # 100 rows and 5 columns

# What do the following lines do?
ecls[,1]
ecls[1,]
ecls[,2]
ecls[2,]
ecls[1:10,]
ecls[, 1:2]
ecls[,-1]
ecls[-1,]

# Want to get rid of the redundant variable X.
head(ecls)
ecls <- ecls[, -1]
head(ecls) # Notice that the "X" rownames column is gone.

# Numeric summaries of variables in the data frame.
summary(ecls)

# Gender is dichotomous; other variables are continuous.
is.data.frame(ecls) # ecls is a data frame

# The dollar sign can be used with data frames
# to specify columns. In combination with tab
# completion, this can be very useful. Type ecls$ 
# and put your cursor after $ and push tab
# and you should see a list of the variable names 
# come up.
ecls$GENDER

# Display the structure of ecls
str(ecls)

# GENDER is categorical so create a factor.
ecls$GENDER_fac <- factor(x = ecls$GENDER,
                          levels = c(0,1),
                          labels = c("male", "female"))

# Make a table.
t1 <- table(ecls$GENDER)
t1
barplot(t1)

# Make histograms of other variables.
hist(ecls$SES)
hist(ecls$MATHK)
hist(ecls$MATH5)

# Examine bivariate relationships via scatterplots.
plot(ecls)

# Estimate correlation matrix for the continuous variables.
head(ecls)
cor(ecls[,-5])
round(cor(ecls[,-5]), digits = 2)

# Variance/covariance matrix for same.
round(cov(ecls[,-5]), digits = 2)

###############################
# SIMPLE LINEAR REGRESSION ####
###############################
# Simple regression of fifth grade math score on
# socioeconomic status (measured in kindergarten).
lm1 <- lm(formula = MATH5 ~ SES, 
          data = ecls)
summary(lm1)

plot(x = ecls$SES,
     y = ecls$MATH5,
     xlab = "Socioeconomic Status",
     ylab = "Fifth Grade Math Score")

# Can extract coefficients
slm1 <- summary(lm1)
coef(slm1)
round(coef(slm1), 2)

# Add regression line
abline(a = 123.78,
       b = 13.03, 
       lwd = 2, # double default width,
       col = "blue") # self-explanatory

# Can extract R-squared and other quantities
slm1$r.squared
# Use tab completion to view others

#################################
# MULTIPLE LINEAR REGRESSION ####
#################################
# Add kindergarten math as a predictor.
lm2 <- lm(formula = MATH5 ~ SES + MATHK, 
          data = ecls)
summary(lm2)

# It is more difficult to visualize multiple regression.
# For Mac OS users, make sure you downlaod Xquartz before
# using rgl. Get it here: https://www.xquartz.org/

# install.packages("rgl")
library(rgl)
open3d()
plot3d(ecls[,c("SES", "MATHK", "MATH5")], col = "red", size = 2) 

# Plot the regression plane
planes3d(a=coef(lm2)[2], b=coef(lm2)[3], c=-1, 
         d=coef(lm2)[1], alpha=.5, col = "pink") 

# Note that if you quit XQuartz before closing RStudio
# your R session will crash.

##############################
# REGRESSION WITH A FACTOR ###
##############################
lm3 <- lm(formula = MATH5 ~ GENDER_fac,
          data = ecls)
summary(lm3)

# Because GENDER was coded as a factor, R will 
# dummy-code it and hold out the first category 
# as the reference. 

levels(ecls$GENDER_fac) # female is the first category
summary(lm3)

# Print the design matrix for the regression.
model.matrix(object = MATH5 ~ GENDER_fac, 
             data = ecls)

# With a factor as predictor, default behavior for 
# plot function is to create boxplots.
plot(formula = MATH5 ~ GENDER_fac,
     data = ecls)

# Density plots
dnsf <- density(ecls$MATH5[ecls$GENDER_fac == "female"])
dnsm <- density(ecls$MATH5[ecls$GENDER_fac == "male"])
plot(dnsf, 
     col = "red", 
     xlab = "Fifth Grade Math Score",
     main = "",
     las = TRUE,
     lwd = 2)
points(dnsm, col = "green", type = "l", lwd = 2)
legend(x = "topleft", 
       col = c("red", "green"),
       legend = c("female", "male"),
       lwd = 2)

######################################
# REGRESSION WITH INTERACTION TERM ###
######################################
# The colon : is used to indicate an interaction 
lm4 <- lm(formula = MATH5 ~ GENDER_fac + MATHK + GENDER_fac:MATHK,
          data = ecls)
summary(lm4)

# The asterisk * is used to include an interaction along
# with all lower-order terms. The following model is 
# equivalent to lm4:
lm5 <- lm(formula = MATH5 ~ GENDER_fac*MATHK,
          data = ecls)
summary(lm5)

# Plot the MATHK, MATH5 relationship, separated by GENDER.
# Create a 0/1 numeric vector based on GENDER for coloring
# points.
plot(x = ecls$MATHK, 
     y = ecls$MATH5,
     col = ecls$GENDER + 1,
     pch = 19,
     xlab = "Kindergarten Math Score",
     ylab = "5th Grade Math Score")
legend(x = "bottomright",
       pch = 19, col = 1:2,
       legend = c("female", "male"))
abline(a = 64.38, b = 1.83, 
       col = 1, lwd = 2)
abline(a = (64.38 + 11.52), 
       b = (1.83 - .23), 
       col = 2, lwd = 2)

######################################
# REGRESSION WITH POLYNOMIAL TERMS ###
######################################
fmla6 <- as.formula("MATH5 ~ GENDER_fac*MATHK + GENDER_fac*I(MATHK^2)")
lm6 <- lm(formula = fmla6,
          data = ecls)
summary(lm6)

# Plot separate curves for male/female
plot(x = ecls$MATHK, 
     y = ecls$MATH5,
     col = ecls$GENDER + 1,
     pch = 19,
     xlab = "Kindergarten Math Score",
     ylab = "5th Grade Math Score")
legend(x = "bottomright",
       pch = 19, col = 1:2,
       legend = c("female", "male"))
ff <- function(x) {.404 + 5.933*x - .062*x^2}
fm <- function(x) {.404 + (5.933 + .338)*x - (.062 + .003)*x^2}
curve(expr = ff, from = 0, to = 70, lwd = 2, col = 1, add = TRUE)
curve(expr = fm, from = 0, to = 70, lwd = 2, col = 2, add = TRUE)


# Lab 01
# Examine the Motor Trend Cars data set
?mtcars

# Run a simple regression of mpg on hp

# Run a mulitple regression of mpg on hp and wt

# Create a factor variable for cyl and then regress
# mpg on cyl_fac

# Run a multiple regression of mpg on cyl_fac, wt
# and include their interaction.

# Choose one or two of the above regressions to plot.

####################
# APPENDIX #########
####################
head(ecls)
# Let's take the mean of the outcome variable.
length(ecls$MATH5)

# Take the mean "by hand"
(y_bar <- sum(ecls$MATH5)/100)
# Now, use the function
mean(x = ecls$MATH5, 
     na.rm = TRUE)

# Sample variance "by hand"
(y_s_sq <- (1/99)*sum((ecls$MATH5 - y_bar)^2))
# Sample variance with function
var(x = ecls$MATH5,
    na.rm = TRUE)

# Sample SD "by hand"
(y_s <- sqrt(y_s_sq))
# With function
sd(x = ecls$MATH5,
   na.rm = TRUE)

# Mean of MATHK and mean of MATH5
(mnk <- mean(ecls$MATHK, na.rm = TRUE))
(mn5 <- mean(ecls$MATH5, na.rm = TRUE))

# Var of MATHK and var of MATH5
(vark <- var(ecls$MATHK, na.rm = TRUE))
(var5 <- var(ecls$MATH5, na.rm = TRUE))

# Covariance between MATHK and MATH5
(cov_k5 <- (1/99)*sum((ecls$MATHK - mnk)*(ecls$MATH5 - mn5)))
# With function
cov(x = ecls$MATHK, y = ecls$MATH5, use = "pairwise.complete.obs")

# Correlation between MATHK and MATH5
cov_k5/sqrt(vark*var5)
# With function
cor(x = ecls$MATHK, y = ecls$MATH5, use = "pairwise.complete.obs")

# Consider the simple linear regression between 
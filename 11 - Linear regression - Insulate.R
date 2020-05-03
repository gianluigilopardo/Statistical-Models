######################################################################
###
###  Linear Regression
###
#######################################################################

### Description of the data
# We want to evaluate the effect of thermic isolation with respect to 
# gas consuption. The file "191001insulate-data.txt" is a data frame containing
# measurements, taken before and after the termic isolation of a building, of
#
# 1) Mean external temperature (°C - The thermometer has been set to 20°C),
# 2) Gas consumption (feet^3).
#
# Measurements were taken for 26 weeks before the installation of the thermic
# isolation system and for 30 weeks after it.
# How can we study the connenction between external temperature and gas consumption,
# by taking into account the effect of thermic isolation?

library(RCurl)
### Clean the environment
rm(list = ls())

### Set the directory
# getwd() -> current directory
# setwd(dir) -> sets the directory

### Import the data
# We use the function read.table for ".txt" files.
# The argument "col.names" assigns names to the columns of the dataset.
data = getURL('https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/11%20-%20dati%20insulate.txt?token=ACNKBBAFJM6OC65IDE6R6226V2NPE')
insulate <- read.table(text = data, col.names = c("when", "temp", "cons"))

# Look to your data
str(insulate)
head(insulate)

# Use the variables
insulate[,2]
insulate$temp

temp #not found!

# To write just "temp" instead of "insulate$temp" we have to attach the dataset
attach(insulate)

temp
cons

### Investigate the relation between temp and cons
# First of all we plot cons vs temp, without considering the variable "when"
plot(temp, cons, pch = 16) #the argument "pch" is used for the plotting symbol (?pch)

# It is evident a decreasing tendency of gas consumption with respect to temp:
# this is natural, if the external temperature rises up, you may not want to 
# use the heating system... This tendency is also described by the correlation
# between the two variables
cor(temp, cons) #[1] -0.6832545 < 0

### Highlight the effect of thermic isolation
plot(temp, cons, type = "n") #type = "n" means empty plot
# We select the rows with "when" is equal to "before"
points(temp[when=="before"], cons[when=="before"], pch=16) #function "points" plots points over an existing plot
# Same for the submatrix "after"
points(temp[when=="after"], cons[when=="after"], pch=17, col=2)
# Add a legend and a title to the graph
legend(x=9,y=6.5, c("before", "after"), col = c(1,2), pch=c(16,17), bty="n", y.intersp = 0.2)
# This plots a legend in a rectangle with left side corresponding to x = 9 and top side
# corresponding to y = 8, bty = "n" does not plot the box and y.intersp regulates the space
# between the entries
title("Scatter plot - insulate data - before-after isolation")

# The relation between temp and cons is decreasing, but the consumption levels
# are lower, if we fix a temperature value: termic isolation has an effect.

### Simple Linear Regression
# lm(y ~ x), where y is you dependent variable, 
# x is the explicative variable

# We perform two linear regressions at first, only for a didactic purpose,
# one by considering only data before thermic isolation, one by considering
# only data after it.

temp_before <- temp[when=="before"]
cons_before <- cons[when=="before"]

# Model: cons_before = b_0 + b_1*temp_before + eps
regr_before <- lm(cons_before ~ temp_before)
summary(regr_before)

# Call:
#   lm(formula = cons_before ~ temp_before)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -0.62020 -0.19947  0.06068  0.16770  0.59778 
# 
# Coefficients:
#               Estimate   Std. Error t value  Pr(>|t|)    
#   (Intercept)  6.85383    0.11842   57.88   <2e-16 ***
#   temp_before -0.39324    0.01959  -20.08   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2813 on 24 degrees of freedom
# Multiple R-squared:  0.9438,	Adjusted R-squared:  0.9415 
# F-statistic: 403.1 on 1 and 24 DF,  p-value: < 2.2e-16

# We see that the R-squared is near 1, this indicates a good performance of our model:
# It accounts for the 94.38% of the variance in consumption, before the thermic isolation.
#
# From the column Pr(>|t|) we see that data support the hypotheses that the regression
# coefficients are significantly different from zero, therefore before insulation
# - if temperature outside is 0°C, the weekly consumption
#   of gas is hat{b_0} = 6.854 (SE = 0.118)ft^3 (cubic feet).
# - cons_before has an expected decrease of hat{b_1} = 0.39324 ft^3
#   for a 1°C increment of temp_before

temp_after <- temp[when=="after"]
cons_after <- cons[when=="after"]

regr_after <- lm(cons_after ~ temp_after)
summary(regr_after)

# Plot of the two regression lines over the scatter plot we did before.
# If you type str(regr_before) you see that it is a list, so we can
# consider the vector of the regression coefficients with "regr_before$coef"
abline(regr_before$coef) 
abline(regr_after$coef, col = 2)

# Fitted values, i.e. values on the regression line
regr_before$fitted

# Residuals
# To have a good fitting, residuals should be centered around zero, otherwise
# there could be some systematic errors and the regression line could over/underestimate
# the data. Moreover, residuals should not show any particular pattern: a pattern
# would indicate that a linear approximation is not adequate and that a polynomial regression
# should be preferred.

res_before <- regr_before$resid
res_after <- regr_after$resid

# Plot two graphs in one window
par(mfrow = c(1,2)) #1 row, 2 columns
#
plot(temp_before, res_before)
abline(h=0)
#
plot(temp_after, res_after)
abline(h=0)

# Plot an histogram of the residuals
hist(res_before)
hist(res_after)

par(mfrow=c(1,1)) #back to one graph per row

### Multiple linear regression
# Until now we performed two simple linear regressions, by decomposing the data
# into two subsets. However, it is better to perform a multiple linear regression
# with two variables, one of them being a cathegorical one.
#
# Case 1
# No interaction when-temp (implies parralel straight lines)
# The model is: cons = gamma_0 + gamma_1*when + gamma_2*temp + eps
#
# R uses the alphabetic order and considers
# - when[i]=0 if when[i]=after
# - when[i]=1 if when[i]=before
#
# that is
#         gamma_0 + gamma_1 + gamma_2*temp + eps, if when = before
# cons =
#         gamma_0 + gamma_2*temp + eps,           if when = after

regr <- lm(cons~when+temp)
summary(regr)
#Call:
#lm(formula = cons ~ when + temp)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-0.74236 -0.22291  0.04338  0.24377  0.74314 
#
#Coefficients:
#            Estimate   Std. Error t value Pr(>|t|)    
#(Intercept)  4.98612    0.10268   48.56   <2e-16 ***
#whenbefore   1.56520    0.09705   16.13   <2e-16 ***
#temp        -0.33670    0.01776  -18.95   <2e-16 ***

#Residual standard error: 0.3574 on 53 degrees of freedom
#Multiple R-squared: 0.9097,     Adjusted R-squared: 0.9063 
#F-statistic: 267.1 on 2 and 53 DF,  p-value: < 2.2e-16

# - Regardless the presence of the insulation, the expected decrease in gas comnsumption
#   is of 0.3367 ft^3 for an increase of 1°C in temperature
# - If we fix a temperature value, the presence of the insulation decreases gas
#   consumption of 1.5652 ft^3

plot(temp, cons, type='n')
points(temp[when=="before"],cons[when=="before"],pch=16, col=1)
points(temp[when=="after"], cons[when=="after"], pch=17, col=2)
abline(a=regr$coef[1]+regr$coef[2], b=regr$coef[3], lwd=3)
abline(a=regr$coef[1], b=regr$coef[3], col=2, lwd=3)
legend(x=8.5,y=6.5, c("before", "after"), col = c(1,2), pch=c(16,17), bty="n", y.intersp = 0.2)
title("Scatter plot - insulate data - no interactions")

# Residuals
plot(temp, regr$resid)
abline(h=0)
hist(regr$resid)

# Case 2
# With interactions between when and temp: does the slope of the regression
# line (i.e. the speed of the decrease of gas consumption) depend on the presence
# of the isolation? To do this, we introduce a new variable (i.e. the interaction term)
# and see if it contributes significantly to the model.
#
# The model is: cons = alpha_0 + alpha_1*when + alpha_2*temp + alpha_3*when*temp + eps
#
# R uses the alphabetic order and considers
# - when[i]=0 if when[i]=after
# - when[i]=1 if when[i]=before
#
# that is
#         (alpha_0 + alpha_1) + (alpha_2 + alpha_3)*temp + eps, if when = before
# cons =
#         alpha_0 + alpha_2*temp + eps, if when = before

regr2 <- lm(cons~when*temp) # or regr2 <- lm(cons~when+temp+when*temp)
                            # or regr2 <- lm(cons~when+temp+when:temp)
summary(regr2)

# Call:
#   lm(formula = cons ~ when * temp)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.97802 -0.18011  0.03757  0.20930  0.63803 
# 
# Coefficients:
#                    Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)      4.72385    0.11810  40.000  < 2e-16 ***
#   whenbefore       2.12998    0.18009  11.827 2.32e-16 ***
#   temp            -0.27793    0.02292 -12.124  < 2e-16 ***
#   whenbefore:temp -0.11530    0.03211  -3.591 0.000731 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.323 on 52 degrees of freedom
# Multiple R-squared:  0.9277,	Adjusted R-squared:  0.9235 
# F-statistic: 222.3 on 3 and 52 DF,  p-value: < 2.2e-16


# We see that the interaction term contributes significantly.

plot(temp,cons,type="n")
points(temp[when=="before"],cons[when=="before"],pch=16, col=1)
points(temp[when=="after"], cons[when=="after"], pch=17, col=2)
title("Scatter plot - insulate data - with interactions")
abline(a=regr2$coef[1]+regr2$coef[2], b=regr2$coef[3]+regr2$coef[4], lwd=3)
abline(a=regr2$coef[1], b=regr2$coef[3], col=2, lwd=3)
legend(x=8.5,y=6.5, c("before", "after"), col = c(1,2), pch=c(16,17), bty="n", y.intersp = 0.2)

# Residuals
plot(temp, regr2$resid)
abline(h=0)

# Plot an histogram showing the relative frequencies
hist(regr2$resid, freq = F)

###Compare our models
# Is regr2 (model with interactions) better than the model cons~1 (i.e. temperature not significant)?
anova(lm(cons~1), regr2)
# The p-value is small so we reject the hypothesis that the two models are equivalent.
# Is regr2 (model with interactions) better than regr (i.e. model without interactions)?
anova(regr, regr2)

# If you do not have to use your dataset anymore, detach it!
detach(insulate)

###Summary
# lm -> performs the (multiple) linear regression
# Check the R^2 and the residuals
# Compare your models
























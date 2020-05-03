########################################################################################
########################################################################################
###############         Linear Regression and log-Transformations         ##############
########################################################################################
########################################################################################

# Introduction
# This lecture contains some examples of linear regression models and logarithmic models.
# The data 191022simgdp.txt is used to study public expenditure on health care in a set of 
# countries. Each row of the dataset represents a country and the variables are:
#   . health_pc: public expenditure on health care per capita in USD,
#   . gdp_pc: GDP per capita in USD,
#   . oecd: dummy variable, 1 if the country belongs to the OECD (Organization for Economic 
#         Cooperation and Development) and 0 otherwise.

rm(list = ls())
#setwd(dir)
library(RCurl)
data = getURL('https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/14%20-%20simgdp.txt?token=ACNKBBHOYPZ2UN2B4RNNUSS6V2OBG')
data_sim <- read.csv(text = data, header=T, sep = " ")
#attach(data_sim)
# Descriptive statistics
dim(data_sim)
names(data_sim)
str(data_sim)
oecd <- as.factor(OECD)
summary(data_sim)

# Linear Regression
# We perform a simple linear regression at first, with gdp as a predictor.
lm_gdp <- lm(health_pc ~ gdp_pc)
summary(lm_gdp)
# The R2 is 62.37%, not so high, and the dependence on the predictor is highly significant.
plot(gdp_pc,health_pc,pch=16)
# the function "coef" extracts the regression coefficients from the object "lm_gdp"
abline(coef(lm_gdp), col = "red")

# For low values of gdp_pc we observe small variability around the estimated regression line,
# while the variability becomes larger when we move to higher values of gdp_pc. This 
# cone-shaped pattern is evident also in the plot of the residuals:
plot(gdp_pc,residuals(lm_gdp), pch=16)

# Other graphical tools to evaluate the goodness of our fitting can be obtained by passing 
# to plot the object lm_gdp. We obtain four diagnostic plots:
par(mfrow=c(2,2),mai=c(0.7,0.7,0.2,0.2), mgp = c(2,1,0))
plot(lm_gdp)

# 1. The top-left graph plots the residuals against the fitted values. This graph is similar 
#     to the residuals vs predictor graph: it shows the mean residual doesn't change with the 
#     fitted values, but the spread of the residuals is increasing as the fitted values 
#     changes. That is, the spread is not constant and this implies heteroskedasticity.
# 2. The top-righ plot is a QQ-plot to detect the violation of the normality assumption of 
#     the errors: if the residuals were normal, the points should have lied on a straight line.
# 3. The bottom-left plot shows residual absolute values vs fitted values, and it is used to 
#     detect a potential heteroskedasticity problem: each point is given by the coordinates 
#     (y^_i,sqrt(|e*_i|))), where e*_i's are the standardized residuals. The points should be 
#     scattered randomly in a band without any pattern or trend against the fitted values.
# 4. The bottom-right graph is the residuals vs leverage graph (very difficult to read and 
#     not so used) and provides information about individual observations that you may wish 
#     to attend to. The graph identifies outliers, high-leverage points, and influential 
#     observations. An outlier is an observation that isn't predicted well by the fitted 
#     regression model (that is, has a large positive or negative residual). An observation 
#     with a high leverage value is an outlier in the predictor space. The dependent variable 
#     value isn't used to calculate an observation's leverage. An influential observation 
#     is an observation that has a disproportionate impact on the determination of the model 
#     parameters. If a particular observation is highly influential in estimating the 
#     parameters of the regression model, we can assess how influential it is by fitting 
#     the regression model with and without that observation and evaluating how the model
#     parameters change. Influential observations are identified using a statistic called 
#     Cook's distance, or Cook's D: The Cook's distance for an observation i (di > 0) is 
#     a measure of how much the estimates of the regression parameters change when that 
#     observation is included versus when it is excluded. The points in the graph we're 
#     looking for are values in the upper right or lower right corners, which are outside 
#     the red dashed Cook's distance line. These are points that would be influential in 
#     the model and removing them would likely noticeably alter the regression results. 
#     Typically a value of Cook's D larger than 1 is a clue of an influential observation 
#     (see the influence plot at the end of the lecture).

lm_oecd <- lm(health_pc ~ oecd)
summary(lm_oecd)

lm_both <- lm(health_pc ~ oecd + gdp_pc)
summary(lm_both)

lm_inter <- lm(health_pc ~ oecd*gdp_pc)
summary(lm_inter)

anova(lm_gdp, lm_oecd, lm_both, lm_inter)


# Logarithmic models
# To solve the heteroskedasticity problem, logarithmic transformations are used. Let us 
# ignore the categorical variables and let us consider the semi-log model
# log(health_pc) = b0 + b1*gpd_pc + eps.
# Clearly, this is a linear model in the parameters, but the relationship between health_pc 
# and gdp_pc is non-linear.
log_lm_gdp <- lm(log(health_pc) ~ gdp_pc)
summary(log_lm_gdp)

par(mfrow=c(2,2),mai=c(0.7,0.7,0.2,0.2), mgp = c(2,1,0))
plot(log_lm_gdp)

# In the first graph residuals follow a non-linear pattern, indicating that there is not
# a linear relationship between log(health_pc) and gdp_pc, clearly shown by the scatterplot 
# log(health_pc) vs gdp_pc.
plot(gdp_pc,log(health_pc))

# How do we interprete the coefficients of this model?
# E[logY] = b0 + b1*x
# E[log(Y + DeltaY)] = b0 + b1*(x + Deltax) = E[logY] + b1*Deltax
# E[DeltaY/Y] =~ E[log(1 + DeltaY/Y)] = b1*Deltax, (DeltaY small)
# 100E[DeltaY/Y] =~ 100*beta1*Deltax
# The left-hand side is %-change in Y , so we can say that Y increases of 100b1% for each 
# unitary increment of X.


# Now, we log-transform the predictor as well, obtaining the so-called log-log model.
# log(health_pc) = b0 + b1 log(gpd_pc) + eps.

loglog_lm_gdp <- lm(log(health_pc) ~ log(gdp_pc))
plot(log(gdp_pc),log(health_pc),pch=16)
abline(coef(loglog_lm_gdp), col = "red")

summary(loglog_lm_gdp)
par(mfrow=c(2,2),mai=c(0.7,0.7,0.2,0.2), mgp = c(2,1,0))
plot(loglog_lm_gdp)
# How do we interprete the coefficients of this model?
# E[logY] = ??0 + ??1*logx
# E[log(Y+???Y)] = ??0 + ??1*log(x + ???x)
# E[log(Y + ???Y)???logY] = ??1 (log(x + ???x)???log x)
# E[???Y /Y ] ' E[log (1 + ???Y /Y )] ' ??1 ???x/x, (???Y small)
# ??1 =~ 100E[???Y/Y] / 100???x/x

# We can interprete ??1 as the ratio between the %-change in Y and the %-change in X, so we
# can say that ??1 is the expected %-change in Y for an increment of 1% of X.
library(car)
influencePlot(loglog_lm_gdp, xlim = c(0, 0.05))

# Each circle represents an observation, located at a point with coordinates given by the 
# corresponding leverage and (studentized) residual: the area of the circle is proportional 
# to the Cook's Distance of the point. The wider circles represent potentially influential 
# observations. Two dashed horizontal lines are drawn in correspondence to the values -2 
# and +2 for the studentized residuals: circles with center outside this band are potential
# outliers. The two dashed vertical lines correspond instead to twice and three times the 
# average leverage: points above the second line might be considered high-leverage 
# observations. Together with the plot, R reports the values of these three quantities for 
# the most problematic observations, identified by their number in the data set: here we 
# have 4 observations. To sum up, from the plot we see two observations with a high-leverage
# but none of these seems to be really influential, indeed even if the corresponding circle 
# of 177 has a wide area, no Cook's D is larger than 1.



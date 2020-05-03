######################################################################
###
###  191008pyjamas_analysis.r  - One-way ANOVA
###  Date: 08/10/2019
###  Course: Data Spaces / Modelli Statistici
###
#######################################################################

### Description of the data
# The resistance to heat of the fabric of 55 pyjamas is tested, according to the type
# of fabric. The data.frame contains
# 1) y = result of the test of resistance, i.e. measure of the time the fabric takes to burn 
# 2) fabric = type of fabric (5 types of fabric are tested)
#
# Question: is there a significant difference of resistance between the fabrics?

### Clean the environment
rm(list = ls())

### Set the directory
#setwd(dir)

### Import the data
# Read the .csv file
library(RCurl)
data = getURL('https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/12%20-%20pyjamas_data.csv?token=ACNKBBA4VM7AQQHERISFJR26V2NX2')
pyjamas <- read.csv(text = data, header=T)

attach(pyjamas)

### Descriptive statistics
# number of rows, of columns, names of the variables
nrow(pyjamas)
ncol(pyjamas)
names(pyjamas)
str(pyjamas)
is.factor(fabric)
# Note that the variable "fabric" is integer, but this has not a meaning.
# The type of fabric is qualitative (coded 1,2,3,4,5), 
# we should not describe it quantitatively
fabric <- as.factor(fabric)
is.factor(fabric)
levels(fabric)

# Level of resistance, overall
summary(y)

# Description of the resistance by type of fabric
for (i in 1:5) {
  print(paste("Fabric: ", i, sep=""))
  print(length(y[which(fabric==i)]))
  print(summary(y[which(fabric==i)]))
}

#par(mar=c(2,2,1,1))
# more visual: the boxplot
boxplot(y~fabric,data=pyjamas, 
        main="Distribution of the resistance\naccording to the type of fabric",
        xlab="Type of fabric", ylab="Resistance") 

# One-way ANOVA 
# In a one-way ANOVA, you are interested in comparing the dependent variable means
# of two or more groups defined by a categorical grouping factor.

aggregate(y, by=list(fabric), FUN=mean)
# It appears that the most resistent is fabric type "5",
# whereas fabric type "4" is the less resistent.

# Model: y_{ik} = nu + alpha_i + eps_{ik}, i = 1,...,I, k = 1,...,n_i
# where 
# - y_{ik} is the observed value of variable Y in the k-th observation of group i;
# - alpha_i is the effect of factor i

# Test for group differences
fit <- aov(y ~ fabric)
summary(fit)

# The ANOVA F test for fabric is significant (p < .01), providing evidence that
# the five fabrics aren't all equally resistent.
# So we can reject the null hypoteses H0: mu_i = mu for i=1:5.

anova1 <- lm(y ~ factor(fabric) )
summary(anova1)
# fabric 1 -> resistance is 3.336
# fabric 2 -> the resistance increases
#             by + 0.264 (SE=0.17302) units compared to fabric 1
# fabric 3 -> the resistance decreases by 0.036 (SE=0.17302)
#             units compared to fabric 1
# ...

# F-test for model comparison
# Reduced model = model with intercept only
# Full model = model with intercept + fabric
anova(lm(y ~1), anova1) 
# Confirms that there are differences between the groups

detach(pyjamas)


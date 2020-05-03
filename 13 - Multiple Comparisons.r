########################################################################################
########################################################################################
#########################         Multiple Comparisons         #########################
########################################################################################
########################################################################################


### 1. Dataset description

# The dataset consists in some clinical trial results which compared three doses 
# (Low dose = 1, Medium dose = 2, High dose = 3) of an experimental treatment versus 
# placebo (Placebo = 0) in Major Depressive Disorder (MDD). The purpose of the study 
# was to find which of the doses, if any, is more efficacious than placebo: is
# there at least one dose which is statistically significantly better than the placebo?
# The response variable is the Hamilton Depression Rating Scale 17 items (HAM-D17) score 
# (see Wikipedia) after 6 weeks of treatment. The HAM-D17 total score ranges from 0 to 
# 52, with higher values indicating a higher severity of illness.


rm(list = ls())
#setwd(dir)

# Libraries
library(gplots)
library(RCurl)
data = getURL('https://raw.githubusercontent.com/gianluigilopardo/statistical-models/master/13%20-%20data_depressivedisorder.csv?token=ACNKBBAFBJT3ALGYN65FCJC6V2N6W')
mdd <- read.csv(text = data, header=T)
attach(mdd)
# Descriptive statistics
#nrow(mdd)
#ncol(mdd)
dim(mdd)
names(mdd)
str(mdd)

# We see that the column "X" indicating the subject, simply indicates the row, so we can
#  delete it.
mdd <- mdd[,-1]
# Check that the variable "dose" is a factor
is.factor(dose)

# False, so force it to be a factor
dose = as.factor(dose)
levels(dose)

# Group sample sizes
table(dose)

# Dataset:
# - Dose = 4 groups (Low dose=1, Medium dose=2, High dose=3, Placebo=0)
# - HAM-D17 = value of the HAM-D17 total score
# - 50 patients / group => 200 patients in total

# Boxplot to see the descriptive statistics
boxplot(HAMD17~dose,main="HAM-D17 Total Score\naccording to the dose",
        xlab="Dose (0=Placebo)", ylab="HAM-D17 Total Score", col =2:5)

# We see that there is an outlier
summary(mdd[which(dose==0),2])

sort(mdd[which(dose==0),2])



### 2. One-way anova

# It is a one-way anova since we have just one grouping factor.
fit.aov <- aov(HAMD17 ~ dose)
summary(fit.aov)

# The ANOVA F test for dose is significant (p < .05), providing evidence
# that the four treatments aren't all equally effective



### 3. Multiple comparisons

# Many scientific experiments involve the simultaneous evaluation of more than one question, 
# leading to multiplicity problem.
# Example Say you have a set of hypotheses that you wish to test simultaneously. The first 
# idea that might come to mind is to test each hypothesis separately, using some level of 
# significance ??, say 0.05. Test k null hypotheses using independent test statistics:
#   P(At least one Type I error) = 1 ??? P(Never incurr in Type I error) = 1 ??? (1 ??? ??)^k

k <- c(2,seq(10,100,by = 10))
pr <- 1-(1-0.05)^k
plot(k, pr, pch=16, xlab = "Number of Tests")
axis(1,at = k, labels = as.character(k))

# If we consider for example 20 tests, we have a 64% chance of observing at least one 
# significant result, even if all of the tests are actually not significant. How can 
# we prevent the Type I error rate to reach quickly 1? There are some multiple comparison 
# procedures wich adjust the ??'s of each test, so that P(At least one Type I error)
# is below your desired level of significance.

# Question: Is there at least one dose different from placebo? First, we do 3 tests:
#   1. Is low dose different from placebo?
#   H01 : ??1 = 0
#   H11 : ??1 != 0
# 2. Is medium dose different from placebo?
#   H02 : ??2 = 0
#   H12 : ??2 != 0
# 3. Is high dose different from placebo?
#   H03 : ??3 = 0
#   H13 : ??3 != 0


# The model is Yik = ? + ??i + eps_ik. Because linear models require numeric predictors, 
# when the lm() function encounters a factor, it replaces that factor with a set of 
# numeric variables representing contrasts among the levels. If the factor has k levels, 
# k ??? 1 contrast variables are created.
fit.lm <- lm(HAMD17 ~ dose)
summary(fit.lm)

# Interpretation:
# 1. When dose = 0 (placebo), the expected HAM-D17 Total score is 18.300 (SE=1.145) points
# 2. When dose = 1 (low dose), the HAM-D17 Total score decreases by 2.500 (SE=1.619) points
#    compared to the placebo
# 3. When dose = 2 (medium dose), the HAM-D17 Total score decreases by 3.880 (SE=1.619) 
#    points compared to the placebo
# 4. When dose = 3 (high dose), the HAM-D17 Total score decreases by 5.380 (SE=1.619) points 
#    compared to the placebo


## Non-parametric procedures
# We have three null hypotheses.

# Bonferroni: test each hypothesis at ??/3 = 0.0167, so we have adjusted the significance level
# 1. p1 = 0.12420 > 0.0167 ??? NOT REJECTED
# 2. p2 = 0.01750 > 0.0167 ??? NOT REJECTED
# 3. p3 = 0.00106 < 0.0167 ??? REJECTED
# or we can compute the adjusted p-values, to be compared with the significance level ?? = 0.05.
raw.p.value=summary(fit.lm)$coefficients[2:4,4]
# Bonferroni
p.value.bonf=p.adjust(raw.p.value, method = "bonferroni")
# 1.242008e-01*3 1.750199e-02*3 1.063671e-03*3
# Conclusion: at least one dose is significantly different from placebo, and it is high dose.

# Holm: order the p-values from the lowest to the largest, p(1) = p3, p(2) = p2 and p(3) = p1
sort(raw.p.value)
# We test the first hypothesis at ??/3 = 0.0167, the second at ??/2 = 0.025 and the third at 
# ?? = 0.05, so we have:
# 1. p(1) = 0.00106 < 0.0167 ??? REJECTED and go on
# 2. p(2) = 0.0175 < 0.025 ??? REJECTED and go on
# 3. p(3) = 0.12420 > 0.05 ??? NOT REJECTED
# Holm
p.value.holm=p.adjust(raw.p.value, method = "holm")
#0.1242*1, 0.0175*2, 0.00106*3
sort(p.value.holm)>0.05
# Conclusion: medium dose and high dose are different from placebo.

# Hochberg: It assume independence or positive dependence between the test statistics. 
# We order the p-values from the lowest to the largest, p(1) = p3, p(2) = p2 and p(3) = p1
# . If p(3) < ?? = 0.05 reject H(1), H(2), H(3) and stop, otherwise go on
# . If p(2) < ??/2 = 0.025 reject H(1), H(2) and stop, otherwise go on
# . If p(1) < ??/3 = 0.0167 reject H(1)
sort(raw.p.value)
# Hochberg
p.value.hoch=p.adjust(raw.p.value, method = "hochberg")
# . p(3) = 0.12420 > 0.050 ??? H01 NOT REJECTED, we continue
# . p(2) = 0.01750 < 0.0250 ??? WE REJECT H02 and H03
# Conclusion: medium dose and high dose are different from placebo.

# Hommel: It assume independence or positive dependence between the test statistics. 
# We order the p-values from the lowest to the largest, p(1) = p3, p(2) = p2 and p(3) = p1
# . If p(3) > ?? = 0.05 accept H(3) and go on, otherwise reject all the hypotheses and stop
# . For k = 2, if p(3???2+1) = p(2) > ??/2 = 0.025 and p(3???2+2) = p(3) > ?? ??? 2/2 = 0.05, accept 
# H(3???2+1) = H(2) and go on, otherwise stop and reject the hypotheses H(1), H(2) such that p(1),
# p(2) ??? ??/(2 ??? 1) = ??
# For k = 3, if p(3???3+1) = p(1) > ??/3 = 0.0167, p(3???3+2) = p(2) > ?? ??? 2/3 = 0.033, 
# p(3???3+3) = p(3) > ?? ??? 3/3 = 0.05, accept H(3???3+1) = H(1) and go on, otherwise stop and reject 
# the hypothesis H(1) such that p(1) ??? ??/(3 ??? 1) = ??/2
# . If p(1) ??? ??/3 = 0.0167 reject H(1)
p.value.hommel=p.adjust(raw.p.value, method = "hommel")
sort(p.value.hommel)
p.value.hommel>0.05
# . p(3) = 0.12420 > 0.050, accept H(3) = H01
# . p(3) = 0.1242 > 0.05 but p(2) = 0.01750 < 0.025 so we see that p(1) = 0.00106 < 0.05 
#   and p(2) = 0.01750 < 0.05, and H(1) = H03 and H(2) = H02 are rejected
# Conclusion: medium dose and high dose are different from placebo.


# Assignment 4
library(tidyverse)
library(stargazer)
library(magrittr)
library(haven)
library(plm)
library(data.table)
library(fastDummies)
library(matrixcalc)
library(glue)
library(plotly)
library(snow)
library(nloptr)
library(boot)
library(data.table)
# Problem 1
# (a) 
dat_A4 = dat_A4 %>% mutate(age = 2019 - KEY_BDATE_Y_1997)
dat_A4[is.na(dat_A4)] = 0
dat_A4 = dat_A4 %>% mutate(work_exp = (CV_WKSWK_JOB_DLI.01_2019 + CV_WKSWK_JOB_DLI.02_2019 + CV_WKSWK_JOB_DLI.03_2019 +CV_WKSWK_JOB_DLI.04_2019 + CV_WKSWK_JOB_DLI.05_2019 + CV_WKSWK_JOB_DLI.06_2019 + CV_WKSWK_JOB_DLI.07_2019 + CV_WKSWK_JOB_DLI.08_2019 + CV_WKSWK_JOB_DLI.09_2019 + CV_WKSWK_JOB_DLI.10_2019 + CV_WKSWK_JOB_DLI.11_2019)/52)
# (b)
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 8] = 20
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 7] = 20
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 6] = 18
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 5] = 16
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 4] = 14
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 3] = 12
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 2] = 10
dat_A4$YSCH.3113_2019[dat_A4$YSCH.3113_2019 == 1] = 8
# (c)
posinc = dat_A4[!dat_A4$YINC_1700_2019 == 0,]
ggplot(posinc %>% group_by(age), aes(x = age, y = YINC_1700_2019)) + geom_point() + ylab("Income")
ggplot(posinc %>% group_by(age) %>% summarize(averageincome = mean(YINC_1700_2019)), aes(x = age, y = averageincome)) + geom_line() + ylab("Average Income")
ggplot(posinc %>% group_by(KEY_SEX_1997), aes(x = KEY_SEX_1997, y = YINC_1700_2019)) + geom_point() + ylab("Income") + xlab("Gender")
ggplot(posinc %>% group_by(KEY_SEX_1997) %>% summarize(averageincome = mean(YINC_1700_2019)), aes(x = KEY_SEX_1997, y = averageincome)) + geom_line() + ylab("Average Income") + xlab("Gender")
ggplot(posinc %>% group_by(CV_BIO_CHILD_HH_U18_2019), aes(x = CV_BIO_CHILD_HH_U18_2019, y = YINC_1700_2019)) + geom_point() + ylab("Income") + xlab("Child")
ggplot(posinc %>% group_by(CV_BIO_CHILD_HH_U18_2019) %>% summarize(averageincome = mean(YINC_1700_2019)), aes(x = CV_BIO_CHILD_HH_U18_2019, y = averageincome)) + geom_line() + ylab("Average Income") + xlab("Child")
dat_A4 %>% group_by(age) %>% count(YINC_1700_2019 == 0)
dat_A4 %>% group_by(KEY_SEX_1997) %>% count(YINC_1700_2019 == 0)
dat_A4 %>% group_by(CV_BIO_CHILD_HH_U18_2019) %>% count(YINC_1700_2019 == 0)

# Problem 2
# (a)
colnames(dat_A4) = c("...1", "X", "ID", "Gender", "bdaym", "bdayy", "samtypr", "biodad", "biomom", "resdad", "resmom", "race", "sat", "hhsize", "marstat", "child", "urbanrural", "job1", "job2", "job3", "job4", "job5", "job6", "job7", "job8", "job9","job10", "job11", "yearssch", "income", "age", "work_exp", "inlf", "IMR")
dat_A4 = dat_A4 %>% mutate(black = ifelse(race == 1, 1, 0))
dat_A4 = dat_A4 %>% mutate(hispanic = ifelse(race == 2, 1, 0))
dat_A4 = dat_A4 %>% mutate(nonbnonh = ifelse(race == 4, 1, 0))
dat_A4$Gender = dat_A4$Gender - 1
dat_A4 = dat_A4[dat_A4$hhsize != 0,]
posinc = dat_A4[dat_A4$income != 0,]
ols = lm(log(income) ~ Gender + biodad + biomom + resdad + resmom + hhsize + marstat + child + urbanrural + yearssch + age + work_exp + black + hispanic + nonbnonh, data = posinc)
summary(ols)
# We have sample selection bias because we exclude people who have 0 wages, 
# therefore, the sample is not random. We can not observe the behavior of 
# those who have no income.
# (b) Heckman two-stage model can deal with this sample selection problem. 
# The first stage will estimate the probability of a person participate in
# the labor market
# (c)
dat_A4 = dat_A4 %>% mutate(inlf = ifelse(income != 0, 1, 0))
inlf = glm(inlf ~ Gender + biodad + biomom + resdad + resmom + hhsize + marstat + child + urbanrural + yearssch + age + work_exp + black + hispanic + nonbnonh, family=binomial(link="probit"), data=dat_A4)
summary(inlf)
# Calculate inverse Mills ratio
dat_A4$IMR = dnorm(inlf$linear.predictors)/pnorm(inlf$linear.predictors)
posinc = dat_A4[!dat_A4$income == 0,]
# Second stage
heckman = lm(log(income) ~ IMR + Gender + biodad + biomom + resdad + resmom + hhsize + marstat + child + urbanrural + yearssch + age + work_exp + black + hispanic + nonbnonh, data = posinc)
summary(heckman)

# Problem 3
# (a) 100,000 is the top-coded value
hist(posinc$income, breaks = 100)
# (b) I propose Tobit model
posinc = posinc %>% mutate(censor = ifelse(income == 100000, 1, 0))
par = c(-0.4129, 0.000635, 0.0030392, 0.0028173, 0.0032969, -0.0524151, 0.0391630, 0.0695484, 0.1346791, 0.0979461, 0.0048824, 0.0337307, -0.1095982, 0.0460811, 0.0069920)
X = posinc[,c(4,8,9,10,11,14,15,16,17,29,31,32,35,36,37)]
X = as.matrix(X)
y = as.matrix(posinc$income)
sigma = sd(log(y) - X %*% par)
par = matrix(data = par, nrow = 15, ncol = 1)
censor = as.matrix(posinc[,38])
flike = function(X, y, par, sigma, censor){
  xbeta = X %*% par
  cdf = pnorm((xbeta - log(100000)) / sigma)
  pdf = dnorm((log(y) - xbeta)/sigma)
  like = censor*log(cdf) + (1-censor)*(log(pdf)-log(sigma))
  return(sum(-like))
}
flike(X, y, par, sigma, censor)
start = par
res = optim(start, fn=flike, method="BFGS", control=list(trace=0,maxit=1000),X = X,,y = y,sigma=sigma,censor=censor, hessian = TRUE)
tobitlike = data.table(res$par)
tobitlike = as.data.frame(tobitlike)
colnames(tobitlike) = "Estimates"
rownames(tobitlike) = c("Gender", "biodad", "biomom", "resdad", "resmom", "hhsize", "marstat", "child", "urbanrural", "yearssch", "age", "work_exp", "black", "hispanic", "nonbnonh")
tobitlike

# Problem 4
# (a) For example, if people who have more years of education have higher wages, that might due to their higher ability, not years of education. Therefore, we have omitted variable problem. We need to get rid of unobserved ability factor.
# (b) estimate effect of education, marital status and experience
nonpanel = read_csv("dat_A4.csv")
panel = dat_A4_panel
panel[is.na(panel)] = 0
panel = panel %>% mutate(age_97 = 1997 - KEY_BDATE_Y_1997)
write.csv(panel, file = "panel.csv")
# I used excell to calculate the experience for each year
panel = read_csv("panel.csv")
panel = panel[,-c(5:11)]
panel = panel[,-c(9:17)]
panel = panel[,-c(13:21)]
panel = panel[,-c(17:25)]
panel = panel[,-c(21:28)]
panel = panel[,-c(25:35)]
panel = panel[,-c(28:37)]
panel = panel[,-c(32:38)]
panel = panel[,-c(36:44)]
panel = panel[,-c(40:48)]
panel = panel[,-c(44:51)]
panel = panel[,-c(48:55)]
panel = panel[,-c(52:59)]
panel = panel[,-c(57:65)]
panel = panel[,-c(62:74)]
panel = panel[,-c(67:76)]
panel = panel[,-c(71:82)]
panel = panel[,-c(75:89)]
panel = panel[,-c(79:89)]
panel$CV_HIGHEST_DEGREE_9899_1998[panel$CV_HIGHEST_DEGREE_9899_1998 == 0] = 8
panel$CV_HIGHEST_DEGREE_9899_1998[panel$CV_HIGHEST_DEGREE_9899_1998 == 1] = 10
panel$CV_HIGHEST_DEGREE_9899_1998[panel$CV_HIGHEST_DEGREE_9899_1998 == 2] = 12

panel$CV_HIGHEST_DEGREE_9900_1999[panel$CV_HIGHEST_DEGREE_9900_1999 == 0] = 8
panel$CV_HIGHEST_DEGREE_9900_1999[panel$CV_HIGHEST_DEGREE_9900_1999 == 1] = 10
panel$CV_HIGHEST_DEGREE_9900_1999[panel$CV_HIGHEST_DEGREE_9900_1999 == 2] = 12

panel$CV_HIGHEST_DEGREE_0001_2000[panel$CV_HIGHEST_DEGREE_0001_2000 == 0] = 8
panel$CV_HIGHEST_DEGREE_0001_2000[panel$CV_HIGHEST_DEGREE_0001_2000 == 1] = 10
panel$CV_HIGHEST_DEGREE_0001_2000[panel$CV_HIGHEST_DEGREE_0001_2000 == 2] = 12
panel$CV_HIGHEST_DEGREE_0001_2000[panel$CV_HIGHEST_DEGREE_0001_2000 == 3] = 14

panel$CV_HIGHEST_DEGREE_0102_2001[panel$CV_HIGHEST_DEGREE_0102_2001 == 0] = 8
panel$CV_HIGHEST_DEGREE_0102_2001[panel$CV_HIGHEST_DEGREE_0102_2001 == 1] = 10
panel$CV_HIGHEST_DEGREE_0102_2001[panel$CV_HIGHEST_DEGREE_0102_2001 == 2] = 12
panel$CV_HIGHEST_DEGREE_0102_2001[panel$CV_HIGHEST_DEGREE_0102_2001 == 3] = 14
panel$CV_HIGHEST_DEGREE_0102_2001[panel$CV_HIGHEST_DEGREE_0102_2001 == 4] = 16

panel$CV_HIGHEST_DEGREE_0203_2002[panel$CV_HIGHEST_DEGREE_0203_2002 == 0] = 8
panel$CV_HIGHEST_DEGREE_0203_2002[panel$CV_HIGHEST_DEGREE_0203_2002 == 1] = 10
panel$CV_HIGHEST_DEGREE_0203_2002[panel$CV_HIGHEST_DEGREE_0203_2002 == 2] = 12
panel$CV_HIGHEST_DEGREE_0203_2002[panel$CV_HIGHEST_DEGREE_0203_2002 == 3] = 14
panel$CV_HIGHEST_DEGREE_0203_2002[panel$CV_HIGHEST_DEGREE_0203_2002 == 4] = 16

panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 0] = 8
panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 1] = 10
panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 2] = 12
panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 3] = 14
panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 4] = 16
panel$CV_HIGHEST_DEGREE_0304_2003[panel$CV_HIGHEST_DEGREE_0304_2003 == 5] = 18

panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 0] = 8
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 1] = 10
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 2] = 12
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 3] = 14
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 4] = 16
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 5] = 18
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 6] = 20
panel$CV_HIGHEST_DEGREE_0405_2004[panel$CV_HIGHEST_DEGREE_0405_2004 == 7] = 20

panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 0] = 8
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 1] = 10
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 2] = 12
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 3] = 14
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 4] = 16
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 5] = 18
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 6] = 20
panel$CV_HIGHEST_DEGREE_0506_2005[panel$CV_HIGHEST_DEGREE_0506_2005 == 7] = 20

panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 0] = 8
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 1] = 10
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 2] = 12
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 3] = 14
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 4] = 16
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 5] = 18
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 6] = 20
panel$CV_HIGHEST_DEGREE_0607_2006[panel$CV_HIGHEST_DEGREE_0607_2006 == 7] = 20

panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 0] = 8
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 1] = 10
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 2] = 12
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 3] = 14
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 4] = 16
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 5] = 18
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 6] = 20
panel$CV_HIGHEST_DEGREE_0708_2007[panel$CV_HIGHEST_DEGREE_0708_2007 == 7] = 20

panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 0] = 8
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 1] = 10
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 2] = 12
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 3] = 14
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 4] = 16
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 5] = 18
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 6] = 20
panel$CV_HIGHEST_DEGREE_0809_2008[panel$CV_HIGHEST_DEGREE_0809_2008 == 7] = 20

panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 0] = 8
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 1] = 10
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 2] = 12
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 3] = 14
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 4] = 16
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 5] = 18
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 6] = 20
panel$CV_HIGHEST_DEGREE_0910_2009[panel$CV_HIGHEST_DEGREE_0910_2009 == 7] = 20

panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 0] = 8
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 1] = 10
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 2] = 12
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 3] = 14
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 4] = 16
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 5] = 18
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 6] = 20
panel$CV_HIGHEST_DEGREE_1011_2010[panel$CV_HIGHEST_DEGREE_1011_2010 == 7] = 20

panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 0] = 8
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 1] = 10
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 2] = 12
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 3] = 14
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 4] = 16
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 5] = 18
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 6] = 20
panel$CV_HIGHEST_DEGREE_1112_2011[panel$CV_HIGHEST_DEGREE_1112_2011 == 7] = 20

panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 0] = 8
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 1] = 10
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 2] = 12
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 3] = 14
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 4] = 16
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 5] = 18
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 6] = 20
panel$CV_HIGHEST_DEGREE_1314_2013[panel$CV_HIGHEST_DEGREE_1314_2013 == 7] = 20

panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 0] = 8
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 1] = 10
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 2] = 12
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 3] = 14
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 4] = 16
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 5] = 18
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 6] = 20
panel$CV_HIGHEST_DEGREE_EVER_EDT_2015[panel$CV_HIGHEST_DEGREE_EVER_EDT_2015 == 7] = 20

panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 0] = 8
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 1] = 10
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 2] = 12
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 3] = 14
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 4] = 16
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 5] = 18
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 6] = 20
panel$CV_HIGHEST_DEGREE_EVER_EDT_2017[panel$CV_HIGHEST_DEGREE_EVER_EDT_2017 == 7] = 20

panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 0] = 8
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 1] = 10
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 2] = 12
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 3] = 14
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 4] = 16
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 5] = 18
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 6] = 20
panel$CV_HIGHEST_DEGREE_EVER_EDT_2019[panel$CV_HIGHEST_DEGREE_EVER_EDT_2019 == 7] = 20

write.csv(panel, file = "panel.csv")
# I change the name of each column to shorter ones
panel = read_csv("panel.csv")

panel = fastDummies::dummy_cols(panel, select_columns = "mstat98", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat99", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat00", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat01", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat02", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat03", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat04", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat05", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat06", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat07", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat08", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat09", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat10", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat11", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat13", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat15", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat17", remove_most_frequent_dummy = F, remove_selected_columns = F)
panel = fastDummies::dummy_cols(panel, select_columns = "mstat19", remove_most_frequent_dummy = F, remove_selected_columns = F)


# Between estimator
backup = panel
between = panel
between = between %>% mutate(Y = (inc98+inc99+inc00+inc01+inc02+inc03+inc04+inc05+inc06+inc07+inc08+inc09+inc10+inc11+inc13+inc15+inc17+inc19)/18)
between = between %>% mutate(educ = (edu98+edu99+edu00+edu01+edu02+edu03+edu04+edu05+edu06+edu07+edu08+edu09+edu10+edu11+edu13+edu15+edu17+edu19)/18)
between = between %>% mutate(exp = (exp98+exp99+exp00+exp01+exp02+exp03+exp04+exp05+exp06+exp07+exp08+exp09+exp10+exp11+exp13+exp15+exp17+exp19)/18)
between = between %>% mutate(mstat = (mstat98_1 + mstat99_1 +mstat00_1+mstat01_1+mstat02_1+mstat03_1+mstat04_1+mstat05_1+mstat06_1+mstat07_1+mstat08_1+mstat09_1+mstat10_1+mstat11_1+mstat13_1+mstat15_1+mstat17_1+mstat19_1)/18)
olsbetween = lm(Y ~ educ + exp + mstat, data = between)
summary(olsbetween)

# within Estimator
within = between
withinincome = gather(within, incyear, incwithin, c("inc98", "inc99", "inc00","inc01","inc02","inc03","inc04", "inc05", "inc06", "inc07", "inc08", "inc09", "inc10", "inc11", "inc13", "inc15", "inc17", "inc19"), factor_key=TRUE)
withinincome = withinincome[,c(143:148)]
withineduc = gather(within, educyear, educwithin, c("edu98","edu99","edu00","edu01","edu02","edu03","edu04","edu05","edu06","edu07","edu08","edu09","edu10","edu11","edu13","edu15","edu17","edu19"), factor_key=TRUE)
withineduc = withineduc[,c(147,148)]
withinexp = gather(within, expyear, expwithin, c("exp98","exp99","exp00","exp01","exp02","exp03","exp04","exp05","exp06","exp07","exp08","exp09","exp10","exp11","exp13","exp15","exp17","exp19"), factor_key=TRUE)
withinexp = withinexp[,c(147,148)]
withinmstat = gather(within, mstatyear, mstatwithin, c("mstat98_1", "mstat99_1","mstat00_1","mstat01_1","mstat02_1","mstat03_1","mstat04_1","mstat05_1","mstat06_1","mstat07_1","mstat08_1","mstat09_1","mstat10_1","mstat11_1","mstat13_1","mstat15_1","mstat17_1","mstat19_1"),factor_key=TRUE)
withinmstat = withinmstat[,c(147,148)]
within = cbind(withinincome, withineduc, withinexp, withinmstat)
within = within %>% mutate(Ywithin = incwithin-Y)
within = within %>% mutate(eduwithinols = educwithin-educ)
within = within %>% mutate(expwithinols = expwithin-exp)
within = within %>% mutate(mstatwithinols = mstatwithin-mstat)
olswithin = lm(Ywithin ~ eduwithinols + expwithinols + mstatwithinols, data = within)
summary(olswithin)

# Difference Estimator
Difference = panel
Difference = Difference %>% mutate(y1 = inc99-inc98)
Difference = Difference %>% mutate(y2 = inc00-inc99)
Difference = Difference %>% mutate(y3 = inc01-inc00)
Difference = Difference %>% mutate(y4 = inc02-inc01)
Difference = Difference %>% mutate(y5 = inc03-inc02)
Difference = Difference %>% mutate(y6 = inc04-inc03)
Difference = Difference %>% mutate(y7 = inc05-inc04)
Difference = Difference %>% mutate(y8 = inc06-inc05)
Difference = Difference %>% mutate(y9 = inc07-inc06)
Difference = Difference %>% mutate(y10 = inc08-inc07)
Difference = Difference %>% mutate(y11 = inc09-inc08)
Difference = Difference %>% mutate(y12 = inc10-inc09)
Difference = Difference %>% mutate(y13 = inc11-inc10)
Difference = Difference %>% mutate(y14 = inc13-inc11)
Difference = Difference %>% mutate(y15 = inc15-inc13)
Difference = Difference %>% mutate(y16 = inc17-inc15)
Difference = Difference %>% mutate(y17 = inc19-inc17)

Difference = Difference %>% mutate(Dedu1 = edu99 - edu98)
Difference = Difference %>% mutate(Dedu2 = edu00 - edu99)
Difference = Difference %>% mutate(Dedu3 = edu01 - edu00)
Difference = Difference %>% mutate(Dedu4 = edu02 - edu01)
Difference = Difference %>% mutate(Dedu5 = edu03 - edu02)
Difference = Difference %>% mutate(Dedu6 = edu04 - edu03)
Difference = Difference %>% mutate(Dedu7 = edu05 - edu04)
Difference = Difference %>% mutate(Dedu8 = edu06 - edu05)
Difference = Difference %>% mutate(Dedu9 = edu07 - edu06)
Difference = Difference %>% mutate(Dedu10 = edu08 - edu07)
Difference = Difference %>% mutate(Dedu11 = edu09 - edu08)
Difference = Difference %>% mutate(Dedu12 = edu10 - edu09)
Difference = Difference %>% mutate(Dedu13 = edu11 - edu10)
Difference = Difference %>% mutate(Dedu14 = edu13 - edu11)
Difference = Difference %>% mutate(Dedu15 = edu15 - edu13)
Difference = Difference %>% mutate(Dedu16 = edu17 - edu15)
Difference = Difference %>% mutate(Dedu17 = edu19 - edu17)

Difference = Difference %>% mutate(Dexp1 = exp99 - exp98)
Difference = Difference %>% mutate(Dexp2 = exp00 - exp99)
Difference = Difference %>% mutate(Dexp3 = exp01 - exp00)
Difference = Difference %>% mutate(Dexp4 = exp02 - exp01)
Difference = Difference %>% mutate(Dexp5 = exp03 - exp02)
Difference = Difference %>% mutate(Dexp6 = exp04 - exp03)
Difference = Difference %>% mutate(Dexp7 = exp05 - exp04)
Difference = Difference %>% mutate(Dexp8 = exp06 - exp05)
Difference = Difference %>% mutate(Dexp9 = exp07 - exp06)
Difference = Difference %>% mutate(Dexp10 = exp08 - exp07)
Difference = Difference %>% mutate(Dexp11 = exp09 - exp08)
Difference = Difference %>% mutate(Dexp12 = exp10 - exp09)
Difference = Difference %>% mutate(Dexp13 = exp11 - exp10)
Difference = Difference %>% mutate(Dexp14 = exp13 - exp11)
Difference = Difference %>% mutate(Dexp15 = exp15 - exp13)
Difference = Difference %>% mutate(Dexp16 = exp17 - exp15)
Difference = Difference %>% mutate(Dexp17 = exp19 - exp17)

Difference = Difference %>% mutate(Dmstat1 = mstat99 - mstat98)
Difference = Difference %>% mutate(Dmstat2 = mstat00 - mstat99)
Difference = Difference %>% mutate(Dmstat3 = mstat01 - mstat00)
Difference = Difference %>% mutate(Dmstat4 = mstat02 - mstat01)
Difference = Difference %>% mutate(Dmstat5 = mstat03 - mstat02)
Difference = Difference %>% mutate(Dmstat6 = mstat04 - mstat03)
Difference = Difference %>% mutate(Dmstat7 = mstat05 - mstat04)
Difference = Difference %>% mutate(Dmstat8 = mstat06 - mstat05)
Difference = Difference %>% mutate(Dmstat9 = mstat07 - mstat06)
Difference = Difference %>% mutate(Dmstat10 = mstat08 - mstat07)
Difference = Difference %>% mutate(Dmstat11 = mstat09 - mstat08)
Difference = Difference %>% mutate(Dmstat12 = mstat10 - mstat09)
Difference = Difference %>% mutate(Dmstat13 = mstat11 - mstat10)
Difference = Difference %>% mutate(Dmstat14 = mstat13 - mstat11)
Difference = Difference %>% mutate(Dmstat15 = mstat15 - mstat13)
Difference = Difference %>% mutate(Dmstat16 = mstat17 - mstat15)
Difference = Difference %>% mutate(Dmstat17 = mstat19 - mstat17)

Differencemstat = Difference[,c(212:228)]
Differenceexp = Difference[,c(195:211)]
Differenceedu = Difference[,c(178:194)]
Differenceinc = Difference[,c(161:177)]

Differencemstat = gather(Differencemstat, mstatyear, differencemstat,Dmstat1:Dmstat17, factor_key =TRUE)
Differenceexp = gather(Differenceexp, expyear, differenceexp,Dexp1:Dexp17, factor_key =TRUE)
Differenceedu = gather(Differenceedu, eduyear, differenceedu, Dedu1:Dedu17, factor_key = TRUE)
Differenceinc = gather(Differenceinc, incyear, differenceinc, y1:y17, factor_key = TRUE)
Difference = cbind(Differencemstat,Differenceexp,Differenceedu,Differenceinc)

Differenceols = lm(differenceinc ~ differenceedu + differenceexp +differencemstat, data = Difference)
summary(Differenceols)










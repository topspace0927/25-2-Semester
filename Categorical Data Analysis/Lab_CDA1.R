##############################################################
## Categorical Data Analysis 1 - Inference about proportions
##############################################################

rm(list=ls())      # remove all

# 1. Read data files
Clinical <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Clinical.dat",
                       header=TRUE)
Clinical
dim(Clinical)


# 2. Test about proportions
attach(Clinical)
y <- sum(response) # sums 0 and 1 values to get number of successes
prop.test(y, n=10, conf.level=0.95, correct=FALSE) # (score) test & CI

# manually
n <- length(response); n
phat <- mean(response); phat
p0 <- 0.5
z0 <- (phat-p0)/sqrt(p0*(1-p0)/n); z0
(1-pnorm(z0))*2
z0^2
1-pchisq(z0^2,1)

# other ways to get identical CI
prop.test(y, 10, correct=FALSE)$conf.int
with(Clinical, prop.test(y, 10, correct=FALSE)$conf.int)

# check how to get score CI
(0.9-0.59585)/sqrt(0.59585*(1-0.59585)/n)
(0.9-0.9821238)/sqrt(0.9821238*(1-0.9821238)/n)


# legalized abortion example - check manually!
prop.test(837, 1810, p=0.50, alternative="two.sided", correct=FALSE)
prop.test(837, 1810, p=0.50, alternative="less", correct=FALSE)



# 3. Confidence intervals about proportions
install.packages("binom")
library(binom)

binom.confint(9, 10, conf.level=0.95, method="asymptotic") # Wald
binom.confint(9, 10, conf.level=0.95, method="wilson")     # Score

# manually
se <- sqrt(phat*(1-phat)/n); se
c(phat-qnorm(0.975)*se, phat+qnorm(0.975)*se)


# 4. Exact test
binom.test(9, 10, 0.50, alternative = "greater")
binom.test(9, 10, 0.50, alternative = "two.sided")

dbinom(9,10,0.5) + dbinom(10,10,0.5)

sum(dbinom(c(0,1,9,10),10,0.5))
# or
sum(dbinom(c(9,10),10,0.5))*2 # since binomial with p=0.5 is symmetric

# Author: Vuyolwethu Zumani
# Student number: g17z1278
# Last updated: 10/10/2019
# Bayesian Statistics Practical 

#______________________________________________________________________________________________________________________
#1
#(a) Done on book



#.....................................................................................
#(b)
#An experiment was then conducted, and it was found that 6 confessions out of a sample
#of 30 had comments deleted. Use this data and the prior from (a) to determine the
#posterior distribution of Pi
#Pi|x~Beta(aprime=a+x,bprime=b+n-x)
#2/20 3/12 3/10 1/10 .

Epi<-mean(c(2/20, 3/12, 3/10,1/10))
Epi # a/(a+b)
varpi<-var(c(2/20, 3/12, 3/10,1/10))
varpi #ab/((a+b)^2(a+b+1))

# Suppose we have a beta prior with
a<-2.500919
b<-10.837316
# Suppose we observe data with
n <- 30
s <- 6  # num of successes
f<- n-s #num of failures



#.....................................................................................
#(c) 
#This computes a plot of the likelihood, prior and posterior
# help(triplot, package="LearnBayes")
triplot(c(a,b),c(s,f)) # graphs beta(a,b) prior with s=x successes 
#and f=n-x failures



#.....................................................................................
#(d) 
#posterior mean knowing the posterior is a beta with parameter 
#a_prime and b_prime:
# The posterior is a beta with
a_prime <- s+a 
b_prime <- b+n-s
ePiGivenx<-a_prime/(a_prime+b_prime)
ePiGivenx



#.....................................................................................
#(e)
#Determine a 90% CI for the proportion
qbeta(c(0.025,0.975),a_prime,b_prime)



#.....................................................................................
#(f)
# To attain the  probability that the proportion is larger 
#than 0.25:
1-pbeta(0.25,a_prime,b_prime)



#.....................................................................................
#(g)
# Using the R function rbeta, simulate 10 000 values from the posterior distribution in
#R. Use set.seed(1) when simulating these values.
set.seed(1)
ps <- rbeta(10000,a_prime,b_prime)



#.....................................................................................
#(h)
#(i)  a 90% CI for pi can be estimated as 
quantile(ps,c(0.025,0.975))
#(ii) The probability that the proportion is larger than
# 0.25 is estimated using the proportion of simulated
# values in this range 
sum(ps>=0.25)/10000
#(iii) mean can be estimated as
mean(ps)

#______________________________________________________________________________________________________________________
#2
#(a) Done on book (proof)



#.....................................................................................
#(b) In a life-testing illustration, five bulbs are tested with observed burn times (in hours)
#of 751, 594, 1213, 1126 and 819. Using the R function rgamma, simulate 10 000 values
#from the posterior distribution of theta. Use set.seed(1) for these simulated values.
set.seed(1)
n=5
x=c(751, 594, 1213, 1126, 819)
a=n
b=sum(x)
psforGamma<-rgamma(10000,a,b)



#.....................................................................................
#(c)
psforInv_G=1/psforGamma
# Since 1/Gamma follows an inverse Gamma distribution
# And R does not have a built in density for inverse
#Gamma



#.....................................................................................
#(d) Estimate the probability that lamba exceeds 1000 hours.
sum(psforInv_G>1000)/10000
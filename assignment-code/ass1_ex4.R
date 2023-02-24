
## EXERCISE 4A
my.data = npk; my.data
my.data$yield = NULL
my.vec = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
my.data$N = my.vec
my.data$P = my.vec
my.data$K = my.vec
my.data
for (i in 1:6) {
  nitr = sample.int(4,2); nitr
  phoss = sample.int(4,2); phoss 
  k = sample.int(4,2); k
  
  nitr = nitr + (4 * (i-1)); nitr
  phoss = phoss + (4 * (i-1)); phoss
  k = k + (4 * (i-1));k
  my.data$N[nitr] = 1; my.data$N[nitr]
  my.data$P[phoss] = 1;
  my.data$K[k] = 1;
  
}
# randomized soil additives per block
my.data


##  EXERCISE 4B
nitr = numeric(6)
nonitr = numeric(6)
for (i in 1:6){
  
  for (j in 0:1){
    cat("block", i, "N", j)
    mean = mean(npk[npk$block == i & npk$N == j,]$yield) 
    print(mean)
    
    if (j == 0){
      nonitr[i] = mean
    }
    else{
      nitr[i] = mean
    }
    
    
  }
}
nitr
nonitr
blocks = seq(1:6)
test = rbind(nonitr,nitr)
barplot(test,beside=T)


## EXERCISE 4C
npk$block = as.factor(npk$block)
npk$N = as.factor(npk$N)

df = npk[c("yield","block","N")]

# two way anova
twoway = lm(yield~block*N,data=df)
anova(twoway)


# not sign. interaction parameter, also interaction plots as evidence for gamma_ij = 0 (parallel).
interaction.plot(npk$N,npk$block,npk$yield); interaction.plot(npk$block,npk$N,npk$yield)

# -> compute anova on additive model
twoway2  = lm(yield~block+N,data=df)

# we obtain sign. values for block and N.
anova(twoway2)
summary(twoway2)
confint(twoway2)

# seems normally distributed residuals
par(mfrow=c(1,2)); qqnorm(residuals(twoway2))
plot(fitted(twoway2),residuals(twoway2))


# was it sensible to also include 'block' as factor in this model? --> comment
# yes, because significant (?)
# or no, because it changes nothing (except for location?) -> as N(PK) is distr. differently across every block and 
# there is no way of describing the difference of how this distr. is affecting the yield


# no friedman test! we need 24 blocks for this, to differentiate. see slide 37 of lec. 5 for explanation of this
friedman.test(npk$yield,npk$N,npk$block)



## EXERCISE 4D
npk
pairwise_N = lm(yield ~ N*block + P + K,data=npk)
pairwise_P = lm(yield ~ P*block + N + K,data=npk)
pairwise_K = lm(yield ~ K*block + P + N,data=npk)

anova(pairwise_N)
anova(pairwise_P)
anova(pairwise_K)

additive = lm(yield ~ N+P+K+block, data=npk)
anova(additive)

# make 3 models -> two way anova
# lm(yield ~ N*block + P + K), etc...
# favorite model is without pairwise interaction term (so no block), just additive


### EXERCISE 4E

library(lme4)
mer1 = lmer(yield ~ N+P+K+(1|block),data=npk,REML=FALSE)
mer2 = lmer(yield ~ P+K+(1|block),data=npk,REML=FALSE)
anova(mer1,mer2)

# we see that anova of two models results in sign. p value. s.t. N has a significant impact on the yield.
# compare to 4C!




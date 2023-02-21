
## EXERCISE 4A
data = npk; data
data$yield = NULL
vec = rep(0, 24)
data$N = vec
data$P = vec
data$K = vec
data

# for each block, select two random positions for each additive.
# concatenate everything put it into the dataframe
for (i in 1:6) {
  n = sample.int(4,2); n
  p = sample.int(4,2); p
  k = sample.int(4,2); k
  
  n = n + (4 * (i-1)); n
  p = p + (4 * (i-1)); p
  k = k + (4 * (i-1)); k
  my.data$N[n] = 1;
  my.data$P[p] = 1;
  my.data$K[k] = 1;
  
}

# result: randomized soil additives per block
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

df = npk[c("yield","block","N")]; df

# two way anova
twoway = lm(yield~block*N,data=df)
anova(twoway)
summary(twoway)
confint(twoway)

# was it sensible to also include 'block' as factor in this model? --> comment
# friedman test? --> comment


## EXERCISE 4D





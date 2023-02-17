
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

df = npk[c("yield","block","N")]; df

# two way anova
twoway = lm(yield~block*N,data=df)
anova(twoway)
summary(twoway)
confint(twoway)

# was it sensible to also include 'block' as factor in this model? --> comment
# friedman test? --> comment


## EXERCISE 4D







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

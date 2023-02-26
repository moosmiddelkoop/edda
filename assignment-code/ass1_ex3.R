data = read.table('data/diet.txt',header=TRUE)

weight.lost = data$preweight - data$weight6weeks
data = cbind(data,weight.lost)

pre = data$preweight
post = data$weight6weeks
age = data$age

# test sign. difference -> check assumptions (normality etc.) otherwise other test is needed
par(mfrow = c(1,3))
boxplot(data$weight.lost[data$diet == 1], main="Diet 1", ylab="Weight lost")
boxplot(data$weight.lost[data$diet == 2], main="Diet 2", ylab="Weight lost")
boxplot(data$weight.lost[data$diet == 3], main="Diet 3", ylab="Weight lost")
# @ plotto -> hier heb ik boxplots van verschil in weight lost per diet, doe ff commenten hierover

par(mfrow = c(1,2))
boxplot(data$pre)
boxplot(data$post)
# @ plotto -> deze doet het nog niet helemaal zoals ik wil, maar wil gewoon normale boxplotto als summary van data


t.test(pre,post,paired=TRUE)
# @ plotto -> deze test laat zien dat het verschil in means van pre en post niet 0 is. (dus diff.)

# @ plotto -> is de data paired? jazeker! want dit zijn steeds dezelfde personen die de meting doen. 


# add boxplots of data -> 
# add boxplots of weight lost -> check!
# add qq plots -> is this right?
# add shapiro wilk

# plot hist of loss per diet
for (i in 1:3){
   hist(data$weight.lost[data$diet == i])
}
mycol = rgb(0, 0, 255, max = 255, alpha = 20, names = "blue50")
mycol2 = rgb(0, 255, 0, max = 255, alpha = 100, names = "green50")


# male vs female loss
femalehist = hist(data$weight.lost[data$gender == 0], plot=FALSE)
malehist = hist(data$weight.lost[data$gender == 1], plot=FALSE)

plot(femalehist, col=mycol)
plot(malehist, col=mycol2, add=TRUE)


# lin reg of age and weight loss -> no relation!
reg = lm(weight.lost ~ age)
plot(age, weight.lost)
abline(reg)
summary(reg)



## ANOVA (Ex. 3B)
# @gijs - waarom stripchart? en als je t al doet zou je t liefst de diets naat elkaar hebben toch?
boxplot(data); stripchart(data)
data$diet = as.factor(data$diet)
df = data[c("weight.lost","diet")]; df
is.factor(df$diet); is.numeric(df$diet)


weightlostaov = lm(weight.lost~diet,data=df)
anova(weightlostaov)
summary(weightlostaov)
confint(weightlostaov)

# plot residuals to see if normality assumption is not violated -> good!
par(mfrow=c(1,2)); qqnorm(residuals(weightlostaov))
plot(fitted(weightlostaov),residuals(weightlostaov))
# show that fitted(weightlostaov) is equal to the mean of the weight lost per diet

# can kruskal-wallis be used for this? -> can be used! However, we have seen that residuals are normally distributed, so usage of kruskal-wallis test is not needed.
# compare kruskal-wallis test results with those of one-way anova
kruskal.test(weight.lost, df$diet)


#TWO WAY ANOVA (ex. 3C)
data$gender = as.factor(data$gender)
df2 = data[c("weight.lost","diet","gender")]; df2

# use normal parametrization

# inspect interaction (diet * gender)
twoway = lm(weight.lost~diet*gender,data=df2)

# interaction variable is significant! -> Pr(>F) = 0.048, we use alpha = 0.05.
# hence no need to inspect further effects of main factor parameters (?) -> check slides lec. 5
anova(twoway)

# summary (parameter values) and confidence interval
summary(twoway)
confint(twoway)

# plot residuals to see if normality assumption is not violated -> good!
par(mfrow=c(1,2)); qqnorm(residuals(twoway))
plot(fitted(twoway),residuals(twoway))

# interaction plot????

## EX 3D
head(data)
height = data$height
diet= data$diet

# show interaction plots to assume a priori that all interactions are 0
par(mfrow=c(1,2))
interaction.plot(height,diet,weight.lost)
interaction.plot(diet,height,weight.lost)

# two way anova, also inspecting interaction
height_diet = lm(weight.lost ~ height*diet,data=data)

# two way anova results in not significant interaction parameter 
anova(height_diet)

# -> so we have to inspect only main factor parameters
height_diet2 = lm(weight.lost ~ height+diet,data=data)

# this results in significant diet factor parameter (Pr(>F) = 0.0056)
anova(height_diet2)

# now we check if the effect of height is the same for all 3 types of diet
# seems like diet 1 and 2 have the same effect, only diet 3 is significantly different from diet 1 (Pr(>F) = 0.00879)
summary(height_diet2)

# plot residuals of linear model -> looks good (normal). QQ plot looks good and fitted vs residuals looks random.
par(mfrow=c(1,2)); qqnorm(residuals(height_diet2))
plot(fitted(height_diet2),residuals(height_diet2))


## EX 3E

#### TO DO Stilllllllll


# what is the best model? second? because height is taken into account, but height does not seem to have a sign. effect!
# so why compute a 2-way anova when height is only interfering?
# we prefer the first model, only taking the sign. effects into account -> namely diet (also P values are lower: 0.003229 < 0.005612)

# because we already got rid of first differences (resulting in variable 'weight.lost'), we can interpret the ANOVA estimated parameters as without intercept
summary(weightlostaov)
# hence for first diet it is -3.3, second 0.27 (not sign.), third -1.8481.



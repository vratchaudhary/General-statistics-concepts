#When number of 0s are so lage that data doesnt fit any standard distibution (Tu 2002)

#why are there so many 0s- true or false absences

#sources of steps Zuur et al. 2009


#####load packages
library(lattice)
library(MASS)
require(pscl) # alternatively can use package ZIM for zero-inflated models
library(lmtest)


###### download data, I am using publicly avaialble data for ucla stat website
# 250 groups that went to the park were surveyed so as to how much fish they caught (count), how many children they 
# had in the group (child) and if they brought camper (camper)

#Based on this data we want to know whats the probability of catching 0 fish and 
#also based on predictor variables how many fish are caught
zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

head(zinb)
summary(zinb)
attach(zinb)
# histogram with x axis in log10 scale
ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()

#whats the percentage of 0

100 * sum(zinb$count==0)/nrow(zinb) # 56.8, preety high

#####exploratory plots

plot(count, camper)
plot(count, child)
# since its count data,  we should use possion gLM, even if the data is 0 inflated we should start with poisson glm

##poisson
#counts∼Poisson(μi)
#E(counts)=μi #mean
#var(Countsi)=μi
#log(μi)=α+β1∗camper


p1<- glm(count~ camper, family= 'poisson', data=zinb)
summary(p1)

# lets check for overdispersion  (mean >>> variance in the model) or underdispersion variance > mean 
x<- resid(p1, type='pearson')
n<- nrow(zinb)
p<- length(coef(p1))
sum(x^2)/n-p
# looks way overdispersed
#Overdispersion can bias parameter estimates; produce false significant relationships.
#underdisperion can mask truly significant relationships.


#lets try negative binomial model (another one for count data)
#counts∼NB(μi,theta)
#E(counts)=μi
#var(Countsi)=(μ+μ^2)/theta
#log(μi)=α+β1∗camper

p2<- glm.nb(count~camper, data=zinb)
summary(p2)

# lets check for overdispersion  (mean >>> variance in the model) or underdispersion variance > mean 
x<- resid(p2, type='pearson')
n<- nrow(zinb)
p<- length(coef(p2))+ 1 # plus 1 for variance param in nb
sum(x^2)/n-p
# looks underdispereded 


####Lets start what we were looking for zero inflated model
#counts∼ZIP(μi,πi); mu is for count and pi is for zero inflation
#E(counts)=μi∗(1−πi)
#var(counts)=(1−πi)∗(μi+πi∗μ^2i)

#log(μi)=β1+β2∗camper

#log(πi)=γ1+γ2∗camper

## It is possible to use different predcitor of count and different predictor of bernoulli  process 
library(pscl)
p3<- zeroinfl(count ~ camper | ## Predictor for the Poisson process
                  camper, ## Predictor for the Bernoulli process;
               dist = 'poisson',
               data = zinb)

summary(p3)
# lets check for overdispersion  (mean >>> variance in the model) or underdispersion variance > mean 
x<- resid(p3, type='pearson')
n<- nrow(zinb)
p<- length(coef(p3)) + 1# 
sum(x^2)/n-p
### less than before, still overdispersed


####lets repeat with zero inflated negative binomial
#counts∼ZINB(μi,πi)
#E(Countsi)=μi∗(1−πi)
#var(Countsi)=(1−πi)∗(μi+π2i/k)+μ2i∗(π2i+πi)
#log(μi)=β1+β2∗camper
#log(πi)=γ1+γ2∗camper# var


p4<- zeroinfl(count ~ camper | ## Predictor for the Poisson process
                camper, ## Predictor for the Bernoulli process;
              dist = 'negbin',
              data = zinb)
# lets check for overdispersion  (mean >>> variance in the model) or underdispersion variance > mean 
x<- resid(p4, type='pearson')
n<- nrow(zinb)
p<- length(coef(p4)) +1 # plus 1 for variance param in nb
sum(x^2)/n-p
##3underdispersion

#oh my, 
###3 lets compare the two zero inflation models with LRT

lrtest(p3,p4) ##apparently second model is better, lets stick to it 
##
library(MuMIn)
QAIC(p3, chat = 3.14) # use qaic if yuor data and model is overdispersed. 
##
# or introduce diff variable
p6<- zeroinfl(count ~ camper+child | ## Predictor for the Poisson process
                child, ## Predictor for the Bernoulli process;
              dist = 'negbin',
              data = zinb)
summary(p6)
# lets check for overdispersion  (mean >>> variance in the model) or underdispersion variance > mean 
x<- resid(p6, type='pearson')
n<- nrow(zinb)
p<- length(coef(p6)) +1 # plus 1 for variance param in nb
sum(x^2)/n-p

lrtest(p4,p6)


## fitted vs predicted 
a1<- predict(p4)
hist(a1)
b1<- zinb$count
plot(a1,b1)
plot(seq(0,30,length=length(a1)),a1)

plot(p4$residuals, p4$fitted.values)

#exponstiated parameter estimate
exp(p4$coefficients$count) # people who brought camper got more fish
exp(p4$coefficients$zero)
  
  
  
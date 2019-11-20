library(readr)
data <- read.csv(choose.files())
View(data)
##data preprocessing##
introduce(data)
##There is one discrete columns which should be converted into the continous##
data$State=factor(data$State,
                  levels =c('New York','California','Florida'), labels =c(1,2,3))
View(data)
## First business model##
summary(data)
   # R.d Spend mean=73722,median=73051,iqr=61667
   # Administration mean=121345,median=122700,iqr=41111
   # Marketing spend mean=211025,median=212716,iqr=170169
   # profit mean=112013,median=107978,iqr=49627
##second business model##
attach(data)
var(R.D.Spend)##2107017150  The data is very much spread in this variable because the variance is greater ##
var(Administration)##784997271 The data is very much spread in this variable because the variance is greater ##
var(Marketing.Spend)##14954920097 The data is very much spread in this variable because the variance is greater ##
var(Profit)## 1624588173  The data is very much spread in this variable because the variance is greater  ##
sd(R.D.Spend)## 45902.26 The data is very much spread in this variable because the Standard deviations is greater  ##
sd(Administration)##28017.8 The data is very much spread in this variable because the Standard deviations is greater##
sd(Marketing.Spend)##122290.The data is very much spread in this variable because the Standard deviations is greater3##
sd(Profit)##40306.18the data is very much spread in this variable because the Standard deviations is greater##
##Third business model##
skewness(R.D.Spend)##0.1590405 the positive Skewness shows it has longer right tail##
plot_histogram(R.D.Spend)## There is much frequency from 50000-100000 from the 100000 its decreasing##
skewness(Administration)##  -0.4742301 the negative skewness shows it has longer left tail##
plot_histogram(Administration)## The Administration costs of startups lies more in interval 120000-160000##
skewness(Marketing.Spend)##  -0.04506632the negative skewness shows it has longer left tail##
plot_histogram(Marketing.Spend)## The Marketing spend is less skewed and most frequency is in the middle##
skewness(Profit)## 0.02258638the positive Skewness shows it has longer right tail##
plot_histogram(Profit)##Most of the profit frequency lies from inbetween interval 100000-150000##
##fourth business model##
kurtosis(R.D.Spend)##2.19 the positive Kurtosis shows it has higher peak and thick heavy tails is normally distributed##
kurtosis(Marketing.Spend)##2.27 the positive Kurtosis shows it has higher peak and thick heavy tails is normally distributed####0.1590405 the positive Skewness shows it has longer right tail##
kurtosis(Administration)##3.08 the positive Kurtosis shows it has higher peak andthick heavy tails is normally distributed## 
kurtosis(Profit)##2.82 the positive Kurtosis shows it has higher peak and thick heavy tails is normally distributed## 
##fifth business model##
plot(R.D.Spend,Profit)##This plot shows a linear correlation between two variable##
plot(Marketing.Spend,Profit)##This plot shows a  moderate linear correlation between two variable##
plot(Administration,Profit)##This plot shows a   weak linear correlation between two variable##
pairs(data)
plot_correlation(data)
## we should check for muti colinarity with vif function##
##bulding the model##
model <- lm(Profit~ .,data=data)
summary(model)
## from the model we can say that the data state is insignificant##
model1 <- lm(Profit~R.D.Spend+Marketing.Spend+Administration)
summary(model1)
## from the model1 we can say that administration is insignificant##
model2 <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(model2)
##from the model2 we can say that marketing variable p_value near to the cofidence interval we can keep are we can remove it##
model3 <- lm(Profit~R.D.Spend)
summary(model3)
## we should predict from this model3##
pred <- predict(model3)
plot(model3)
plot_qq(model3)
plot_scatterplot(model3)

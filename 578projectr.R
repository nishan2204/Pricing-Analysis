library(ggplot2)
library(readr)
hsprice <- read.csv("C:/Users/nisha/OneDrive/Documents/kc_house_data.csv")
dim(hsprice)
class(hsprice)
hsprice$date <- as.Date(as.Date(as.character(hsprice$date),"%Y%m%d"))  #converting date into YYYY-MM-DD
hsprice$age <- as.numeric(format(hsprice$date, "%Y")) - hsprice$yr_built    #Creating Age=Date-Year Built
hsprice$yr_renovated[hsprice$yr_renovated == 0] <- NA   #Classifying all houses not renovated as NA under this column
hsprice$renage <- as.numeric(format(hsprice$date, "%Y")) - hsprice$yr_renovated  #Calculatin Age of house since renovation
table(is.na(hsprice$renage))   # only approax 5% houses are renovated)
table(hsprice$waterfront)      # less than 0.5% have waterfront
table(hsprice$view)            # approax 10% has other than zero views 1,2,3,4
table(hsprice$bedrooms)        # mostly bedrooms are between 1-6
table(hsprice$bathrooms)      # mostly accounts for 1,1.5,1.75,2,2.25,2.5,2.5,3,3.5 total 30
table(hsprice$condition)      # mostly 3 then 4 then 5 then 2 then 1
table(hsprice$grade)          # mostly 5-9 out of 1-12
table(hsprice$floors)         # mostly 1 and 2 then 1.5 
table(hsprice$zipcode)
hsprice$rate <- hsprice$price/hsprice$sqft_living #calculating rate
str(hsprice)
summary(hsprice)
summary(hsprice$price)
summary(hsprice$sqft_living)
Mean<-tapply(hsprice$price, hsprice$zipcode,mean)
dt<-data.frame(Mean)                      #created new dataframe dt displaying mean of prices for each zipcode
write.csv(hsprice,"hsprice.csv")
install.packages("lattice")
library(lattice)
bwplot(bedrooms~price, data = hsprice)
bwplot(sqft_living~bedrooms, data = hsprice)
bwplot(~bedrooms, data = hsprice) 
bwplot(~price, data = hsprice) 
bwplot(~sqft_living, data = hsprice) 
densityplot(hsprice$price)
hist(hsprice$price, breaks = c(0,500000,1000000,1500000,2000000,2500000,3000000,3500000,4000000,4500000,5000000,5500000,6000000,6500000,7000000,7500000,8000000), xlab="Price ($)", main="Frequency of Price")
ggplot(hsprice, aes(hsprice$price)) + stat_bin(bins = 100, colour="black", fill="green") + labs(x= "Price",y= "Frequency" , title = "Histogram of Price") + xlim(0,4000000) + scale_fill_discrete()
ggplot(hsprice, aes(hsprice$price)) + stat_bin(bins = 100, colour="black", fill="green") + labs(x= "Price",y= "Frequency" , title = "Histogram of Price") + xlim(4000000,8000000) + scale_fill_discrete()
qplot(hsprice$view, main = "Plot of House View" , xlab = "View" , ylab = "Frequency")
qplot(hsprice$bathrooms)
install.packages("car")
library(car)
scatterplot(x =hsprice$sqft_living, y=hsprice$bathrooms) 
scatterplot(x =hsprice$price, y=hsprice$sqft_living)
scatterplot(x =hsprice$price, y=hsprice$grade)           
scatterplot(x =hsprice$price, y=hsprice$zipcode)
ggplot(data = hsprice, mapping = aes(x = sqft_living, y = price)) + geom_point(colour = 'skyblue') + geom_smooth(method = 'lm')
hspricecor <- hsprice[ ,c(3:10,12:14,17,22)]   #including only relevant variables in the correlation matrix
cor(hspricecor)
install.packages("corrplot")
library(corrplot)
corrplot(cor(hspricecor))
install.packages("corrgram")
library(corrgram)
corrgram(hspricecor, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)
cor.test(hsprice$price,hsprice$sqft_living)
t.test(hsprice$price,hsprice$sqft_living)
t.test(hsprice$price,hsprice$bathrooms)
t.test(hsprice$price,hsprice$grade)
t.test(hsprice$price,hsprice$bedrooms)
hsprice$bedrooms <- as.factor(hsprice$bedrooms)
hsprice$bathrooms <- as.factor(hsprice$bathrooms)
newhsprice2$grade_factor <- as.factor(newhsprice2$grade)    #creating a categorical variable grade_factor from grade
hsprice$grade_factor <- as.factor(hsprice$zipcode)
str(hsprice)
model1 <- lm(price~ sqft_living + bedrooms + bathrooms + grade_factor + sqft_above,data = hsprice)
vif(model1)
summary(model1)
vif(model1)
plot(model1)
model2 <- lm(price~ sqft_living + bedrooms + bathrooms + grade + sqft_above + zipcodefactor,data = hsprice)
summary(model2)
str(hsprice)
plot(cooks.distance(model2))
cooks.distance(model2)
myCDs <- cooks.distance(model2)
plot(model2)
round(myCDs,7)
myCDS <- round(myCDs,7)
myCDS
sort(myCDS)
myCDS2=sort(myCDS)
myCDS2
round(myCDS,7)
library(car)
outliertest(model1)
qqplot(model1)
sort(myCDS)
install
avplots(model1)
rstudent(model1)
count.fields(newhsprice2$logcooksd>(4/nrow(newhsprice2)))
newhsprice2 <- hsprice[-c(12778), ]
summary(model3)
str(hsprice)
plot(model2)
model3 <- lm(log(price)~ sqft_living + bedrooms + bathrooms+ grade_factor + waterfront+ view_factor+sqft_living:bedrooms:bathrooms+sqft_living_Sq,data = newhsprice2)
summary(model3)
plot(model3)
vif(model3)
newhsprice2$sqft_living_Sq <- newhsprice2$sqft_living*newhsprice2$sqft_living  #creating quadratic term for sqft_living
newhsprice2$view_factor <- as.factor(newhsprice2$view)  #creating categorical variable for view
plot(model3)
hsprice$logcooksd <- cooks.distance(model2)
outlierTest(model4)
newhsprice2$logrstudent<-rstudent(model2)
newhsprice2$logcooksd <- cooks.distance(model2)
  


#Covid-19 Research Using Polynomial Regression in R

#Covid 19 Research using Polynomial Regression(Recovered Patients)

#Importing the dataset
dataset=read.csv('Covid_Cases.csv')
dataset=dataset[-c(2,4)]

#Fitting Polynomial Regressor To The Dataset
dataset$Days2=dataset$Days^2
dataset$Days3=dataset$Days^3
dataset$Days4=dataset$Days^4
poly_reg=lm(formula=Recovered~.,data=dataset)
summary(poly_reg)

#Visualising the Polynomial Regressor
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Days ,y=dataset$Recovered),
             colour='red')+
  geom_line(aes(x=dataset$Days,y=predict(poly_reg,newdata = dataset)),
            colour='blue')+
  ggtitle('Recovered Patients Prediction')+
  xlab('Days')+
  ylab('Recovered')


#Predicting number of recovered patients over a span of 13 days
y = 391:420
for (i in y) {
  y_pred1=predict(poly_reg,data.frame(Days=y,
                                      Days2=y^2,
                                      Days3=y^3,
                                      Days4=y^4))
}
y_pred1 = round(y_pred1)

#Covid 19 Research using Polynomial Regression(Deceased Patients)

#Importing the dataset
dataset=read.csv('Covid_Cases.csv')
dataset=dataset[-c(2,3)]

#Fitting Polynomial Regressor To The Dataset
dataset$Days2=dataset$Days^2
dataset$Days3=dataset$Days^3
dataset$Days4=dataset$Days^4
poly_reg=lm(formula=Deceased~.,data=dataset)
summary(poly_reg)

#Visualising the Polynomial Regressor
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Days ,y=dataset$Deceased),
             colour='red')+
  geom_line(aes(x=dataset$Days,y=predict(poly_reg,newdata = dataset)),
            colour='green')+
  ggtitle('Deceased Patients Prediction')+
  xlab('Days')+
  ylab('Deceased')

#Predicting number of deceased patients over a span of 13 days
y = 391:420
for (i in y) {
  y_pred2=predict(poly_reg,data.frame(Days=y,
                                      Days2=y^2,
                                      Days3=y^3,
                                      Days4=y^4))
}
y_pred2 = round(y_pred2)


#Covid 19 Research using Polynomial Regression(New Cases)

#Importing the dataset
dataset=read.csv('Covid_Cases.csv')
dataset=dataset[-c(3,4)]

#Fitting Polynomial Regressor To The Dataset
dataset$Days2=dataset$Days^2
dataset$Days3=dataset$Days^3
dataset$Days4=dataset$Days^4
dataset$Days5=dataset$Days^5
dataset$Days6=dataset$Days^6
dataset$Days7=dataset$Days^7
poly_reg=lm(formula=Cases.Found~.,data=dataset)
summary(poly_reg)

#Visualising the Polynomial Regressor
library(ggplot2)
ggplot()+
  geom_point(aes(x=dataset$Days ,y=dataset$Cases.Found),
             colour='red')+
  geom_line(aes(x=dataset$Days,y=predict(poly_reg,newdata = dataset)),
            colour='black')+
  ggtitle('New Patients Prediction')+
  xlab('Days')+
  ylab('New Cases')


#Predicting number of new patients over a span of 13 days
y = 391:420
for (i in y) {
  y_pred3=predict(poly_reg,data.frame(Days=y,
                                      Days2=y^2,
                                      Days3=y^3,
                                      Days4=y^4,
                                      Days5=y^5,
                                      Days6=y^6,
                                      Days7=y^7))
}
y_pred3 = round(y_pred3)

pred_table = data.frame(y_pred1,y_pred2,y_pred3)
pred_table
write.csv(pred_table,"covid_pred.csv")

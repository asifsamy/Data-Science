#----------------------------------------------------------
# Reset R's brain
#----------------------------------------------------------
rm(list=ls())

#----------------------------------------------------------
# Reset graphic device
# As long as there is any dev open (exept "null device") 
# close the active one!
# Caution: closes all open plots!!!!
#----------------------------------------------------------
while(!is.null(dev.list()))
{
  dev.off()
}


require(ISLR)
library(ISLR)
attach(Wage)
assessment_dataframe <- Wage[sample(nrow(Wage), 3000), ]
View(assessment_dataframe)

#----------------------------------------------------------
#Extraction of year, age, education and wage from the entire dataset
#----------------------------------------------------------
assessment_dataframe <- assessment_dataframe[,c(1, 2, 5, 11)]
summary(assessment_dataframe)
View(assessment_dataframe)


#----------------------------------------------------------
#A short description of each variable (attribute) from that extracted dataset
#----------------------------------------------------------
library(knitr)  # kable
Variable = c("Year", "Age", "Education", "Wage")
Description = c("Year that wage information was recorded",
                "Age of worker",
                "Education level of worker",
                "Workers raw wage")
description_assesment_dataframe = data.frame(Variable, Description)
kable(description_assesment_dataframe, format='markdown')


#----------------------------------------------------------
#Visualizations using plots
#----------------------------------------------------------


#-----------------------------------
#Visualizing Numeric/Continuous variables(year, age and wage) in the extracted Dataset
#-----------------------------------
library(ggplot2) # Plotting
library(purrr) # Organizing
library(tidyr) # Organize/tidy data

assessment_dataframe %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value, fill=key)) +
  facet_wrap(~ key, scales="free") +
  geom_histogram( bins=sqrt( nrow(OJ) ) ) +
  theme(legend.position="none")

#-----------------------------------
#Visualizing Categorical variable(education) in the extracted Dataset
#-----------------------------------
ggplot(data = assessment_dataframe, aes(education,
                                        fill = education)) +
  geom_bar(mapping = aes(x = education))


#-----------------------------------
#Visualizing realtionship between variables(wage and others 3 variables) using pairs plot in the extracted Dataset
#-----------------------------------
library(GGally) # ggpairs plot
library(reshape) # Melt data for plotting
pairs_plot = ggpairs(assessment_dataframe[, ], 
            aes(alpha=0.6),
            upper = list(continuous = wrap("cor", size = 4)),
            diag = list(continuous = "barDiag"),
            lower = list(continuous = "smooth"))
suppressMessages(print(pairs_plot))

#-----------------------------------
#Visualizing realtionship between wage and categorical variables(education) using box plot in the extracted Dataset
#-----------------------------------
ggplot(data = assessment_dataframe, mapping = aes(x = education, y = wage, color=education)) +
  geom_boxplot()


#----------------------------------------------------------
#Obtaining suitable Statistics and Prediction using some methods or models
#----------------------------------------------------------


#-----------------------------------
#GAMs (Generalized Addidtive Models) 
#Analysis of variance (ANOVA, using an F-test)
#Fit five different models and sequentially compare the simpler model to the more complex model
# make year a smooth spline with 3 DoF to help show GAM can combine multiple non-linear models
#-----------------------------------
library(splines) # Splines
library (gam)    # GAM
fit.1= lm(wage~age + s(year,3) + education, data=assessment_dataframe)
fit.2= lm(wage~poly(age,2) + s(year,3) + education, data=assessment_dataframe)
fit.3= lm(wage~poly(age,3) + s(year,3) + education, data=assessment_dataframe)
fit.4= lm(wage~poly(age,4) + s(year,3) + education, data=assessment_dataframe)
fit.5= lm(wage~poly(age,5) + s(year,3) + education, data=assessment_dataframe)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

#-----------------------------------
# split Wage in half training and testing
#-----------------------------------
set.seed(8)
train=sample(1:nrow(assessment_dataframe), 0.5*nrow(assessment_dataframe))
assessment_dataframeDf.test = assessment_dataframe[-train,]
assessment_dataframeVar.test= assessment_dataframe$wage[-train]

#-----------------------------------
# Determining the best degree to uge the polynomial function of age via CV(Cross-Validation)
#-----------------------------------
library(caret) # Showing Confusion Matrix Data
library(boot)    # cv.glm

# Create folds for each observation in training data
set.seed(8)
folds = createFolds(assessment_dataframe$wage[train], k=10) # 10 folds is default

# Set up a matrix which will have 1 row for every 
# CV iteration and 1 column for each polynomial degree.
polynomialDF = 8
Errors_CV = matrix(nrow=10, ncol=polynomialDF)

# Loop over degrees of polynomial 
for(poly_degree in 1:polynomialDF){
  # Loop over folds of cv
  for(k in 1:10){
    fit = gam(wage~poly(age, poly_degree) + s(year,3) + 
                education, data=assessment_dataframe, subset = -folds[[k]])
    predictions = predict(fit,newdata=assessment_dataframe[folds[[k]],])
    Errors_CV[k,poly_degree]=mean((predictions-assessment_dataframe[folds[[k]],c("wage")])^2)
  }   
}

# Find which degree has lowest average MSE over all k folds:
mean_cv_errors = apply(Errors_CV, 2, mean)
plot(mean_cv_errors, type = 'b', xlab = "Polynomial Degree", ylab = "Squared Error")

best_poly_degree = which.min(mean_cv_errors)

#-----------------------------------
# GAM for the polynomial function of age with a degree of 7
#-----------------------------------
par(mfrow = c(2, 3))
### Smoothing spline
fit=gam(wage~ poly(age, 7) + s(year, 3) + education, data = assessment_dataframe, subset = train)
plot(fit, se=TRUE, col ="blue")


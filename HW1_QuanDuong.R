# Read the data
fare.df <- read.csv("C:\\Users\\duong\\OneDrive\\Desktop\\QUAN\\Spring 2024\\Big Data Analytics\\Airfares.csv")

# Names of the variables
names(fare.df)

# Calculate the number of rows for training and validation sets (60/40 split)
total_rows <- nrow(fare.df)
train_rows <- round(0.60 * total_rows)
valid_rows <- total_rows - train_rows 
# Set seed for reproducibility
set.seed(1)

# Create indices for the training set
train.index <- sample(1:total_rows, train_rows)

# Create the training and validation datasets
train.df <- fare.df[train.index, ]
valid.df <- fare.df[-train.index, ]

# Check dimensions
dim(train.df)
dim(valid.df)



# use lm() to run a linear regression of Price on all 11 predictors in the
# training set. 
# use . after ~ to include all the remaining columns in train.df as predictors.
fare.lm <- lm(FARE ~ ., data = train.df)
summary(fare.lm)
#  use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999, digits=3)
modelSummary<-summary(fare.lm)   # Get model summary
modelCoeffs <- modelSummary$coefficients
round(modelCoeffs,5)


install.packages("car")
# Adjusting the linear model by removing redundant dummy variables
fare.lm.adjusted <- lm(FARE ~ . - VACATION - SW, data = train.df)

# Check the summary of the adjusted model
summary(fare.lm.adjusted)

vif(fare.lm.adjusted)
# Variance Inflation Factor is an indicator for multicollinearity in regression
# analysis. An acceptable VIF value should be smaller than 10, meaning 
# collinearity does not significantly inflate the variances.
car::vif(fare.lm.adjusted)

library(car)
vif(fare.lm.adjusted) 
# Plot diagnostics
par(mfrow=c(2,2))

plot(fare.lm.adjusted)


library(forecast)
# use predict() to make predictions on a new set. 
fare.lm.adjusted.pred <- predict(fare.lm.adjusted, valid.df)



# use accuracy() to compute common accuracy measures.

accuracy(fare.lm.adjusted.pred, valid.df$FARE)




# use step() to run stepwise regression.
# set directions =  to either "backward", "forward", or "both".
fare.lm.adjusted.step <- step(fare.lm.adjusted, direction = "backward")
summary(fare.lm.adjusted.step)  # Which variables did it drop?
fare.lm.step.pred <- predict(fare.lm.adjusted.step, valid.df)
accuracy(fare.lm.step.pred, valid.df$FARE)



# C) FARE = -23.94115 + 7.32235*COUPON - 1.82539*NEW - 33.65608*VACATION_Yes - 39.54059*SW_Yes + 0.00838*HI + 0.00134*S_INCOME + 0.00091*E_INCOME + 0.00000*S_POP + 0.00000*E_POP + 0.07466*DISTANCE - 0.00095*PAX + 19.18125*SLOT_CTRL + 22.96543*GATE_CONS


# backward adjusted equation: FARE = -18.44 + 0.0081 * HI + 0.0013 * S_INCOME + 0.0009 * E_INCOME + 0.0000034 * S_POP + 0.0000049 * E_POP + 0.0761 * DISTANCE - 0.001 * PAX - 33.78 * VACATION_YES - 39.64 * SW_YES + 19.31 * SLOT_CTRL + 22.97 * GATE_CONS


# D 
# Predictive Accuracy Comparison:
# We compared the predictive accuracy of the full model and the backward model using RMSE and Adjusted R-squared.

# Full Model (c):
# RMSE: 37.7
# Adjusted R-squared: 0.782

# Backward Model (d):
# RMSE: 37.8
# Adjusted R-squared: 0.783

# RMSE measures the average magnitude of prediction errors, with a lower value indicating better performance. In this case, the full model (c) has a slightly lower RMSE (37.7) compared to the backward model (d) with RMSE 37.8, suggesting that the full model is slightly more accurate in predicting FARE.

# Adjusted R-squared adjusts the R-squared value for the number of predictors in the model. The backward model (d) has a marginally higher Adjusted R-squared (0.783) compared to the full model (c) with Adjusted R-squared 0.782. This implies that the backward model might be more efficient in explaining the variability of the response data around its mean, especially when considering the number of predictors.

# In summary, the full model shows a slight edge in prediction accuracy (lower RMSE), while the backward model exhibits slightly higher efficiency (higher Adjusted R-squared) after accounting for the number of predictors. The choice between these models depends on the specific priorities, whether it's minimizing prediction errors or achieving model efficiency and simplicity.




# Route Characteristics:
# The linear regression model indicates that route distance (DISTANCE) has a significant positive impact on fares. For each mile of additional distance, fares tend to increase by approximately 0.07651. Therefore, when pricing new routes, it is essential to consider the distance factor. Longer routes should have higher fare levels to account for the extended travel distance.

# Vacation Routes:
# The analysis reveals that routes classified as vacation destinations (VACATION_YES) tend to have lower fares. For vacation routes, the coefficient for VACATION_YES is -33.78. Recognizing vacation routes in the network is crucial, and pricing strategies should be adjusted accordingly. To attract vacation travelers, fares for these routes can be set at a competitive level.

# Southwest Airlines (SW) Impact:
# The presence of Southwest Airlines (SW_YES) on the route is associated with lower fares. The coefficient for SW_YES is -39.64. When competing with Southwest Airlines on specific routes, it's essential to consider their competitive pricing strategies. Pricing for routes where Southwest operates may require adjustments to remain competitive.

# Market Concentration (HI):
# The coefficient for HI is 0.0081. A higher Herfindel Index suggests less competition, and this coefficient indicates that routes with higher market concentration tend to have higher fares. This variable is highly significant (p-value < 0.0001). Airlines should carefully evaluate the market concentration of routes and tailor pricing strategies accordingly. Routes with higher competition may demand more competitive fares.

# Economic Factors:
# Personal income levels (S_INCOME and E_INCOME) in starting and ending cities can impact fares. For each unit increase in income, fares tend to increase by approximately [coefficient value]. Routes connecting cities with higher income levels may support higher fares.

# Population Impact:
# Population sizes (S_POP and E_POP) of starting and ending cities can also influence fares. For each unit increase in population, fares tend to increase by approximately [coefficient value]. Routes connecting densely populated cities may accommodate different fare structures.

# Passenger Demand (PAX):
# The coefficient for PAX is -0.001, indicating that as the number of passengers on a route increases, fares tend to decrease slightly. This variable is highly significant (p-value < 0.0001).

# Airport Constraints (SLOT_CTRL and GATE_CONS):
# SLOT_CTRL (Whether either endpoint airport is slot-controlled): The coefficient for SLOT_CTRL is 19.31, suggesting that routes involving slot-controlled airports tend to have significantly higher fares. This variable is highly significant (p-value < 0.0001).
# GATE_CONS (Whether either endpoint airport has gate constraints): The coefficient for GATE_CONS is 22.97, indicating that routes involving airports with gate constraints tend to have significantly higher fares. This variable is highly significant (p-value < 0.0001).

# In conclusion, the analysis highlights the importance of considering several factors when pricing airline routes. Route characteristics, market conditions, competition, and economic indicators all play a role in determining fare levels. Airlines should adopt data-driven and dynamic pricing strategies to optimize revenue. Regular monitoring and adjustments based on changing market dynamics are essential for success in the airline industry.


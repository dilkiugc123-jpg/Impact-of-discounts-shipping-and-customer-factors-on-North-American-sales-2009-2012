Data set for analysis
https://docs.google.com/spreadsheets/d/1wtsLZceAqFpb_sbV9RWlhbXECnOZaSXV0fJonEAAVgU/edit?gid=0#gid=0
library(ggplot2)
library(dplyr)
library(readxl)
#Check linearity,normality
library(lmtest)
library(car)
data <- final_dataset
data
View(data)
attach(data)
# Check for missing values in the entire data set
missing_data <- sum(is.na(data))
print(paste("Total missing data:", missing_data))


# Check null values in the data set
null_data <- sum(is.null(data))
print(paste("Total null data:", null_data))
#Draw boxplot
boxplot(data$SALES,col = "skyblue",boxlty = 0,whisklty = 3,whisklwd = 1.5,staplelwd = 1.5,main = "Boxplot of SALES", xlab = "SALES")




#Draw a Histogram
hist(data$SALES,col = "skyblue",ylim = c(0,8000),breaks = 20,main = "Histogram of SALES",xlab = "SALES",ylab = "Frequency")
#Assigning numerical values for non-numerical values
ship_mode_num <- c("Delivery Truck"=1,"Express Air"=2,"Regular Air"=3)
ship_mode_numeric <- ship_mode_num[data$SHIPMODE]
customer_segment_num <- c("Consumer"=1,"Corporate"=2,"Home Office"=3,"Small Business"=4)
customer_segment_numeric <- customer_segment_num[data$CUSTOMERSEGMENT]
product_catergory_num <- c("Furniture"=1,"Office Supplies"=2,"Technology"=3)
product_category_numeric <- product_catergory_num[data$PRODUCTCATEGORY]
# Get full model
y=log(data$SALES)
boxplot(y, main="Boxplot of log(Sales)", ylab="log(Sales)")
model2 <- lm(y~UNITPRICE+DISCOUNT+ship_mode_numeric+product_category_numeric+SHIPPINGCOSTS+
               customer_segment_numeric+SHIPPINGDURATION,data = data)
model2
summary(model2)
anova(model2)
data1<- data.frame("LOGSALES"=y,"UNITPRICE"=data$UNIT.PRICE,"DISCOUNT"=data$DISCOUNT, "SHIPPINGCOSTS"=data$SHIPPING.COSTS,"PRODUCTCATEGORY"=product_category_numeric, "CUSTOMER SEGMMENT"=customer_segment_numeric,"SHIP MODE"=ship_mode_numeric,"SHIPPING DURATION"=data$SHIPPINGDURATION)
View(data1)
corr2 <- cor(data1)
corr2
# Descriptive statistics
summary(data)
# Draw Correlogram
library(corrplot)
corrplot(corr2,method="color",type="lower",order="hclust", addCoef.col = "black",
     	tl.col = "black",main="Correlogram", tl.srt = 90,
     	col = colorRampPalette(c("red", "white", "blue"))(200),cl.lim = c(-1, 1))
# Drow scatterplots
plot(data$DISCOUNT,y, main = "Scatterplot of log(Sales) vs Sales Promotion", xlab = "Sales Promotion", ylab = "log(Sales)")
points(data$DISCOUNT, y, col = "blue", pch = 10)
abline(lm(y~data$DISCOUNT), col = "red",lwd=4) 
 
plot(data$UNITPRICE,y,  main = "Scatterplot of log(Sales) vs Unit Price", xlab = "Unit Price", ylab = "log(Sales)")
points(data$UNITPRICE,y,  col = "blue", pch = 10)
abline(lm(y~data$UNITPRICE), col = "red",lwd=4) 
 
plot(data$SHIPPINGCOSTS,y,  main = "Scatterplot of log(Sales) vs Shipping Cost", xlab = "Shipping Cost", ylab = "log(Sales)")
points(data$SHIPPINGCOSTS, y, col = "blue", pch = 10)
abline(lm(y~data$SHIPPINGCOSTS), col = "red",lwd=4) 
 
 plot(data$SHIPPINGDURATION,y,  main = "Scatterplot of log(Sales) vs Shipping Duration", xlab = "Shipping Duration", ylab = "log(Sales)")
points(data$SHIPPINGDURATION,y,  col = "blue", pch = 10)
abline(lm(y~data$SHIPPINGDURATION), col = "red",lwd=4) 
 
plot(cucustomer_segment_numeric,y,main = "Scatterplot of log(Sales) vs Customer Segment", xlab = "Product Categories", ylab = "log(Sales)")
points(customer_segment_numeric,y,  col = "blue", pch = 10)
abline(lm(y~customer_segment_numeric), col = "red",lwd=4)
legend("topright", legend =c("1=Consumer","2=Corporate","3=Home Office","4=Small Business"))
 
plot(product_category_numeric,y,  main = "Scatterplot of log(Sales) vs Product Categories", xlab = "Product Categories", ylab = "log(Sales)")
points(product_category_numeric,y,  col = "blue", pch = 10)
abline(lm(y~product_category_numeric), col = "red",lwd=4)
legend("topright",legend = c("1=Furniture","2=Office Supplies","3=Technology"))
 
plot(ship_mode_numeric,y,  main = "Scatterplot of log(Sales) vs Shipping Mode", xlab = "Shipping Mode", ylab = "log(Sales)")
points(ship_mode_numeric,y,  col = "blue", pch = 10)
abline(lm(y~ship_mode_numeric), col = "red",lwd=4)
legend("topright",legend = c("1=Delivery Truck","2=Express Air","3=Regular Air"))
 
#Draw a Histogram
hist(y,col = "skyblue",ylim = c(0,1500),breaks = 20,main = "Histogram of log(Sales)",xlab = "log(Sales)",ylab = "Frequency")
hist(data$DISCOUNT,col = "skyblue",ylim = c(0,2000),breaks = 20,xlab = "SALES PROMOTION",ylab = "Frequency",main = "Histrogram of SALES PROMOTION")
hist(data$UNITPRICE,col = "skyblue",ylim = c(0,9000),breaks = 30,main = "Histrogram of UNIT PRICE",xlab = "UNIT PRICE",ylab = "Frequency")
hist(data$SHIPPINGCOSTS,col = "skyblue",breaks = 30,ylim = c(0,3500),main = "Histrogram of SHIPPING COST",xlab = "SHIPPING COST",ylab = "Frequency")
hist(data$`No  of days`,col = "skyblue",breaks = 40,ylim = c(0,8000),main = "Histrogram of SHIPPING DURATION",xlab = "SHIPPING DURATION",ylab = "Frequency")
# Extract residuals and fitted values and Normal Q-Q plot from the model
residuals <- residuals(model2)
fitted_values <- fitted(model2)
# Normal Q-Q plot
qq_plot <- ggplot(residuals_df, aes(sample = residuals)) +
  geom_qq(col="blue") +
  geom_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot",
   	x = "Theoretical Quantiles",
   	y = "Sample Quantiles") +
  theme_minimal()
print(qq_plot)
# Create the Residuals vs Fitted plot
residuals_fitted_plot <- ggplot(residuals_fitted_df, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals vs Fitted Values Plot",
   	x = "Fitted Values",
   	y = "Residuals") +
  theme_minimal()
print(residuals_fitted_plot)
# Convert the data frame to a matrix
data_matrix <- as.matrix(data2)
rownames(data_matrix) <- paste("Sample", 1:8399, sep="_")
# Homoscedasticity
bp_test <- bptest(model2)
print(bp_test)
 
plot(model2$fitted.values, rstandard(model2),
 	main = "Standardized Residuals vs Fitted",
 	xlab = "Fitted values",
 	ylab = "Standardized Residuals")
abline(h = 0, col = "red")
# Fit the full model
full_model <- model2
#backward elimination
best_model_F <- step(full_model, direction="backward", scope=formula(full_model), alpha=0.05)
summary(best_model_F)
anova(best_model_F)
# F table value
f_table_value<- qf(0.95,2,8391)
f_table_value

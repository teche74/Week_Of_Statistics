---------------
DAY2 - STATISTICS
----------

- Understanding Measure of Central Tendeny.
- Understanding measure of Dispersion.
- Percentiles and Quartiles.
- 5 Point Summary / 5 Number Summary.

 
### Topic 1 - Measure of Central Tendency.

- We already covered it in day 1, Now we understand how it will be affected .

`Experiment 1` : We consider a dataset and find its central Tendency. Then we add some outliers to it and again find central tendency of data, With results with draw some conclusions.

```R
# Creating custom data.
data <- c(1,2,2,3,3,3,4,4,4,4,5,5,5);

# computing central tendencies.
cat("Before Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", max(table(data)),"\n\n");


# We add a single outlier to data

data <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,100);

# Computing central tendency
cat("After adding Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", max(table(data)),"\n");
```

**Result :**
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/af3611af-f4c3-4148-8861-3503e843b53d)
-  We can see that mean has an adverse impact of outliers , This mean you can easily identify outliers using mean.
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/81661465-06d2-4fbe-b891-e5b065783ae7">
</center>

> Note that the median🔴 and mode🔴 will have no effect but the mean🟢 got shifted with just an single outlier.

**Daigram Code**
```R
data_with_outliers <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,100)


data_without_outliers <- c(1,2,2,3,3,3,4,4,4,4,5,5,5)


mean_with_outliers <- mean(data_with_outliers)
median_with_outliers <- median(data_with_outliers)
mode_with_outliers <- as.numeric(names(table(data_with_outliers)))[which.max(table(data_with_outliers))]


mean_without_outliers <- mean(data_without_outliers)
median_without_outliers <- median(data_without_outliers)
mode_without_outliers <- as.numeric(names(table(data_without_outliers)))[which.max(table(data_without_outliers))]


par(mfrow = c(1, 2))  


hist(data_with_outliers, main = "With Outliers", col = "lightblue", border = "black")


abline(v = mean_with_outliers, col = "red", lty = 2, lwd = 2)
abline(v = median_with_outliers, col = "blue", lty = 2, lwd = 2)
abline(v = mode_with_outliers, col = "green", lty = 2, lwd = 2)

hist(data_without_outliers, main = "Without Outliers", col = "lightgreen", border = "black", )

abline(v = mean_without_outliers, col = "red", lty = 2, lwd = 2)
abline(v = median_without_outliers, col = "blue", lty = 2, lwd = 2)
abline(v = mode_without_outliers, col = "green", lty = 2, lwd = 2)


legend("topright", legend = c("With Outliers", "Without Outliers"), fill = c("lightblue", "lightgreen"))
```

`Experiment 2 : ` What if there are multiple outliers in the data.💭😲

```R
# Data with single outlier
data <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,100);

# computing central tendencies.
cat("Before Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", max(table(data)),"\n\n");


# We add multiple outliers to data

data <- c(1,2,2,3,3,3,4,4,4,4,5,5,5,100,100,121);

# Computing central tendency
cat("After adding Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", max(table(data)),"\n");
```
**Result :**
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/a881802f-db99-4171-b4ba-9c798f78fb1a)

> This time the median🔴 and mode🔴 will again have no effect but the mean🟢 got shifted more. This means while finding data , best is to use mean, but while dealing with missing value use mean .

#### Problem with mode

- Even the data have less valued outliers, it still got shifted.

```R
data <- c(1,2,3,4,5,5,6,7,8,9);

# computing central tendencies.
cat("Before Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", as.numeric(names(table(data)))[which.max(table(data))],"\n\n");


# We add multiple outliers to data

data <- c(1,2,3,4,5,5,6,7,8,9,15,15,15);

# Computing central tendency
cat("After adding Outlier : \n")
cat("Mean : ", mean(data),"\n");
cat("Median : ", median(data),"\n");
cat("Mode : ", as.numeric(names(table(data)))[which.max(table(data))],"\n");
```  

**Result**
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/ee8020e8-2ef1-4dc8-a98a-2f4435e076e2)
- Mode get affected adversely. Best Example to analyze that median is the only member which gets less affected to outliers.


###  Topic 2  Measure of Dispersion

- Dispersion means how well our data is spread ? Right, lets check ...

```R
# In R we can simply find standard deviation using sd()
data <- c(12,2,3,4,6,7,2,5);
# print
cat("Standard Deviation:", sd(data),  "\n");
``` 

Let me show u something intresting why we use spread (Dispersion)
 
```R
 data <- c(1,2,3,4,5,6,7,8,9);
data
cat("Mean : ", mean(data),"\n");

data <- c(5,5,5,5,5,5,5,5,5);
data
cat("Mean : ", mean(data),"\n");
```
- Different dataset with same mean but if we see  their distributions : 
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/b27f6827-7b42-4394-bc51-afded4119ed4" >
</center>


- As we know that sd values has some meaning :

-  ==Low SD value==  signifies that high peak is present in distribution.

```R
# Generate a dataset with low standard deviation 
data_low_sd <- rnorm(1000, mean =  50, sd =  5) 

# Generate a dataset with high standard deviation 
data_high_sd <- rnorm(1000, mean =  50, sd =  20)

# Combine data
combined_data <- data.frame(value =  c(data_low_sd, data_high_sd), group =  rep(c("Low SD",  "High SD"), each =  1000))

# Plot
ggplot(combined_data, aes(x = value, fill = group))  + geom_histogram(binwidth =  2, position =  "dodge", color =  "black", alpha =  0.7)  + scale_fill_manual(values =  c("Low SD"  =  "lightblue",  "High SD"  =  "lightcoral"))  +  # Adjust colors labs(title =  "Comparison of Distributions with Different Standard Deviations", x =  "Value", y =  "Frequency")  + theme_minimal()  + theme(legend.position =  "top")
```
**Result**
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/7e341c13-5812-47a0-8018-fa553d79dd48" >
<center>

- `Low SD` signifies high peak while `High SD` signifies low peak .


##  PERCENTILES AND QUARTILES

-  These are the techniques used to identify outliers.

`Percentiles : `A percentile is a value below which a certain percentage of observation lies.

`Example : ` From the given data below, Find percentile ranking of number 10.
	 data = [ 2,2, 3,4,5,5,5,6,7,8,8,8,8,9,9,10,11,11,12]

> Percentile Ranking of 10 = ( No of values below 10/total values count) x100 = (15x100)/19 = 80 Percentile.

**This means 80 % of entire distribution is less than 10**.

> How to Find value from Given Percentile range ?? 
> Value Index = (Percentile/100) x (total_count +1).

```R
# You can directly find quantile values using quantiles(function)

data<- c(2,2,3,4,5,5,5,6,7,8,8,8,8,9,9,10,11,11,12);
quantile(data);
```
**Result**
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/b7adfb57-d303-46ff-b30c-0a33c3da4639)


## 5 POINTS SUMMARY


-  5 Points Summary consist of :
	- Minimum
	- I st Quartile (Q1)
	- Median
	- III rd Quartile (Q3)
	- Maximum

- It also helps in removing outliers using lower and higher fence.

`Example : ` From the Given data Below, we had to remove outliers.

`data :` 1,2,2,2,3,3,4,5,5,5,6,6,6,6,7,8,8,9,27 .

**Step 1 - Find IQR (Inter Quartile  Range)**

> IQR = 3rd Quartile (Q3) - 1st Quartile (Q1).

Q1 = (20 x 25)/ 100 = 4 (index pos) = 3.
Q1 = (75 x 25)/ 100 = 15 (index pos) = 7. 

IQR = 7-3  = 4 .

**Step2 - Find lower  and upper fence**

lower fence = Q1 - 1.5 x IQR = -3 .
upper fence = Q3 + 1.5 x IQR = 13.

**This means 27 is the outlier, Now we can simply do the same thing using boxplot which also use the 5 point summary for outliers detection**

```R
data <- c(1,2,2,2,3,3,4,5,5,5,6,6,6,6,7,8,8,9,27);

boxplot(data)
```

<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/cbc5f1ed-b419-4999-ad5a-12b9c0ff917a">
</center>

<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/de7fb5fe-f560-4477-9626-30ccb4ee5653" >
</center>

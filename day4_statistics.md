------------------
DAY4 - STATISTICS
------------

### OUTLIERS DETECTION

- As we had already know about normal distribution, we can conclude a statement that almost all of our **outliers will lie after 3rd standard deviation** . Lets analyze and visualize it.


#### Using 3rd Standard Deviation
```R
# import library for smoothening histogram
library("rafalib")

set.seed(1)

# Normal distribution with mean 10 and sd lies 3.
data <- rnorm(200, mean=10, sd=3) 

# View some values
head(data)

# Creating probaility density function plot
shist(data , col='coral',plotHist =TRUE)
```

<center>
<image src= "https://github.com/teche74/Week_Of_Statistics/assets/129526047/60a295ea-4a44-40ac-829e-d5984c553ce9">
</center>

- Now we add some outlier and then use 3rd standard deviation as threshold for finding outliers in data.

```R
# Adding Outliers 
cat(data, c(60,70,80));

# create function to collect outliers;
check_outliers <- function(data){
  temp <- sort(data)
  threshold <- 3
  data_mean <- mean(data)
  data_sd <- sd(data)
  outliers <- numeric()  # Initialize an empty vector to store outliers
  
  for (val in data){
    val_zscore <- (val - data_mean) / data_sd
    if (abs(val_zscore) > threshold){
      cat("Outlier: ", val, "\n")
      outliers <- c(outliers, val)  # Append the outlier to the vector
    }
  }
  
  return(outliers)
}

outliers <- check_outliers(data)
outliers
```
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/de3f6097-f146-4c41-840b-008e6451d98a">
</center>

- **This mean we are right**


#### Using IQR

- Using IQR we define upper and lower fence and value outside these bound are outliers. 

**Step 1** : Find Q1 and Q3. Then Find IQR using Q3-Q1.
**Step 2** : Calculate upper and lower fence using  IQR.
	-  `lower fence : Q1 + 1.5 * IQR`
	-  `upper fence : Q3 - 1.5 * IQR`

**Step 3** : Condition for strict bounded , values that are outside the bound are outliers.

```R

detect_outliers_iqr <- function(data) {
  # Step 1: Find Q1 and Q3
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  
  # Step 2: Calculate IQR
  iqr <- q3 - q1
  
  # Step 2: Calculate upper and lower fence
  lower_fence <- q1 - 1.5 * iqr
  upper_fence <- q3 + 1.5 * iqr
  
  # Step 3: Identify outliers
  outliers <- data[data < lower_fence | data > upper_fence]
  
  return(outliers)
}


data <- c(data, 60, 70, 80) 
outliers_iqr <- detect_outliers_iqr(data)

cat("Outliers detected using IQR method: ", outliers_iqr, "\n")
```
 <center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/c7b154b2-60b2-42e5-8db7-2096eee5a07e">
</center>

#### Using Box plot

- Predefined function that visualize 5 point summary as well as outliers (if present) 

```R
data <- rnorm(200, mean=10, sd=3) 

boxplot(data,
main = "Without Outliers",
col = "orange",
border = "brown",
horizontal = TRUE,
notch = TRUE
)



data <- c(data, 60, 70, 80)  

boxplot(data,
main = "With Outliers",
col = "orange",
border = "brown",
horizontal = TRUE,
notch = TRUE
)
```
 <center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/dd393f75-75d8-4891-bf96-8339e233dcb7">
</center>

- You had noticed that right figure have 3 outliers int it which is easily Visualized.


## PROBABILITY 

- `Probability measures the likelihood of an event occurring`. 
- It's value ranges from  0 to  1 (both floating and discrete values) , where `0 indicates impossibility`, `1 indicates certainty`, and values in between represent degrees of likelihood.

### Sample Space and Events

-   **Sample Space (S):** The set of all possible outcomes of an experiment.
-   **Event (E):** A subset of the sample space representing a particular outcome or a combination of outcomes.

## Probability Distributions

### Discrete and Continuous Distributions

-   **Discrete Distribution:** Probability distribution of a discrete random variable.
-   **Continuous Distribution:** Probability distribution of a continuous random variable.

### PMF and PDF

-   **Probability Mass Function (PMF):** Probability distribution function for discrete random variables.
-   **Probability Density Function (PDF):** Probability distribution function for continuous random variables.

### CDF

-   **Cumulative Distribution Function (CDF):** Cumulative probability of a random variable up to a certain point.



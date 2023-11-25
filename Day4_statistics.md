
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


## Common Probability Distributions

### Bernoulli Distribution

-   Represents two possible outcomes (success/failure) with probabilities p and 1−p.

```R
library(ggplot2)

p_success <- 0.3
data_bernoulli <- rbinom(1000, 1, p_success)
ggplot(data.frame(outcome = factor(data_bernoulli)), aes(x = outcome)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bernoulli Distribution",
       x = "Outcome (Success/Failure)",
       y = "Frequency")
```
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/92f622fd-c8bd-4633-85a6-3f510a45739e">
</center>

### Binomial Distribution

-   Represents the number of successes in a fixed number of independent Bernoulli trials.

```R
# 10 trials with probability of success 0.5
n_trials <- 10
p_success <- 0.5

# data from Binomial distribution
data_binomial <- rbinom(1000, n_trials, p_success)

#  histogram
ggplot(data.frame(outcome = data_binomial), aes(x = outcome)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Binomial Distribution",
       x = "Number of Successes",
       y = "Frequency")
``` 
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/e95fd339-7471-42f1-aa73-f508c7f65f67">
</center>

### Normal Distribution

-   Symmetric distribution with a bell-shaped curve.

```R
# mean = 0 and standard deviation = 1
data_normal <- rnorm(1000, mean = 0, sd = 1)

# histogram
ggplot(data.frame(value = data_normal), aes(x = value)) +
  geom_histogram(binwidth = 0.2, fill = "salmon", color = "black") +
  labs(title = "Normal Distribution",
       x = "Value",
       y = "Frequency") +
  theme_minimal()
```
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/df28d48a-079d-4816-b78b-4cfc19fdace3">
</center>

## 6. Hypothesis Testing

### Null and Alternative Hypotheses

-   **Null Hypothesis (H0):** A statement to be tested.
-   **Alternative Hypothesis (H1​) :** The hypothesis we want to support.

### Type I and Type II Errors

-   **Type I Error:** Rejecting a true null hypothesis.
-   **Type II Error:** Failing to reject a false null hypothesis.

### Significance Level and P-Value

-   **Significance Level (α):** The probability of committing a Type I Error.
-   **P-Value:** The probability of obtaining results as extreme as the observed results, assuming the null hypothesis is true.

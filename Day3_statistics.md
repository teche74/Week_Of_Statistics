---------
DAY3 -STATISTICS
---------

## DISTRIBUTIONS
- Distributions helps us to have an idea about dataset.

#### **Normal Distribution**

- When data is normally distributed in dataset, there is no skew. When follow a graph creates a bell curve with most values concentrated in central region and tapering off as they go further away from the center.

<center>
<image src = "https://thumbs.dreamstime.com/b/normal-distribution-statistics-teacher-explains-to-students-principles-area-probability-231294621.jpg">
</center>

#### Why it matters a lot ??😲

- All kinds of variables in natural and social sciences are normally or approximately normally distributed. Height, birth weight, reading ability, job satisfaction, or SAT scores are just a few examples of such variables.

- Because normally distributed variables are so common, many statistical tests are designed for normally distributed populations.

- Understanding the properties of normal distributions means you can use  inferential statistics to compare different groups and make estimates about populations using samples.

#### What are the properties of normal distributions?

Normal distributions have key characteristics that are easy to spot in graphs:

-   The  mean,  median and mode  are exactly the same.
-   The distribution is symmetric about the mean—half the values fall below the mean and half above the mean.
-   The distribution can be described by two values: the mean and the standard deviation.


- **The mean determines where the peak of the curve is centered. Increasing the mean moves the curve right, while decreasing it moves the curve left.**
<center>
<image src = "https://cdn.corporatefinanceinstitute.com/assets/skewness2.png">
</center>


- **The standard deviation stretches or squeezes the curve. A small standard deviation results in a narrow curve, while a large standard deviation leads to a wide curve.**
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/17d41253-60fb-4a19-8f8d-500f900c331f">
</center>
 
 ### Empirical rule

The  **empirical rule**, or the 68-95-99.7 rule, tells you where most of your values lie in a normal distribution:

-   Around 68% of values are within 1 standard deviation from the mean.
-   Around 95% of values are within 2 standard deviations from the mean.
-   Around 99.7% of values are within 3 standard deviations from the mean.
<center>
<image src ="https://www.simplypsychology.org/wp-content/uploads/normal-distribution-1024x640.jpeg">
</center>

### Normal Curve Formula

- Once you have the mean and standard deviation of a normal distribution, you can fit a normal curve to your data using a **probability density function**.
- In a probability density function, the area under the curve tells you probability. The normal distribution is a probability distribution, so the total area under the curve is always 1 or 100%.
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/27bbca34-d192-4be7-9adb-884a1bfbb674">
</center>


### Standard Normal Distribution

- The **standard normal distribution**, also called the **_z_-distribution**, is a special normal distribution where the mean is 0 and the standard deviation is 1.
- _Z_-scores tell you how many standard deviations away from the mean each value lies.
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/f7de2dcd-9035-47ac-973f-a85f627740eb">
</center>

##### Caculating Z-Score
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/2b357aff-e2f2-4ec7-94e8-8fe77cf7451b">
</center>

- **Z-Score is used in process of Standardization, while in case of normalization we use Minmax Scalar**

`Example : ` Suppose we have an dataset with mean 4 and standard deviation 1 such that `data : {3.439524, 3.769823, 5.558708, 4.070508, 4.129288, 5.715065, 4.460916, 2.734939, 3.313147, 3.554338}`. What % of Scores fall below 4.25 ?

`Solutiton : `
**Step1 :** Calculate z score for 4.25 which results as **0.25** in terms of standard deviation
```R
# Values
x_i = 4.25
mean =4
stan_dev = 1

# Computing z_score 
z_score <- (x_i - mean)/stan_dev;
z_score
```

**Step 2 :** check for value in z table for the value 0.25 and subtract it from 1 .Final result is **1- 0.5987 = 0.4013 (40%)**

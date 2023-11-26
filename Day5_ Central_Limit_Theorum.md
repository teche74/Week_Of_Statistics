## CENTRAL LIMIT THEORUM

- One of the most powerful concept when you get to know about it.

<center>
	**Distribution Of Sample mean of a large number of independent and identically distributed random variable will approach anormal distribution.**
</center>

#### What it mean ??😲🔎

- Let suppose we take an dataset of Average Salary Of Indians which might looks like the below given figure.

<center>
<image src= "https://github.com/teche74/Week_Of_Statistics/assets/129526047/376f09a3-2f89-469d-9e69-9f48e34c2cb7">
</center>
 
 >**Note that above image is just an infrence. As its almost impossible to get details of each indivisual salary in Bharat🗺️**
 
 - Now its time to understand what central limit theorum says :  `You can take up any distribution (Normal, Log, Binomial etc ) and if you randomly generate sample 1000 times ( it's not fixed ). Now if you took mean of each sample and plot the mean sample distribution ,you will see that it will definitely following normal disrtibution (bell curve)`.  

- Wait wait to follow `CTL (Central Limit Theorum)`. It must follow these `3 conditions.`

**Condition 1 :**: Size of Sample must be large (atleast 30) to perform CTL.

**Condition2 :** : Sample is drawn from either finite or infinite populations `must have a finite variance` .

**Condition 3** : Random variable in sample are `dependent and identically distributed`.

#### Lets Check whether its true or not  ??🧐🤓

- **For Normal Distribution**
```R
# Setting up parameters for population
population_mean <- 50
population_sd <- 10
sample_size <- 30
num_samples <- 1000 

# Generate 1000 sample of size 30 with mean =50 and sd =10 and follows normal distrbution.
samples <- matrix(rnorm(sample_size * num_samples, mean = population_mean, sd = population_sd), ncol = num_samples)

# Extracting mean of Each sample 
sample_means <- rowMeans(samples)

# Plot histogram to check whther it follows normal distribution or not ?
hist(sample_means, main = "Distribution of Sample Means", xlab = "Sample Mean", col = "coral", border = "black")

# Adding legend
legend("topright", legend = c("Sample Means" ), fill = c("coral"))
```

<center>
<image src="https://github.com/teche74/Week_Of_Statistics/assets/129526047/2c0233e9-bb66-4e91-a628-705cd1b23684">
</center>

**Wow it follows Normal Distribution.**

- **For Poisson Distribution**

```R
# For reproducibility
set.seed(123)

# Parameters ]
lambda <- 5  # (mean)

# Sample Parameters
sample_size <- 100
num_samples <- 1000

# Generate 1000 samples of 100 size
samples <- matrix(rpois(sample_size * num_samples, lambda = lambda), ncol = num_samples)

# Sample means calculation
sample_means <- rowMeans(samples)

# Plot the histogram
hist(sample_means, main = "Distribution of Sample Means (Poisson)", xlab = "Sample Mean", col = "skyblue", border = "black")


# Add a legend
legend("topright", legend = c("Sample Means", "Normal Distribution"), fill = c("skyblue", "red"))
```
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/4ba83eb4-6c21-4bcc-a863-f3e2690fd3f0">
</center>

**Its Working, the sample mean distribution of poison distribution follows bell curve** 




## DID  YOU KNOW ??
- Using Central limit theorem you can easily predict population mean and standard deviation by given sample.  

> ** Lets understand it with an Example : **
- `Suppose you had to know about the average salary of Indians, and you know that it difficult to go and get indivisual salary report in billions of population. So what you can do is to have random sampling technique to create multiple samples (must follows condition of CTL). at last you can get mean of these sample and you obviously get a normal distribution. From this sample mean and sd, you can predict mean and sd of population. Its heart whelming right😲🎓`

> Population mean = Sample mean.
> population vaiance = variance of sample * sample_size .
```R
# Set the parameters
num_samples <- 10000
sample_size <- 50

# Log-Normal distribution parameters
mu <- 0.5
sigma <- 0.7

# Calculate the theoretical mean and variance
theoretical_mean <- exp(mu + (sigma^2) / 2)
theoretical_variance <- (exp(sigma^2) - 1) * exp(2 * mu + sigma^2)

# Generate samples from the Log-Normal distribution
samples <- matrix(rlnorm(num_samples * sample_size, meanlog = mu, sdlog = sigma), ncol = sample_size)

# Calculate the sample means
sample_means <- rowMeans(samples)

```
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/baf5b967-0477-462d-b7b4-757a3036157f">
</center>

> Theoritical vairance / sample size  == Emperical Vairance **(2.81 /50 = 0.056)** . Its Correct . 

### SUMMARIZING CTL

- **For any Distribution, sample mean of random samples will always follow normal distribution under specific conditions.**
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/d6b22516-55ae-4bb8-ad42-ddfd1c83e7c6">
</center>

- **The CLT allows us to make inferences about the population parameters (mean, variance) based on sample statistics. For example, if we have a sufficiently large sample size, we can use the sample mean to make inferences about the population mean.**

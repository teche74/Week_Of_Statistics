
# INTRODUCTION TO STATISTICS

- What is `Statistics` ? 
> Science of collecting, organizing and analyzing data for critical decision making. 

- What is 'Data' ?
> Peices of information which can be measured. (raw facts and figures)

- eg : Measuring IQ of student's in a class.
```r
 IQ_record <- c(95,98,60,74,89,58) # data which can be measured 
```

- eg : Age of students in a class
```r
 Age_record <- c(18,15,12,17,19,11)  
```


# TYPE OF STATISTICS

Statistics is used to either gain information from data or use information or data to validate your point or collect evidences in your favour .

Based on these idealogy, we can describe statistics maily in two categories :

`Descriptive Statistics : ` It is used to gain information about data.
`Infrential Statistics : ` It is a technique where we use data to draw some conclusions.

## Descriptive Statistics

- It consist of organizing and summarizing data.


#### **Measure Of Central Tendency**
    
- Mean ( Tells about average )
- Median ( Central point of data )
- Mode ( Most frequent Value )

> eg : We have students marks record of 1 semester .Descriptive stats help us to determine following information from data :
    **Average marks?**
    **Marks frequently occurs?**
    
- We use R language to get desciption of data 
```R
marks_record <- c(84,86,87,65,50,31,22,89,95);

# Average marks of class 
mean(marks_record);

# Marks frequently occurs
getmode <- function(v) {
   uniqv <- unique(v) # Extract unique elements.
   uniqv[which.max(tabulate(match(v, uniqv)))] 
   # First Tabulate the unique
   # Then find max index 
   # And return it in vector format.
}
getmod(marks_record);
```
    
- **Central Tendency is used to describe data using single integer value.**



####  **Measure Of Variance**
  
##### Standard Deviation    
  
  - Standard deviation is a useful measure of spread for **normal distributions**

- In normal distributions, data is symmetrically distributed with no  **skew**. Most values cluster around a central region, with values tapering off as they go further away from the center. The standard deviation tells you how spread out from the center of the distribution your data is on average.

- Many scientific variables follow normal distributions, including height, standardized test scores, or job satisfaction ratings. When you have the standard deviations of different  **samples**.


`What does standard deviation tell you?`

> Example: Comparing different standard deviations

You collect data on job satisfaction ratings from three groups of employees.

The mean ( **M** ) ratings are the same for each group – it’s the value on the x-axis when the curve is at its peak. However, their standard deviations ( **SD** ) differ from each other.

The standard deviation reflects the dispersion of the distribution. The curve with the lowest standard deviation has a high peak and a small spread, while the curve with the highest standard deviation is more flat and widespread.

![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/29a9a58f-f8ae-479b-bf19-fef36e430031)

#####  Variance

- The  **variance**  is a measure of  variability . It is calculated by taking the average of squared deviations from the mean.

- Variance tells you the degree of spread in your data set. The more spread the data, the larger the variance is in relation to the  mean.

	- Population variance

		- When you have collected data from every member of the  [population](https://www.scribbr.com/methodology/population-vs-sample/)  that you’re interested in, you can get an exact value for population variance.

		- The **population variance**  formula looks like this:
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/2042e9ff-76a4-46c3-b23c-5c599ff53af1)

	- Sample variance

		- When you collect data from a sample, the sample variance is used to make estimates or  inferences about the population variance.

		- The  **sample variance**  formula looks like this:
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/1d25d1e6-9a5b-4b6c-b6b9-906871476bc1)
 
##### Range

- In statistics, the  **range**  is the spread of your data from the lowest to the highest value in the distribution. It is a commonly used measure of  **variability**.

- Along with measures of  **central tendency**, measures of variability give you  **descriptive statistics** for summarizing your data set.

- The range is calculated by subtracting the lowest value from the highest value. While a large range means high variability, a small range means low variability in a distribution.

![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/5e38defc-31cc-415c-9d94-ba75787a4cb1)

##### InterQuartile Ranges

- In  descriptive statistics, the  **interquartile range** tells you the spread of the middle half of your distribution.

- **Quartiles** segment any distribution that’s ordered from low to high into four equal parts. The interquartile range (IQR) contains the second and third quartiles, or the middle half of your data set.
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/b7210caa-245b-4a7d-b6d4-5b02ebe92136)
 
 - The interquartile range is found by subtracting the Q1 value from the Q3 value:
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/efac150c-bc5a-4364-bfb4-238e685f1c59)

- `Q1` is the value below which `25 percent` of the distribution lies, while `Q3` is the value below which `75 percent` of the distribution lies.


## POPULATION AND SAMPLES
![enter image description here](https://img.freepik.com/premium-vector/cartoon-elections-vote-design_24877-14751.jpg?size=526&ext=jpg)

`example : ` Suppose There is an election held on **Goa** and **UP**. Now we obviously knows that it is difficult to find the exit pole. It is not possible for a reporter to go to each indivisual and ask whom they voted ??  

![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/eddabd9e-67f9-4ec6-8ae7-bbd6bac36ca5)

- Reporter took **samples** from entire **population** of different region and based on their data , he creates exit pole. 
		- `Population :` ( **N** ) entire group that you want to draw conclusions about.
		- `Sample` ( **n** ) is the specific group that you will collect data from. The size of the sample is always less than the total size of the population.
		

## SAMPLING TECHNIQUES

- There are multiple ways to took out sample from dataset based on specific problems.

#### Simple Random Sampling

- It is most frequently used technique.
-  We randomly select samples from entire population.
- Every member of population have an equal chance of getting selected for sample ( **n** ).
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/fde17a19-d759-4aa3-ac86-47c4f0749eb6" >
</center>

#### Stratified Sampling

- Where the population ( **N** ) is split into Non Overlapping groups, called **stratas**.

`example : ` ==Gender== split into ==male== and ==female==
<center>
<image src = "https://github.com/teche74/Week_Of_Statistics/assets/129526047/a8b7a0c3-6316-4dfb-8847-10dc2da6da09">
</center>

#### Systematic Sampling

- From given Population **N** we define a systematic behaviour of sampling , like ==pick every Nth indivisual==.
<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/eb1584b5-b283-4e9c-b753-ae739581ec2a">
</center>


#### Convinence Sampling 

- When sampling is performed over specific domain and only samples are allowed which lies to that domain.

<center>
<image src ="https://github.com/teche74/Week_Of_Statistics/assets/129526047/1fb5f7c0-4ce8-4ceb-9b34-953abc8c1b6b">
</center> 


## Variables

- It is a property that can take on any value.
 `example : ` Height, Weight etc.

#### **Kinds of Variable**

- `Quantitative Variable` : Variables which can be measured numerically. We can Perform mathematical operations in it .
	- Example : Age, Weight etc. 

- `Qualitative Variable`: Variables can be defined based on categories. We cannot perform any kind of mathematical operations in it . 
	-  Example :  Gender, Age Group, Blood group.

<image src ="https://images.ctfassets.net/dkgr2j75jrom/A6Xf1MfISZhiQWuyGFDpV/7ee6b8f5538a321e4edbe23b87556064/PillarPage-Qual-Quan__2_-min.png?w=750&h=383&q=50&fm=png">

```R
# Quantitative Variable
Age <- c(10,18,22,32,2,36,61,74)

# We can  perform operations 
mean(Age);
median(Age);
sd(Age);
quantiles(Age);


# Qualitative Variable
Flowers = c("Rose","Lily","Rose","Marigold","Marigold","Rose","Tulip")

# We cannot find mean, median here 

# We can generate Frequency distribution.
table(Flowers)

# We can create  Barcharts.
Barplot(Flowers)
```


#### **Variable Measurment Scales**

- There are 4 types of measurement scales :
	- Ordinal.
	- Nominal.
	- Interval.
	- Ratio.


##### `Nominal`
	 -  Split into classes and categories.
	 -  Example : Color, Grades, Flower Types etc.  
![enter image description here](http://intellspot.com/wp-content/uploads/2017/09/Examples-of-Nominal-Data-short-infographic.png)

##### `Ordinal`
	- Order of data matters but value doesnot.
	- Example : Students ranking.
![enter image description here](https://github.com/teche74/Week_Of_Statistics/assets/129526047/f648ff6a-8749-411a-9b71-6e2de1f62021)

##### `Interval`
	- Order of data matters, value also matters, but natural zero is not present.
	- Example : Temprature, Distance.
![enter image description here](https://imgs.search.brave.com/rp6XZFqk5JAI7rdX1FXo8Oe1T8x9ALFVQ51ZWEorqUQ/rs:fit:500:0:0/g:ce/aHR0cHM6Ly9oZWxw/ZnVscHJvZmVzc29y/LmNvbS93cC1jb250/ZW50L3VwbG9hZHMv/MjAyMy8wOC9pbnRl/cnZhbC12YXJpYWJs/ZS1leGFtcGxlcy1h/bmQtZGVmaW5pdGlv/bi0xMDI0eDcyNC5q/cGc)

##### `Ratio`
	- Order of data matters, value also matters and natural zero is also present.
	- Example : Speed, Height, No of times a person do workout in a week.

### Conclusion

![enter image description here](https://d3mm2s9r15iqcv.cloudfront.net/en/wp-content/uploads/old-blog-uploads/four-levels-of-measurement-data.jpg)

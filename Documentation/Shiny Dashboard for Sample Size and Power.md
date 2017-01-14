---
title: "Shiny Dashboard for Sample Size and Power"
author: "Matthew F. Partridge"
date: "12/30/2016"
output:
  html_document:
    toc: true
    toc_float: true
    toc_depth: 4
runtime: shiny
---

# Table of Contents


# 1. Introduction
The purpose of this Shiny Dashboard is to create an updated and interactive version of the Piface Java applet created by Russell V. Lenth _CITATION_. The ability to use interactive sliders and text boxes allows the user to see in real time how certain inputs, e.g. significance level or power, affect outputs, e.g. sample size or power. This will not only provide answers, but also provide education to the user about co-dependencies of the inputs and outputs.

## 1.1 R and R Studio
This Shiny Dashboard is programmed using `R` version 3.2.2 and RStudio version 0.99.489. Four packages, besides the base packages included in R, are used for dashboard setup and for statistical calculations.

## 1.2 Packages
### 1.2.1 `shiny`
[shiny](https://cran.r-project.org/web/packages/shiny/shiny.pdf)  
The `shiny` package in R "Makes it incredibly easy to build interactive web applications with R."_CITATION (URL above)_ It uses R functions to create HTML code for a web page. There are various different input, display, and settings options that can be customized all within the same shiny application.

### 1.2.2 `shinydashboard`
[link](https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf)  
The `shinydashboard` package expands beyond the `shiny` package to add themes to the dashboard for a more attractive look.

### 1.2.3 `pwr`
[link](https://cran.rstudio.com/web/packages/pwr/pwr.pdf)  
The `pwr` package calculates power and sample sizes for various different situations using Cohen _1980_ as a basis for the calculations. In this dashboard, the `pwr` package is used for the calculations of the One Sample Mean, One Sample Proportion, Two Sample Means, and Two Sample Proportions tabs.

### 1.2.4 `gsDesign`
[link](https://cran.r-project.org/web/packages/gsDesign/gsDesign.pdf)  
The `gsDesign` package is used for power and sample size calculations of time to event studies. Specifically, it incorporates factors targeting the time component that is common these types of studies.

# 2. Using the Application
The setup of each of the tabs is very similar with only the specific inputs being slightly different between each of them. Each tab has a drop down selector with either two or three options of what to solve for.
<!--html_preserve--><div class="form-group shiny-input-container" style="width: 100%;">
<label class="control-label" for="solvefor_OM">
<p>Solve For</p>
</label>
<div>
<select id="solvefor_OM" class="form-control"><option value="Sample Size" selected>Sample Size</option>
<option value="Power">Power</option>
<option value="Precision">Precision</option></select>
<script type="application/json" data-for="solvefor_OM">{}</script>
</div>
</div><!--/html_preserve-->
The `selectizeInput` above is from the One Sample Mean tab and shows that Sample Size, Power, and Precision can be calculated. The dashboard changes layouts and widgets depending on the tab and what is being solved for. One Sample Mean, One Sample Proportion, Two Sample Means, and Two Sample Proportions are all set up with study information on the left-hand panel and calculation information as well as the calculation itslef on the right-hand panel. For all of the tabs, the variable that is being calculated is outlined in green.


```
## Error in loadNamespace(name): there is no package called 'webshot'
```

Each numeric variable has both a text box and a slider that can be manipulated. If one is manipulated, the other will update to the new value and the desired output will then be calculated based on the new parameter values. Although the output variable text box and slider can be manipulated, it will not cause any new calculations to be performed.


# 3. Application Tabs
## 3.1 One Sample Mean
### 3.1.1 Parameters

* Mean - The mean of the target population
* True Mean - The mean of the reference population
* Standard Deviation - The standard deviation of the measure for the target population
* Sample Size - The sample size of the experimental population
* Significance Level - The probability of incorrectly rejecting a true null hypothesis
* Power - The probability of correctly rejecting a false null hypothesis
* Precision - The distance in one direction of the confidence interval from the mean
* Confidence Interval - The interval in which the mean is likely to fall within

### 3.1.2 Use of Parameters
The power, sample size, precision, and confidence interval are all parameters that can be calculated, while the power and sample size are also parameters that can be used as inputs parameters for calculations. All of the other parameters can be changed and manipulated resulting in different values for sample size, power, and precision.

### 3.1.3 Statistical Explanation

### 3.1.4 Example
This tab is used in the scenario when there is a one arm treatment that is being tested against a known number. Specifically in this case, the affected measurement is continuous and is summarized with a mean. An example of solving for the needed sample size in this scenario is if a comparison of heart rates is being examined for patients at rest versus those after walking for 5 minutes. Heart rate is a measurement that can be summarized using the mean. Say it is known that the average resting heart rate of a typical patient is 70 beats per minute, while the average heart rate after walking is not known. In this case, the true mean would be 70 and the significance level can be 0.05 and the power can be 0.8, which are both very common levels. The experimental mean and standard deviation of the population should both either be reasonable estimates or obtained from previous or pilot studies.

## 3.2 One Sample Proportion
### 3.2.1 Parameters

* Proportion - The proportion affected in the target population
* True Proportion - The proportion affected in the reference population
* Sample Size - The sample size of the target population
* Significance Level - The probability of incorrectly rejecting a true null hypothesis
* Power - The probability of correctly rejecting a false null hypothesis
* Precision - The distance in one direction of the confidence interval from the mean
* Confidence Interval - The interval in which the mean is likely to fall within

### 3.2.2 Use of Parameters

### 3.2.3 Statistical Explanation

### 3.2.4 Example

## 3.3 Two Sample Means
### 3.3.1 Parameters

* Mean One - The mean of the first population
* Sample Size One - The sample size of the first population
* Mean Two - The mean of the second population
* Sample Size Two - The sample size of the second population
* Standard Deviation - The standard deviation of the measure for the target population
* Significance Level - The probability of incorrectly rejecting a true null hypothesis
* Power - The probability of correctly rejecting a false null hypothesis

### 3.3.2 Use of Parameters

### 3.3.3 Statistical Explanation

### 3.3.4 Example

## 3.4 Two Sample Proportions
### 3.4.1 Parameters

* Proportion One - The proportion affected in population one
* Sample Size One - The sample size of population one
* Proportion Two - The proportion affected in population two
* Sample Size Two - The sample size of population two
* Significance Level - The probability of incorrectly rejecting a true null hypothesis
* Power - The probability of correctly rejecting a false null hypothesis

### 3.4.2 Use of Parameters

### 3.4.3 Statistical Explanation

### 3.4.4 Example

## 3.5 Time to Event
### 3.5.1 Parameters

* Enrollment Schedule - The time at which subjects are enrolled in the study
* Distribution of Enrollment - The distribution of how subjects are enrolled
* Rate - Rate of growth (decay) under the exponential enrollment assumption
* Study Duration - The duration of the study in units of time
* Enrollment Duration - The duration of enrollment in units of time
* Sample Allocation Ratio - The ratio of sample sizes for population one to population two
* Targeted Event Rate - The number of events per unit of time for the targeted population
* Targeted Censoring Rate - The rate at which events will not be observed in the targeted population
* Reference Event Rate - The number of events per unit of time for the reference population
* Reference Censoring Rate - The rate at which events will not be observed in the reference population
* Total Sample Size - The total sample size of both the targeted and reference populations added together
* Significance Level - The probability of incorrectly rejecting a true null hypothesis
* Power - The probability of correctly rejecting a false null hypothesis

### 3.5.2 Use of Parameters

### 3.5.3 Statistical Explanation

### 3.5.4 Example

# 4 References







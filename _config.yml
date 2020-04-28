---
title: "IBS 538 - Capstone - Preety"
author: "Preety Shakya"
date: "4/28/2020"
output: html_document
---

```{r message=FALSE, warning=FALSE}
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ez)
```

##1) Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Hypoplastic Left Heart Syndrome (HLHS) is a critical congenital heart disease characterized by underdeveloped left side of the heart that affects normal blood flow. To avoid fatality, babies born with this condition must undergo a set of aggressive palliative operations early in their life. Since the right ventricle (RV) facilitate both pulmonary and systemic blood flow, RV hypertrophy and pumping inefficiency are inevitable consequences that eventually lead to RV failure. In order to circumvent the clinical complications of RV load bearing, there is a great need of novel regenerative therapies to improve RV tissue functionality during early stages of life. The proposed study focuses on investigating an injectable, minimally invasive dual therapy that combines human cardiac-derived c-kit+ cells and porcine cardiac-derived extracellular matrix (cECM) hydrogel for treatment of RV dysfunction in HLHS. This naturally derived biomaterial supports and directs the embedded cells allowing for local diffusion of therapeutic paracrine factors over a long time period and provides healthy biochemical cues for optimal cellular function. The overall goal of this project is to obtain data for preclinical studies and accelerate the therapy towards IND application for clinical translation.**

##2) Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**It is not known whether injecting human c-kit+ cells encapsulated in cECM hydrogel results in functional improvement of the heart in an athymic rat RV failure model.**

##3) Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If human c-kit+ cells encapsulated in cECM hydrogel is injected in an athymic rat RV failure model, the RV function will improve compared to injecting cells only and cECM only. The functional improvement will be comparable to that of sham (positive control) group.**

##4) What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**There will be four treatment groups: sham, cECM only, cells only and cells with cECM. Transthoracic echocardiography will be performed before and after injection of therapeutic once a week for a total of 4 weeks. The dependent variables that will be observed to test this prediction are treatment and time which are both sorted variables. Tricuspid annular plane systolic excursion (TAPSE) which is a functional measurement of RV obtained from echocardiography will be the independent variable. This is a measured variable.**

##5) Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3.

**Null hypothesis: The TAPSE measurement of rats treated with cells and cECM hydrogel will be equal to or lower than rats treated with cells only and cECM hydrogel only and different than that of sham group.**

**Alternate hypothesis: The TAPSE measurement of rats treated with cells and cECM hydrogel will be greater than rats treated with cells only and cECM hydrogel only and equal to that of sham group.**

##6) What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**A two-way, mixed ANOVA will be done to test the hypothesis since we have time and treatment as two predictor variables where the treatment is randomized, and time is related measure variable. Bonferroni post hoc analysis will be done for a strong protection against type 1 error since we are doing multiple comparisons.**

##7) List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**The units for time will be in days, and the unit for TAPSE will be mm. Rats will be randomly assigned to each treatment group. The endpoint of the experiment will be at the end of 4 weeks post injection of treatment. The decision threshold for type1 error will be 5%. The null hypothesis will be rejected at a p-value of less than 0.05. The sample size will be based upon 90% power. The tolerance threshold for type2 error will be 10%. The independent biological replicates for a treatment group will be the number of rats assigned to that group.**

##8) Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```{r}
b <-  2.5 #expected outcome value of positive control
a <-  0.5 #scientifically relevant fold-to-basal effect of cell only treatment
f <-  0.6 #scientifically relevant fold-to-basal effect of cECM only treatment
g <- 0.25 #expected fold-to-basal effect of cell + cECM treatment
sd <-  0.01 #expected standard deviation of outcome variable
n <-  4
r <- 0.9 
k <- sqrt(1-r^2)

RMdataMaker <- function(n, b, a, f, g, sd, r, k) {
  a1 <- rnorm(n, b, sd) #sham
  a2 <- a1*r-k*rnorm(n, (b*a), sd) #cell only
  a3 <- a1*r-k*rnorm(n, (b*f), sd) #cECM only
  a4 <- a1*r-k*rnorm(n, (b*g), sd) #cECM+cell
    Tapse <- c (2.5, a1, 1.5, a2, 1.5, a3, 1.5, a4)
    Time <- c(rep(c(0,7,14,21,28),n))
    Treatment <- rep(c("Sham", "cells only", "cECM only", "cells+cECM"), each = 5)
    df <-data.frame(Treatment, Time, Tapse)
    }
dat <- RMdataMaker(n,b,a,f,g,sd,r,k)

ggplot(dat, 
       aes(Time, Tapse, color=Treatment, group=Treatment)
       )+ 
  geom_line()+
  geom_point(size = 4, shape=5)+
  scale_color_viridis_d()+
  theme_classic()+
  labs(title= "TAPSE", x="Time (days)", y= "TAPSE (mm)")
```
##9) Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint.

```{r message=FALSE, warning=FALSE}
sims = 100 #number of Monte Carlo simulations to run.
pval <- replicate(
  sims, {
    sample.df <- RMdataMaker(n, b, a, f, g, sd, r, k)
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = Treatment,
            dv = Tapse,
            within = Time,
            type = 2,
            )
  pval <- sim.ezaov$ANOVA[1,5]
    }
  )
pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power.")
```

**The value of n that gives us 90% power is the sample size necessary to test the hypothesis.**

##10) Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.

**Done!**

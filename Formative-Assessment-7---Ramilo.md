Formative Assessment 7
================
Ramilo, Zion John Yousef T.

1.  Provide an R or Python (with markdown) file for the independent
    samples t-test for Invisibility Cloak data set.

- The file should have all the codes/and or explanations from Assumption
  1 to 5 and computation.
- Present the results of the analysis by making a short report.

``` r
df <- read_csv("DataSets/Invisibility Cloak.csv")
```

    ## Rows: 24 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): Participant, Cloak, Mischief
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(df)
```

    ## # A tibble: 6 × 3
    ##   Participant Cloak Mischief
    ##         <dbl> <dbl>    <dbl>
    ## 1           1     0        3
    ## 2           2     0        1
    ## 3           3     0        5
    ## 4           4     0        4
    ## 5           5     0        6
    ## 6           6     0        4

Assumptions:

1.  The dependent variable needs a continuous scale
2.  The independent variable needs to have two independent groups with
    two levels.
3.  The data should have independence of observations. More
    specifically, there shouldn’t be the same participants in both
    groups.
4.  The dependent variable should be normally or near-to-normally
    distributed for each group
5.  There should be no extreme outliers.
6.  The data must have homogeneity of variances.

(Assumption Check 1) Check if the dependent variable is in a continuous
scale. Our dependent in the data set is the variable Mischief, where the
values are in the interval form, where data like these are considered as
continuous data.

(Assumption Check 2) Check if the independent variable have independent
groups with two levels. The dataset has two distinct levels which are
the groups with a cloak and the group without a cloak.

(Assumption Check 3) Since there are distinct numbers for the
participant variable then we can say that there are unique participants
within the experiment and that there are no participants that
participated on the other groups

(Assumption Check 4) -\> We can check normality using graphically and
using the shapiro wilk test

``` r
#Graphical
withoutCloakGroup <- df[df$Cloak == 0, ]

hist(withoutCloakGroup$Mischief, breaks = 5, probability = TRUE,
     main = "Distribution of Mischief (Cloak = 0)", xlab = "Mischief", ylab = "Density", col = "lightgreen")
```

![](Formative-Assessment-7---Ramilo_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
testForNormality_a <- shapiro.test(withoutCloakGroup$Mischief)
testForNormality_a
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  withoutCloakGroup$Mischief
    ## W = 0.91276, p-value = 0.2314

``` r
withCloakGroup <- df[df$Cloak == 1, ]
hist(withCloakGroup$Mischief, breaks = 5, main = "Distribution of Mischief (Cloak = 1)", xlab = "Mischief", ylab = "Frequency", col = "lightpink")
```

![](Formative-Assessment-7---Ramilo_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
testForNormality_b <- shapiro.test(withCloakGroup$Mischief)
testForNormality_b
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  withCloakGroup$Mischief
    ## W = 0.97262, p-value = 0.9362

Lets test for overall dataset

``` r
hist(df$Mischief, breaks = 10, main = "Distribution of Mischief", xlab = "Mischief", ylab = "Frequency", col = "lightblue")
```

![](Formative-Assessment-7---Ramilo_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
testForNormality_c <- shapiro.test(df$Mischief)
testForNormality_c
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  df$Mischief
    ## W = 0.95439, p-value = 0.3363

Let us say that our alpha or signifance level is at 0.05, Let H_0 = The
data is normally distributed Let H_a = The data is not normally
distributed

Since our p values for all distributions for each groups and the
combined groups are bigger than 0.05 then we fail to reject the null
hypothesis therefore we can say that our data is normally distributed.

(Assumption Check 5) Check if the data has extreme outliers.

``` r
boxplot(df$Mischief, main = "Boxplot of Mischief", horizontal = TRUE)
```

![](Formative-Assessment-7---Ramilo_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
It is worth noting that the t-test is robust for minor violations in
normality. So we can have the data 0 within our dataset.

(Assumption Check 4) Check if it passes the levene’s test of
homogeniety, H_0 = the variances are homogeneous H_a = the variances are
not homogeneous

``` r
df$Cloak <- as.factor(df$Cloak)
levene_Test<- leveneTest(df$Mischief ~ df$Cloak, data = df)
print(levene_Test)
```

    ## Levene's Test for Homogeneity of Variance (center = median)
    ##       Df F value Pr(>F)
    ## group  1  0.2698 0.6087
    ##       22

Since our significant level is 0.05 and the p-value not beiiong able to
be less than our signifance level indicates that we fail to reject our
null hypothesis, meaning we can say that our data is homogeneous.

Since all assumptions are satisfied let us perform the t-test: H_0 =
There is no significant difference between the two groups H_a = There is
a significant difference between the two groups

``` r
independentSamplesTTest <- t.test(df$Mischief ~ df$Cloak, data = df)
print(independentSamplesTTest)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  df$Mischief by df$Cloak
    ## t = -1.7135, df = 21.541, p-value = 0.101
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.764798  0.264798
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##            3.75            5.00

Given by the p-value being greater than 0.05 we can say that we fail to
reject our null hypothesis and therefore mean that there is no
significant difference between the means of the two groups.

We have calculated a t value of a negative wherein this iimplies that
group 0 must have a smaller mean than group 1, this mean tells us the
expected times the group 0 conduct mischief is about 3.75 times whiles
group 1 is about 5 times.

Statistical assignment 3
================
James Hughes 660025969
12/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)
library(readr)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/James/Documents/Data III/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        # Create a vector of the variables with the correct prefix.
        varsToSelect <- paste(letters[i], vars, sep = "_")
        # Add pidp to this vector (no prefix for pidp)
        varsToSelect <- c("pidp", varsToSelect)
        # Now read the data. 
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        # Now we can remove data to free up the memory.
        rm(data)
} 
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
  pivot_longer(a_memorig:g_vote6, names_to = "variable", values_to = "value") %>% 
  ## Separating the 'variable' column into 'wave' and 'variable'
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
## Making it wider, putting all the variables back in
  pivot_wider(names_from = variable, values_from = value)
```

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
table(Long$vote6)
```

    ## 
    ##    -10     -9     -7     -2     -1      1      2      3      4 
    ##   4615    366  24725    431    358  30903 102110  86712  84295

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 2, "female",
                           ifelse(sex_dv == 1, "male", NA))) %>%
        mutate(vote6 = case_when(
          vote6 %in% 1:4 ~ vote6,
          vote6 < 1 ~ NA_integer_
          
        ))

table(Long$sex_dv)
```

    ## 
    ## female   male 
    ## 117665 100342

``` r
table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>%
  group_by(wave, sex_dv) %>% 
  filter(!is.na(sex_dv)) %>% 
  summarise(mean(vote6, na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv `mean(vote6, na.rm = TRUE)`
    ##    <chr> <chr>                        <dbl>
    ##  1 a     female                        2.84
    ##  2 a     male                          2.53
    ##  3 b     female                        2.82
    ##  4 b     male                          2.51
    ##  5 c     female                        2.87
    ##  6 c     male                          2.54
    ##  7 d     female                        2.89
    ##  8 d     male                          2.55
    ##  9 e     female                        2.87
    ## 10 e     male                          2.51
    ## 11 f     female                        2.81
    ## 12 f     male                          2.47
    ## 13 g     female                        2.73
    ## 14 g     male                          2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
names(meanVote6) <- c("Wave", "Sex", "Mean_Pol")

meanVote6Wide <- meanVote6 %>%
  dcast(Sex ~ Wave)
```

    ## Warning in dcast(., Sex ~ Wave): The dcast generic in data.table has been
    ## passed a grouped_df and will attempt to redirect to the reshape2::dcast; please
    ## note that reshape2 is deprecated, and this redirection is now deprecated as
    ## well. Please do this redirection yourself like reshape2::dcast(.). In the next
    ## version, this warning will become an error.

``` r
meanVote6Wide
```

    ##      Sex        a        b        c        d        e        f        g
    ## 1 female 2.839437 2.816370 2.874985 2.887006 2.865092 2.807873 2.728400
    ## 2   male 2.527112 2.512143 2.544448 2.551704 2.507875 2.472188 2.415998

These findings suggest that males show consistently higher levels of
political interest than females in Waves 1-7, as they have a lower mean
value of hvote 6 in every wave. Both males and females show small
fluctuations in mean political interest throughout the waves. Both
groups show an overall increase in political interest between waves 1
and 7, with females going from 2.84 to 2.73, and males going from 2.52
to 2.41 in Wave 1 to Wave 7 respectively.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.
2.  Calculate Delta for each person in the data set.
3.  Calculate mean Delta for men and women.
4.  Calculate mean Delta by age (at wave 1) and plot the local
    polynomial curve showing the association between age at wave 1 and
    mean Delta. You can use either **ggplot2** or the *scatter.smooth()*
    function from base R.
5.  Write a short interpretation of your findings.

<!-- end list -->

``` r
## 1. To simplify interpretation, keep only the respondents with non-missing values for political interest in all seven waves.
## 2. Calculate mean delta for each person in the data set

DeltaSet <- Long %>% 
  group_by(pidp) %>%
  filter(!is.na(vote6)) %>% 
  filter(n()>= 7)

DeltaSet <- DeltaSet %>%
  group_by(pidp) %>% 
  mutate(deltarow = order_by(wave, vote6 - lag(vote6))) %>% 
  mutate(deltarow = ifelse(is.na(deltarow), 0, deltarow))

DeltaSet$deltarow = abs(DeltaSet$deltarow)

DeltaSet <- DeltaSet %>%
  group_by(pidp) %>% 
  mutate(
    sum.delta = sum(deltarow, na.rm = TRUE)
    )

## 3. Calculate mean delta for men and women 

sexdelta <- as.table(tapply(DeltaSet$sum.delta, DeltaSet$sex_dv, mean, na.rm = TRUE))

sexdelta
```

    ##   female     male 
    ## 2.494727 2.527760

``` r
## 4. Calculate mean Delta by age (at wave 1) and plot the local polynomial curve showing the association between age at wave 1 and mean Delta. You can use either **ggplot2** or the *scatter.smooth()* function from base R.

agedelta <- DeltaSet %>%
  mutate(case_when(
    age_dv[wave == "a"] == '-9' ~ NA
  )) %>% 
  filter(!is.na(age_dv))

agedelta <- subset(agedelta, subset = agedelta$wave == "a")

agemeantable <- data.frame(tapply(agedelta$sum.delta, agedelta$age_dv, mean))

agemeantable$Age <- c(15:90, 91, 95)

names(agemeantable) <- c("Mean_Delta", "Age")

ggplot(agemeantable, aes(x = Age, y = Mean_Delta)) + geom_point() + geom_smooth()
```

![](assignment3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
## 5. Write a short interpretation of your findings.
```

The graph suggests that between the ages of 15 and 50, political
interest (mean delta) remains relatively constant, perhaps decreasing
slightly but remaining at an average of about 2. However, as age
increases past 55, mean delta also increases. This suggests that older
voters are more likely to change their level of political interest
year-on-year than younger voters. As age increases past around 75, the
points vary from the mean greatly. This is largely due to the lack of
voters who are above the age of 75 in Wave A.

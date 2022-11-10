# Abhigna Ramamurthy - ALY 6010
# comorbidities
getwd()

df0 <- read.csv(file = 'comorbidities.csv', stringsAsFactors=FALSE)
head(df0)
str(df0)
summary(df0)

df1 <- as.data.frame(df0)

#deleting column 1 : comorbidity as it does not help with analysis 
df1 <- subset(df1, select = -c(comorbidity))

# converting sex, ethnicity and race to factor from character as it will be helpful to use group by analysis
df1$sex = as.factor(df1$sex)
df1$ethnicity = as.factor(df1$ethnicity)
df1$race = as.factor(df1$race)
head(df1)

nrow(df1)
# remove data that has sex, race and ethnicity as unknown
df2<-df1[!(df1$sex=="Unknown" & df1$ethnicity=="Unknown" & df1$race=="Unknown"),]
nrow(df2)
# 122 rows deleted. 

# Analysing how the data is spread
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

library(dplyr)

# Analysing data spead by sex
df3_sex <- table(df2$sex)
df3_sex

df3_sex_df <- as.data.frame(df3_sex)
df3_sex_df

# normal bar plot 
barplot(df3_sex, main="Distribution of sex data",col=c("lightblue"), xlab="Sex", cex.names=0.7)

# rename column names
colnames(df3_sex_df)[1] <- "Sex"
colnames(df3_sex_df)[2] <- "Count"

# plot using ggplot
ggplot(data=df3_sex_df, aes(x=Sex, y=Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+ 
  ggtitle("Figure 1: Plot showing spread of data by sex") 

#232 Unknown sex data.

# Analysing data spead by ethnicity
df3_ethnicity <- table(df2$ethnicity)
df3_ethnicity

df3_ethnicity_df <- as.data.frame(df3_ethnicity)
df3_ethnicity_df


# rename column names
colnames(df3_ethnicity_df)[1] <- "Ethnicity"
colnames(df3_ethnicity_df)[2] <- "Count"

# plot using ggplot
ggplot(data=df3_ethnicity_df, aes(x=Ethnicity, y=Count)) +
  geom_bar(stat="identity", fill="lightgreen")+
  geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+ 
  ggtitle("Figure 2: Plot showing spread of data by ethnicity") 

# Analysing data spead by race
df3_race <- table(df2$race)
df3_race

df3_race_df <- as.data.frame(df3_race)
df3_race_df


# rename column names
colnames(df3_race_df)[1] <- "Race"
colnames(df3_race_df)[2] <- "Count"

# plot using ggplot
ggplot(data=df3_race_df, aes(x=Race, y=Count)) +
  geom_bar(stat="identity", fill="red")+
  geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+ 
  ggtitle("Figure 3: Plot showing spread of data by race") + 
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=14,face="bold"))

# Total number of cases = 253896
unique(df2[c("case")])
totalCases <- nrow(df2)

# total number of deaths = 11359
unique(df2[c("death")])

df4_deaths <- filter(df2, df2$death == 1)
head(df4_deaths)
totalDeaths <- nrow(df4_deaths)

Cases <- c("Cases", "Deaths")
Count <- c(nrow(df2), nrow(df4_deaths))

df4_cases_deaths <- data.frame(Cases, Deaths)

# Plotting cases versus deaths total 
ggplot(data=df4_cases_deaths, aes(x=Cases, y=Count)) +
  geom_bar(stat="identity", fill="orange")+
  geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+ 
  ggtitle("Figure 4: Plot showing spread of data by cases and deaths") 

# Calculate the probability of death (i.e. COVID-19 infection fatality rate) 

prob_death <- (totalDeaths/totalCases)*100
prob_death

# conduct one-sample t-tests of mean or proportion using a hypothesized value (usually 0 or the national norm). 
# Supposed the true population COVID-19 infection fatality rate during the same period was 0.042.
# Use prop.test() or binom.test() for testing the proportion in R.
# Was the infection fatality rate in Georgia statistically different from the hypothesized population rate during this period? 

# prop.test is used as the number of sample size is high

res <- prop.test(x = totalDeaths, n = totalCases,  p = 0.042,
                 correct = FALSE)
res

tidy_prop_test <- broom::tidy(res)
tidy_prop_test 
write_xlsx(tidy_prop_test,"/Users/abhi/Desktop/tidy_prop_test.xlsx")

install.packages("gginference")
library(gginference)
ggproptest(res)

res$p.value
res$estimate
res$conf.int

# binomial test 
binom_test <- binom.test(totalDeaths, totalCases, p = 0.042,
           conf.level = 0.95)
binom_test
tidy_binom_test <- broom::tidy(binom_test)
tidy_binom_test 
write_xlsx(tidy_binom_test,"/Users/abhi/Desktop/tidy_binom_test.xlsx")


# plot of binomial test 
success <- 0:totalDeaths

plot(success,dbinom(success,size=totalCases,prob= 0.04473879),
     type='h',
     main='Figure 5: Binomial Distribution (n= 253896, p=0.04473879)',
     ylab='Probability',
     xlab ='# Successes',
     lwd=3)

# t-test 
library(stats)
t_test <- t.test(df2$death)
t_test

library(broom)
tidy_t_test <- broom::tidy(t_test)
tidy_t_test 
library("writexl")
write_xlsx(tidy_t_test,"/Users/abhi/Desktop/tidy_t_test2.xlsx")



#  Conduct multiple one-sample t-tests of proportion (prop.test or binom.test) 
# for multiple groups. Note that you may need to create two vectors to isolate a group. 
# For example, conducting a t-test for a male sample and another t-test for a female sample 
# requires filter( ) into two vectors, and then run a test for each sample. 
# Are they different from the hypothesized population rate (0.042)? 
# Make decision based on p-value. Do this in R. 
# Provide the null and the alternative hypothesis for each test. 
# Provide the test results in a table and several paragraphs interpreting the results.

male_sample <- filter(df2, df2$sex == 'Male' )
head(male_sample)
no_of_male_cases <- nrow(male_sample)
no_of_male_deaths <- nrow(filter(male_sample, male_sample$death == 1 ))
no_of_male_cases
no_of_male_deaths
male_prop <- (no_of_male_deaths/no_of_male_cases)*100
male_prop

female_sample <- filter(df2, df2$sex == 'Female')
head(female_sample)
no_of_female_cases <- nrow(female_sample)
no_of_female_deaths <- nrow(filter(female_sample, female_sample$death == 1 ))
no_of_female_cases
no_of_female_deaths
female_prop <- (no_of_female_deaths/no_of_female_cases)*100
female_prop

sex_prop_test <- prop.test(x=c(no_of_female_deaths, no_of_male_deaths), n=c(no_of_female_cases, no_of_male_cases),
          conf.level=0.95, 
          correct = FALSE,
          alternative = "greater")
sex_prop_test
sex_prop_test$p.value
sex_prop_test$estimate
sex_prop_test$conf.int

male_prop_test <- prop.test(x= no_of_male_deaths, n= no_of_male_cases,
                           conf.level=0.95, 
                           correct = FALSE)
male_prop_test

female_prop_test <- prop.test(x=no_of_female_deaths, n=no_of_female_cases,
                           conf.level=0.95, 
                           correct = FALSE)
female_prop_test
tidy_male_prop_test <- broom::tidy(male_prop_test)
tidy_male_prop_test
tidy_female_prop_test <- broom::tidy(female_prop_test)
tidy_female_prop_test

comb <- rbind(tidy_female_prop_test, tidy_male_prop_test)
comb
write_xlsx(comb,"/Users/abhi/Desktop/tidy_sex_prop_test_combined.xlsx")

tidy_sex_prop_test <- broom::tidy(sex_prop_test)
tidy_sex_prop_test 
write_xlsx(tidy_sex_prop_test,"/Users/abhi/Desktop/tidy_sex_prop_test.xlsx")

# Multiple one sample test for ethnic groups 
# filtering ethnicity data 
hispanic_sample <- filter(df2, df2$ethnicity == 'Hispanic/ Latino' )
head(hispanic_sample)
no_of_hispanic_cases <- nrow(hispanic_sample)
no_of_hispanic_deaths <- nrow(filter(hispanic_sample, hispanic_sample$death == 1 ))
no_of_hispanic_cases
no_of_hispanic_deaths
hispanic_prop <- (no_of_hispanic_deaths/no_of_hispanic_cases)*100
hispanic_prop

nonhispanic_sample <- filter(df2, df2$ethnicity == 'Non-Hispanic/ Latino' )
head(nonhispanic_sample)
no_of_nonhispanic_cases <- nrow(nonhispanic_sample)
no_of_nonhispanic_deaths <- nrow(filter(nonhispanic_sample, nonhispanic_sample$death == 1 ))
no_of_nonhispanic_cases
no_of_nonhispanic_deaths
nonhispanic_prop <- (no_of_nonhispanic_deaths/no_of_nonhispanic_cases)*100
nonhispanic_prop

unknown_sample <- filter(df2, df2$ethnicity == 'Unknown' )
head(unknown_sample)
no_of_unknown_cases <- nrow(unknown_sample)
no_of_unknown_deaths <- nrow(filter(unknown_sample, unknown_sample$death == 1 ))
no_of_unknown_cases
no_of_unknown_deaths
unknown_prop <- (no_of_unknown_deaths/no_of_unknown_cases)*100
unknown_prop



ethnicity_prop_test <- prop.test(x=c(no_of_hispanic_deaths, no_of_nonhispanic_deaths), n=c(no_of_hispanic_cases, no_of_nonhispanic_cases),
                           conf.level=0.95, 
                           correct = FALSE,
                           alternative = "greater")


ethnicity_prop_test
ethnicity_prop_test$p.value
ethnicity_prop_test$estimate
ethnicity_prop_test$conf.int

tidy_ethnicity_prop_test <- broom::tidy(ethnicity_prop_test)
tidy_ethnicity_prop_test 
write_xlsx(tidy_ethnicity_prop_test,"/Users/abhi/Desktop/tidy_ethnicity_prop_test.xlsx")


hispanic_prop_test <- prop.test(x=no_of_hispanic_deaths, n=no_of_hispanic_cases,
                                 conf.level=0.95, 
                                 correct = FALSE)


hispanic_prop_test


nonHispanic_prop_test <- prop.test(x= no_of_nonhispanic_deaths, n= no_of_nonhispanic_cases,
                                 conf.level=0.95, 
                                 correct = FALSE)


nonHispanic_prop_test

unknown_prop_test <- prop.test(x= no_of_unknown_deaths, n= no_of_unknown_cases,
                                 conf.level=0.95, 
                                 correct = FALSE)


unknown_prop_test

tidy_hispanic_prop_test <- broom::tidy(hispanic_prop_test)
tidy_hispanic_prop_test
tidy_nonHispanic_prop_test <- broom::tidy(nonHispanic_prop_test)
tidy_nonHispanic_prop_test
tidy_unknown_prop_test <- broom::tidy(unknown_prop_test)
tidy_unknown_prop_test

comb2 <- rbind(tidy_hispanic_prop_test, tidy_nonHispanic_prop_test,tidy_unknown_prop_test)
comb2
write_xlsx(comb2,"/Users/abhi/Desktop/tidy_ethnicity_prop_test_combined.xlsx")

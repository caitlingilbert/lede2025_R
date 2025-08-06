library(tidyverse)
library(vroom)

# download data file from https://realtimecrimeindex.com/data/
allcrimes <- vroom("final_sample.csv")

glimpse(allcrimes)

# just look at nationwide, full sample
nationwidecrime <- allcrimes %>%
  filter(State == "Nationwide" & Agency == "Full Sample")

# Location measures
median(nationwidecrime$`Violent Crime`)
mean(nationwidecrime$`Violent Crime`)

# Dispersion measures
min(nationwidecrime$`Violent Crime`)
max(nationwidecrime$`Violent Crime`)
range(nationwidecrime$`Violent Crime`)
max(nationwidecrime$`Violent Crime`) - min(nationwidecrime$`Violent Crime`)

quantile(nationwidecrime$`Violent Crime`, 0.25)
quantile(nationwidecrime$`Violent Crime`, 0.75)
quantile(nationwidecrime$`Violent Crime`, 0.92)
IQR(nationwidecrime$`Violent Crime`)

sd(nationwidecrime$`Violent Crime`)
var(nationwidecrime$`Violent Crime`)

# Let's look at that distribution
hist(nationwidecrime$`Violent Crime`)

nationwidecrime %>%
  ggplot(aes(`Violent Crime`)) +
  geom_histogram()

nationwidecrime %>%
  ggplot(aes(`Violent Crime`)) +
  geom_histogram(bins = 15)

boxplot(nationwidecrime$`Violent Crime`)

# get all of these summary stats at once!
summary(nationwidecrime)

library(descr)
descr(nationwidecrime)

# or you can group by another variable to get summary stats by that variable
nationwidecrime_byregion <- allcrimes %>%
  filter(State == "Nationwide" & Region != "Other")

by(nationwidecrime_byregion, nationwidecrime_byregion$Region, summary)

# correlation (two variables)
cor(nationwidecrime$`Violent Crime`, nationwidecrime$`Property Crime`)
cor(nationwidecrime$`Violent Crime`, nationwidecrime$`Property Crime`,
    method = "spearman")

# let's look at all possible correlations!
nationwidecrime_quant <- nationwidecrime %>%
  select(2,8:16)

round(cor(nationwidecrime_quant),
      digits = 2 # rounded to 2 decimals
)

library(corrplot)
corrplot(cor(nationwidecrime_quant),
         method = "number",
         type = "upper" # only show upper side of plot
         )

# contingency table (two qualitative variables)
crime_bystate <- allcrimes %>%
  filter(State != "Nationwide" & Agency != "Full Sample")
table(crime_bystate$State, crime_bystate$Region)
table(crime_bystate$Region, crime_bystate$Source.Type)

# visualize distributions for one quant/one qual variable
boxplot(nationwidecrime_byregion$`Violent Crime` ~ nationwidecrime_byregion$Region)

nationwidecrime_byregion %>%
  ggplot(aes(x = Region, y = `Violent Crime`)) +
  geom_boxplot() +
  theme_minimal()

nationwidecrime_byregion %>%
  ggplot(aes(x = Region, y = `Violent Crime`)) +
  geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 300) +
  theme_minimal()

# visualize the two-quantitative variables you correlated as a scatterplot
nationwidecrime %>%
  ggplot(aes(x=`Violent Crime`, y=`Property Crime`)) +
  geom_point() +
  theme_minimal()

nationwidecrime_byagency <- allcrimes %>%
  filter(State == "Nationwide" & startsWith(Agency, "Agencies"))

nationwidecrime_byagency %>%
  ggplot(aes(x=`Violent Crime`, y=`Property Crime`, color=Agency)) +
  geom_point() +
  theme_minimal()

library(car)
qqPlot(nationwidecrime_byregion$`Violent Crime`, groups = nationwidecrime_byregion$Region)

### HYPOTHESIS TESTING

# test the correlation between two quantitative variables (assuming independent variables, normal distribution)
cor.test(nationwidecrime$`Violent Crime`, nationwidecrime$`Property Crime`,
         alternative = "greater")

# test if there is a relationship between two qualitative variables
# also known as the Chi-Squared Test of Independence, requires independent variables
chisq.test(table(crime_bystate$Region, crime_bystate$Source.Type))

# proportion testing (for large samples, n > 30; otherwise use binom.test)
# alt-hypothesis: proportion of crime data from Midwest versus South is different
table(crime_bystate$Region)
prop.test(
  x = 14391, # number of "successes" (Midwest)
  n = 49061, # total number of trials (total number)
  p = 0.5 # we test for equal proportion so prob = 0.5 in each group
)

# One mean
t.test(x = crime_bystate$Murder[crime_bystate$Region=="South"],
       mu = 20)

## Compare two (or more) means

# Are the means of murder counts in the Northeast and South different?
crime_bystate_agg <- crime_bystate %>%
  group_by(Year, State, Region) %>%
  summarise(murder_total = sum(Murder)) %>%
  filter(Region %in% c("Northeast","South"))

crime_bystate_wide <- crime_bystate_agg %>%
  pivot_wider(names_from = Region, values_from = murder_total)

t.test(crime_bystate_wide$Northeast, crime_bystate_wide$South, # variance unknown/equal
       var.equal = TRUE, alternative = "greater"
       )

t.test(murder_total ~ Region, # variance unknown/unequal
       data = crime_bystate_agg,
       var.equal = FALSE,
       alternative = "less"
       )

wilcox.test(murder_total ~ Region,
            data = crime_bystate_agg) # non-parametric

crime_bystate_agg2 <- crime_bystate %>%
  group_by(Year, State, Region) %>%
  summarise(murder_total = sum(Murder))

oneway.test(murder_total ~ Region,
            data = crime_bystate_agg2,
            var.equal = TRUE # assuming equal variances
            )

oneway.test(murder_total ~ Region,
            data = crime_bystate_agg2,
            var.equal = FALSE # assuming unequal variances
            )

library(ggstatsplot)

ggbetweenstats(
  data = nationwidecrime_byregion,
  x = Region,
  y = `Violent Crime`,
  type = "parametric", # ANOVA or Kruskal-Wallis
  var.equal = FALSE, # ANOVA or Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE
)

# Did violent crime numbers change from 2018 to 2024?
nationwide_violentcrime_wide <- nationwidecrime %>%
  select(Year, Month, `Violent Crime`) %>%
  pivot_wider(names_from = Year, values_from = `Violent Crime`)

t.test(nationwide_violentcrime_wide$`2018`, nationwide_violentcrime_wide$`2024`,
       alternative = "greater",
       paired = TRUE
       )

### LINEAR REGRESSION

model <- lm(`Violent Crime` ~ `Property Crime`, data = nationwidecrime)
summary(model)

library(ggpubr)

nationwidecrime %>%
  ggplot(aes(x = `Violent Crime`, y = `Property Crime`)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.x = 45000, label.y = 250000) + # for regression equation
  stat_cor(aes(label = after_stat(rr.label)), label.x = 32000, label.y = 250000) + # for R^2
  theme_minimal()

# see how the slope changes! is it better or worse?
model2 <- lm(`Violent Crime` ~ `Property Crime` + Theft, data = nationwidecrime)
summary(model2)



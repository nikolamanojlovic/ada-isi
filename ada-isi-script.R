# add sources for packages
options(repos = c(CRAN = "https://cloud.r-project.org"))

# install needed packages
install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("nortest") # to replace ks test as it is not correct

# activate library for usage
library(readxl)
library(dplyr)
library(tidyr)
library(nortest)

# reading excel data
gei_data <- read_excel("Gender Equality Index.xlsx", col_names = TRUE)

if (nrow(gei_data) != 28 || ncol(gei_data) != 53) {
  stop(("Data not read properly, columns and row mismatch."))
}
print("Data sucessfully read.")

#
# Obtain the parameters of descriptive statistics for the indicators Care Activities and Economic Situation
#

descriptive_columns <- c("Care activities", "Economic situation")
descriptives_data <- gei_data %>% select(all_of(descriptive_columns))

descriptives <- descriptives_data %>%
  summarise(across(everything(), list(
    Mean = ~mean(.x, na.rm = TRUE), # na.rm - ignore blanks
    SD = ~sd(.x, na.rm = TRUE),
    Median = ~median(.x, na.rm = TRUE),
    Min = ~min(.x, na.rm = TRUE),
    Max = ~max(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
  separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value)  

print("Descriptives for Care Activities and Economic Situation:")
print(descriptives)

print("Boxplot for Care Activities and Economic Situation:")
# persist plot for additional information
svg("gei_boxplot.svg", width = 8, height = 8)
boxplot(descriptives_data,
        main = "Boxplot of Care activities and Economic situation",
        ylab = "Values",
        col = c("blue", "green"),
        names = descriptive_columns,
        pch = 1,             # circle for outliers
        cex = 1.2,           # size of outliers
        outcol = "black")

# add some important points
points(1:ncol(descriptives_data), descriptives$Mean, col = "lightblue", pch = 16, cex = 1.2)
points(1:ncol(descriptives_data), descriptives$Median, col = "lightgreen", pch = 17, cex = 1.2)

# add legend for readability
legend("bottomright", legend = c("Mean", "Median", "Outliers"),
       col = c("lightblue", "lightgreen", "black"), pch = c(16, 17, 1), cex = 0.9)

dev.off()

variance_check <- descriptives_data %>%
  summarise(across(everything(), list(
    Variance = ~var(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
  separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value)  

print("Comparison of variance (SD^2) for Care Activities and Economic Situation:")
print(variance_check)

ranges_check <- descriptives_data %>%
  summarise(across(everything(), list(
    # Range = Max - Min
    Range = ~diff(range(.x, na.rm = TRUE)),
    Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
    Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
    # IQR = Q3 - Q2
    IQR = ~IQR(.x, na.rm = TRUE)
  ))) %>%
  pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
  separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
  pivot_wider(names_from = Statistic, values_from = Value)  

print("Comparison of ranges for Care Activities and Economic Situation:")
print(ranges_check)

print("Denisty for Care Activities and Economic Situation:")
# persist plot for additional information
svg("gei_density.svg", width = 16, height = 8)

# get ranges so no cutting happens
dens_care <- density(descriptives_data$`Care activities`, na.rm = TRUE)
dens_econ <- density(descriptives_data$`Economic situation`, na.rm = TRUE)
density_x_range <- range(c(dens_care$x, dens_econ$x))
density_y_range <- range(c(dens_care$y, dens_econ$y))

plot(dens_care, col = "blue", lwd = 2, main = "Density of Care Activities & Economic Situation", xlim = density_x_range, ylim = density_y_range)
lines(dens_econ, col = "green", lwd = 2)

# add legend for readability
legend("bottomright", legend = c("Care activities", "Economic situation"),
       col = c("blue", "green"), lwd = 2)

dev.off()

#
# How many countries belong to each group based on the calculated pillar Work?
#

groups_to_count <- c('Work', 'Money', 'Knowledge', 'Time', 'Power', 'Health')
groups_count <- gei_data %>% 
  select(all_of(groups_to_count)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Group = case_when(
    Value == 1 ~ "High",
    Value == 2 ~ "Medium",
    Value == 3 ~ "Low"
  )) %>%
  count(Variable, Group) %>%
  pivot_wider(names_from = Group, values_from = n, values_fill = 0)

print("Number of countries that belong to group based on Work:")
print(groups_count)

print("Barplot for Work group:")
# persist plot for additional information
svg("gei_work_barplot.svg", width = 8, height = 8)

group_count_labels = c("High","Medium","Low")
barplot(as.numeric(groups_count[match("Work", groups_count$Variable), group_count_labels]),
        names.arg = group_count_labels,
        col = c("green","orange","red"),
        main = "Counts of group numbers based on Work",
        ylab = "Number of Observations")

dev.off()

#
# Is there statistically significant difference between the country groups based on Time for the indicator Access?
#

print("Testing if data is normally distributed with KS test for indicator Access")
print("H0: Access indicator has ND")
print("H1: Access indicator doesn't have ND")

# access_indicator_ks_test <- 
#   ks.test(na.omit(gei_data$Access), "pnorm",
#           mean = mean(gei_data$Access, na.rm = TRUE),
#           sd = sd(gei_data$Access, na.rm = TRUE))

access_indicator_ks_test <- lillie.test(na.omit(gei_data$Access))

print("Results of testing if data is normally distributed with KS test for indicator Access")
print(access_indicator_ks_test)
paste0("Since ", access_indicator_ks_test$p.value ," < 0.05 we conclude that Access indicator is not normally distributed.")

print("Testing if samples are from same population with Kruskal Wallis test for indicator Access for groups based on Time")
print("H0: Samples are from the same population")
print("H1: Samples are not from the same population")

# make time categorical variable
gei_data$TimeCategorical <- factor(gei_data$Time, levels = c(1, 2, 3), labels = group_count_labels)

access_indicator_kruskal_test <- kruskal.test(Access ~ TimeCategorical, data = gei_data)

print("Results of testing if samples are from same population with Kruskal Wallis test for indicator Access for groups based on Time")
print(access_indicator_kruskal_test)
paste0("Since ", access_indicator_kruskal_test$p.value ," < 0.05 we conclude that Access groups based on Time are not from the same population.")

#
# Is there difference in the values of the indicator Share of members of regional assemblies (%) between countries which have medium and high Money?
#

print("Testing if data is normally distributed with KS test for indicator Share of members of regional assemblies (%)")
print("H0: Share of members of regional assemblies (%) indicator has ND")
print("H1: Share of members of regional assemblies (%) indicator doesn't have ND")

gei_data$MoneyCategorical <- factor(gei_data$Money, levels = c(1, 2, 3), labels = group_count_labels)

gei_data_share_of_members_subset <- gei_data[gei_data$MoneyCategorical %in% c("High", "Medium"),]

# share_of_members_indicator_ks_test <- 
#   ks.test(na.omit(gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`), "pnorm",
#           mean = mean(gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`, na.rm = TRUE),
#           sd = sd(gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`, na.rm = TRUE))

share_of_members_indicator_ks_test <- lillie.test(na.omit(gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`))

print("Results of testing if data is normally distributed with KS test for indicator Share of members of regional assemblies (%)")
print(share_of_members_indicator_ks_test)
paste0("Since ", share_of_members_indicator_ks_test$p.value ," > 0.05 we conclude that Share of members of regional assemblies (%) indicator is normally distributed.")

print("Testing if there is statistical difference with Independent Samples T-Test test for indicator Share of members of regional assemblies (%) for groups based on Money")
print("H0: m(medium) = m(high)")
print("H1: m(medium) != m(high)")

share_of_members_indicator_t_test <- 
  t.test(
    gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`[gei_data_share_of_members_subset$MoneyCategorical == "High"],
    gei_data_share_of_members_subset$`Share of members of regional assemblies (%)`[gei_data_share_of_members_subset$MoneyCategorical == "Medium"],
    var.equal = TRUE
    )

print("Results of testing if there is statistical difference with Independent Samples T-Test test for indicator Share of members of regional assemblies (%) for groups based on Money")
print(share_of_members_indicator_t_test)
paste0("Since ", share_of_members_indicator_t_test$p.value ," > 0.05 we conclude that Share of members of regional assemblies (%) groups based on Money don't have statistical differences.")

#
# Is there statistically significant correlation between the indicators Political and Economic.
#

print("Testing if data is normally distributed with KS test for indicators Political and Economic")
print("H0: Political/Economic indicator has ND")
print("H1: Political/Economic indicator doesn't have ND")

# political_indicator_ks_test <- 
#   ks.test(na.omit(gei_data$Political), "pnorm",
#          mean = mean(gei_data$Political, na.rm = TRUE),
#           sd = sd(gei_data$Political, na.rm = TRUE))

political_indicator_ks_test <- lillie.test(na.omit(gei_data$Political))

print("Results of testing if data is normally distributed with KS test for indicator Political")
print(political_indicator_ks_test)
paste0("Since ", political_indicator_ks_test$p.value ," > 0.05 we conclude that Political indicator is normally distributed.")

# economic_indicator_ks_test <- 
#   ks.test(na.omit(gei_data$Economic), "pnorm",
#           mean = mean(gei_data$Economic, na.rm = TRUE),
#           sd = sd(gei_data$Economic, na.rm = TRUE))

economic_indicator_ks_test <- lillie.test(na.omit(gei_data$Economic))

print("Results of testing if data is normally distributed with KS test for indicator Economic")
print(economic_indicator_ks_test)
paste0("Since ", economic_indicator_ks_test$p.value ," > 0.05 we conclude that Economic indicator is normally distributed.")

print("Testing if there is significant correlation with Person coefficient of correlation test for indicators Polictical and Economic")
print("H0: r = 0")
print("H1: r != 0")

political_and_economic_pearson_test <- cor.test(
  gei_data$Political,
  gei_data$Economic,
  method = "pearson"
)

print("Results of testing if there is significant correlation with Person coefficient of correlation test for indicators Polictical and Economic")
print(political_and_economic_pearson_test)
paste0("Since ", political_and_economic_pearson_test$p.value ," < 0.05 we conclude that there is significant correlation between indicators Polictical and Economic.")

#
# Does the Time level impact the Knowledge level? How many countries are there with low Time and medium Knowledge level?
#

# we already have time, but recalculate
gei_data$TimeCategorical <- factor(gei_data$Time, levels = c(1, 2, 3), labels = group_count_labels)
gei_data$KnowledgeCategorical <- factor(gei_data$Knowledge, levels = c(1, 2, 3), labels = group_count_labels)

time_knowledge_contingency_table <- table(gei_data$TimeCategorical, gei_data$KnowledgeCategorical)
print(time_knowledge_contingency_table)

paste0("Dimensions: ", ncol(time_knowledge_contingency_table), " x ", nrow(time_knowledge_contingency_table))

# FALSE because it is 3x3 > 2x2
time_knowledge_contingency_table_chi <- chisq.test(time_knowledge_contingency_table, correct = FALSE)
print(time_knowledge_contingency_table_chi)

print("Check expected counts")
print(time_knowledge_contingency_table_chi$expected)

print("Check if has less than 20% of cells with expected count less than 5")
paste0("Number of cells with expected count less than 5 is ", (sum(time_knowledge_contingency_table_chi$expected < 5) / length(time_knowledge_contingency_table_chi$expected)) * 100, " %")

print("To see if Time impacts Knowlegde we use Likelihood Ratio since dimensions are > 2x2 and 100% > 20% of cells that have count less than 5")
time_knowledge_contingency_table_likelihood_ration <- chisq.test(time_knowledge_contingency_table, simulate.p.value = TRUE, B = 10000)
print(time_knowledge_contingency_table_likelihood_ration)
paste0("Since ", time_knowledge_contingency_table_likelihood_ration$p.value ," < 0.05 we conclude that Knowlegde depends on Time")

#
# Is there statistically significant difference between countries with low and medium Health regarding the values of the indicator Social activities?
#

print("Testing if data is normally distributed with KS test for indicator Social activities")
print("H0: Social activities indicator has ND")
print("H1: Social activities indicator doesn't have ND")

# SKIP - all data is High for health, do test on all
# gei_data$HealthCategorical <- factor(gei_data$Health, levels = c(1, 2, 3), labels = group_count_labels)

# social_activities_ks_test <- 
#  ks.test(na.omit(gei_data$`Social activities`), "pnorm",
#          mean = mean(gei_data$`Social activities`, na.rm = TRUE),
#          sd = sd(gei_data$`Social activities`, na.rm = TRUE))

social_activities_ks_test <- lillie.test(na.omit(gei_data$`Social activities`))

print("Results of testing if data is normally distributed with KS test for indicator Social activities")
print(social_activities_ks_test)
paste0("Since ", social_activities_ks_test$p.value ," > 0.05 we conclude that Social activities indicator is normally distributed.")

# Missing data, further testing cannot be done

#
# Is the mean value of the indicator Status on the population 87?
#

print("Testing if data is normally distributed with KS test for indicator Status")
print("H0: Status indicator has ND")
print("H1: Status indicator doesn't have ND")

# status_ks_test <- 
#  ks.test(na.omit(gei_data$Status), "pnorm",
#          mean = mean(gei_data$Status, na.rm = TRUE),
#          sd = sd(gei_data$Status, na.rm = TRUE))

status_ks_test <- lillie.test(na.omit(gei_data$Status))

print("Results of testing if data is normally distributed with KS test for indicator Status")
print(status_ks_test)
paste0("Since ", status_ks_test$p.value ," > 0.05 we conclude that Status indicator is normally distributed.")

print("Testing if there equality of means with One Sample T-Test test for indicator Status")
print("H0: m = 87")
print("H1: m != 87")

status_one_sample_t_test <- 
  t.test(
    gei_data$Status,
    mu = 87
  )

print("Results of testing if there equality of means with One Sample T-Test test for indicator Status")
print(status_one_sample_t_test)
paste0("Since ", status_one_sample_t_test$p.value ," < 0.05 we conclude that Status mean differs from 87.")

#
# Create a linear regression model and model how Graduates of tertiary education (%) impact Attainment and participation
#

print("Scatterplot for visualisation:")
# persist plot for additional information
svg("gei_graduates_attainment_scatterplot.svg", width = 12, height = 8)

plot(gei_data$`Graduates of tertiary education (%)`, gei_data$`Attainment and participation`,
     xlab = "Graduates of tertiary education (%)",
     ylab = "Attainment and participation",
     main = "Scatterplot of Graduates of tertiary education (%) vs Attainment and participation",
     pch = 19, col = "blue")

dev.off()

print("Least Squares Method to obtain the model equation:")
# NOTE: lm(Y ~ X, data = df) Y we wanna predict
linear_regression_model <- lm(`Attainment and participation` ~ `Graduates of tertiary education (%)`, data = gei_data)
linear_regression_model_summary <- summary(linear_regression_model)
print(linear_regression_model_summary)

linear_regression_model_coef = coef(linear_regression_model)
paste0("Model has coefficients B0 ", linear_regression_model_coef[2], " and B1 ", linear_regression_model_coef[1])
paste0("Model is Y = ", linear_regression_model_coef[2], "X + ", linear_regression_model_coef[1])

paste0("Value of stat for B1 is ", linear_regression_model_summary$coefficients[2, "t value"], " and p value is ", linear_regression_model_summary$coefficients[2, "Pr(>|t|)"])
paste0("Value of R^2 is ", linear_regression_model_summary$r.squared)

print("To test if this model is good for prediction:")
paste0("1. R^2 > 0.5 is true: ", linear_regression_model_summary$r.squared)
paste0("2. p value for B1 < 0.05 in order to accept H1 (B1 is significant) is not true: ", linear_regression_model_summary$coefficients[1, "Pr(>|t|)"])

#
# Is there statistically significant difference between the Power groups if we observe the values of the indicator Social activities.
#

print("Testing if data is normally distributed with KS test for indicator Social Activities")
print("H0: Social Activities indicator has ND")
print("H1: Social Activities indicator doesn't have ND")

# social_activities_ks_test <- 
#  ks.test(na.omit(gei_data$`Social activities`), "pnorm",
#          mean = mean(gei_data$`Social activities`, na.rm = TRUE),
#          sd = sd(gei_data$`Social activities`, na.rm = TRUE))

status_ks_test <- lillie.test(na.omit(gei_data$`Social activities`))

print("Results of testing if data is normally distributed with KS test for indicator Social activities")
print(social_activities_ks_test)
paste0("Since ", social_activities_ks_test$p.value ," > 0.05 we conclude that Social activities indicator is normally distributed.")

print("Testing if there is statistical difference with ANOVA test for indicator Access for groups based on Time")
print("H0: m(low) = m(medium) = m(high)")
print("H1: at least 2 means differ")

# make time categorical variable
gei_data$PowerCategorical <- factor(gei_data$Power, levels = c(1, 2, 3), labels = group_count_labels)

social_activities_indicator_anova_test <- aov(`Social activities` ~ PowerCategorical, data = gei_data)

print("Results of testing if there is statistical difference with ANOVA test for indicator Social activities for groups based on Power")
social_activities_indicator_anova_summary = summary(social_activities_indicator_anova_test)
print(social_activities_indicator_anova_summary)
paste0("Since ", social_activities_indicator_anova_summary[[1]][["Pr(>F)"]][1] ," < 0.05 we conclude that Social activities groups based on Power have statistical differences.")
df <- read.csv("recent-grads.csv")
high_salary_cutoff <- 50000
# ^ The cutoff for what is considered a high salary. 
#Feedback questioned the value of 50k, we can change it 
df$high_salary <- ifelse(df$Median >= high_salary_cutoff, TRUE, FALSE)

#Categorical Data Analysis

#Chi-Squared test of independence for employment vs. category of major - 

contingency_table <- xtabs(cbind(Employed, Unemployed) ~ Major_category, data = df)
#xtabs generates a contingency table. 
#the variable to the right of the tilde (in this case Major_category, specifies what to use for the rows)
#the variable to the left of the tilde specifies that we are using these two columns
#data = df tells as to use our current data frame, the file recent-grads.csv we are analyzing 

chisq.test(contingency_table)

#add row and column totals
table_with_sums <- addmargins(contingency_table)

#access the table to view it - previous code only gives results 
print(table_with_sums)

#Numerical Data Analysis

#Generates category means (of the major median incomes within a category, 
#weighted by the relative frequency of each major in the category)
#I used Sample_size to weight it but Total might be better. 
summary_df <- df %>%
  filter(!is.na(Median), !is.na(Sample_size)) %>%
  group_by(Major_category) %>%
  summarise(Weighted_avg = weighted.mean(Median, Sample_size))

#Barplot of Major_category Weighted_avg
ggplot(summary_df, aes(x = reorder(Major_category, Weighted_avg), y = Weighted_avg)) +
  geom_col(fill="lightblue") +
  coord_flip() + 
labs(
  x = "Major category",
  y = "Weighted average median income",
  title = "Weighted Average Median Income by Major Category"
) +
  theme_minimal()

#Boxplot of Major_category by Median
ggplot(df, aes(x = Major_category, y = Median)) +
  geom_boxplot(fill="lightblue") +
  coord_flip() + 
  labs(
    x = "Major category",
    y = "Median Income",
    title = "Median Incomes of Majors by Category"
  ) +
  theme_minimal()

#ANOVA test on unweighted. An ANOVA test compares between-category variation (Category Medians)
#and within category-variation (Medians of Majors)
anova_df <- df %>%
  filter(!is.na(Median), !is.na(Major_category))

fit <- aov(Median ~ Major_category, data = anova_df)
summary(fit)

#Some kind of table for ANOVA result? 

#Something with unemployment rates, stacked bar graph? 








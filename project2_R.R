df <- read.csv("recent-grads.csv")
high_salary_cutoff <- 50000
# ^ The cutoff for what is considered a high salary. Feedback questioned the value of 50k, we can change it 
df$high_salary <- ifelse(df$Median >= high_salary_cutoff, TRUE, FALSE)

#Numerical Data Analysis
#Generates category means (of the major median incomes within a category, weighted by the relative frequency of each major in the category)

df %>%
  filter(!is.na(Median), !is.na(Sample_size)) %>%
  group_by(Major_category) %>%
  summarise(weighted_avg = weighted.mean(Median, Sample_Size))

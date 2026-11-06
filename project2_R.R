df <- read.csv("recent-grads.csv")
high_salary_cutoff <- 50000
# ^ 
df$high_salary <- ifelse(df$median >= high_salary_cutoff, TRUE, FALSE)

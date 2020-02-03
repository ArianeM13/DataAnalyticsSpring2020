library(dplyr)
df_mtcars <- mtcars
head(df_mtcars)

#nesting
filter(df_mtcars, mpg > 20)
#we want 10 samples of that
sample_n(filter(df_mtcars,mpg>20),10)
#arrangonf in descending order beased on mpg
arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
results_mpg <- arrange(sample_n(filter(df_mtcars,mpg>20),10),desc(mpg))
results_mpg


#Using Multiple assignments
a1 <- filter(df_mtcars,mpg>20)
a2 <- sample_n(a1,5) #getting a random sample of 5
results_mpg_des <- arrange(a2,desc(mpg))
results_mpg_des


#Using Pipe Operator instead of multiple assignments
librabry(dplyr)
df_mtcars %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))
results <- df_mtcars %>% filter(mpg>20) %>% sample_n(5) %>% arrange(desc(mpg))
results

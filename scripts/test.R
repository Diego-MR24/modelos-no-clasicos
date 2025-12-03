source("data/data.R")

df1 <- get_data_logit()
df2 <- get_data_probit()
df3 <- get_data_ridge()
df4 <- get_data_robusta()

head(df1)
head(df2)
head(df3)
head(df4)



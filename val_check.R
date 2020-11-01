df1 = read.csv("outputs/survey_data.csv")
df2 = read.csv("outputs/census_data.csv")

unique(df1$hispan)
unique(df2$hispan)

unique(df1$race)
unique(df2$race)

unique(df1$education)
unique(df2$education)

length(unique(df1$state))
length(unique(df2$state))
unique(df2$state)

unique(df1$state) == unique(df2$state)
sort(unique(df1$state)) == sort(unique(df2$state))

library(dplyr)
student_info <- data.frame(name = c("amy", "ben", "chris", "dan"),
                           age = c(23, 22, 22, 29),
                           height_2010 = c(158, 169, 173, 175),
                           height_2020 = c(164, 185, 180, 176))
student_info$age[2]
colnames(student_info)

# to update the dataframe
student_info <- mutate(student_info, height_avg=((height_2010+height_2020)/2))
student_info

# subset
student_info_above25 <- filter(student_info, age >25)

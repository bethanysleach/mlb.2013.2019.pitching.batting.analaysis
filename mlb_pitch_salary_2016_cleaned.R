install.packages("Lahman")

library(Lahman)
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)
library(ggplot2)
library(knitr)

mlb_salary_2016 <- read_csv("/Users/bethanyleach/Downloads/salaries_2016_final - Sheet1-2.csv")


pitching_table <- Pitching %>%
        filter(yearID >= 2013, yearID <= 2019)

pitching_table2 <- pitching_table

pitch_salary <- merge(pitching_table2, mlb_salary_2016, by="playerID", all=TRUE)
pitch_salary_na <- pitch_salary
table(is.na(pitch_salary_na$salary))
test <- subset(pitch_salary_na, is.na(salary))
data2 <- subset(pitch_salary_na, !is.na(salary))

pitch_salary_cleaner <- pitch_salary_na %>%
        group_by(playerID) %>%
        select(-active_majors_2016)

View(pitch_salary_cleaner)
pitch_salary_na <- pitch_salary_cleaner %>%
        select(-traded_2016)
View(pitch_salary_na)
pitch_salary_aard <- pitch_salary_na[complete.cases(pitch_salary_na), ]
View(pitch_salary_aard)

revised_pitching <- as_tibble(pitch_salary_aard)

selected_columns <- revised_pitching %>%
        dplyr::select(-stint, -L, -CG, -SHO, -H, -ER, -IBB,-IPouts, 
                      -WP, -HBP, -BK, -BFP, -GF, -R, -SH, -SF, -GIDP, -salary, 
                      -total_value, -pos, -years, -avg_annual, -team)

View(revised_pitching)
View(selected_columns)

seven_years <- selected_columns %>%
        mutate(salary_2016=salary_2016) %>%
        dplyr::mutate(salary_2016 = gsub("^.","", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::select(-BB, -GS) %>%
        dplyr::summarize(meanW = mean(W), meanHR = mean(HR), meanSO = mean(SO), 
                         meanERA = mean(ERA), salary_2016, meanSV = mean(SV),
                         meanBAOpp = mean(BAOpp))

View(seven_years)

seven_years$salary_2016 <- as.numeric(seven_years$salary_2016)

seven_years_edited <- seven_years[!duplicated(seven_years$playerID),] %>%
        filter(meanERA > 0 && meanERA < 9 && salary_2016 >=1000000 && meanSV > 0)

View(seven_years_edited)

ggplot(seven_years_edited, aes(meanERA, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean ERA from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average ERA vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

seven_years_edited_2 <- seven_years_edited

ggplot(seven_years_edited, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Strike Outs vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(seven_years_edited, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Home Runs from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Home Runs vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(seven_years_edited, aes(meanW, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Wins from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Wins vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(seven_years_edited, aes(meanERA, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean ERA from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average ERA vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(seven_years_edited, aes(meanSV, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Saves from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Saves vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(seven_years_edited, aes(meanBAOpp, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean BAOpp from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Batting Opponent's Average vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)


era_salary_correlation <- cor(seven_years_edited$meanERA, seven_years_edited$salary_2016)
so_salary_correlation <- cor(seven_years_edited$meanSO, seven_years_edited$salary_2016)
hr_salary_correlation <- cor(seven_years_edited$meanHR, seven_years_edited$salary_2016)
w_salary_correlation <- cor(seven_years_edited$meanW, seven_years_edited$salary_2016)
sv_salary_correlation <- cor(seven_years_edited$meanSV, seven_years_edited$salary_2016)
baopp_salary_correlation <- cor(seven_years_edited$meanBAOpp, seven_years_edited$salary_2016)




#Now will look at the three year avergages before 2016


yr_2013_2015_pitching <- seven_years

yr_2013_2015_pitching <- selected_columns %>%
        filter(yearID > 2012, yearID < 2016) %>%
        mutate(salary_2016=salary_2016) %>%
        dplyr::mutate(salary_2016 = gsub("^.", "", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::select(-BB, -GS) %>%
        dplyr::summarize(meanW = mean(W), meanHR = mean(HR), meanSO = mean(SO), 
                         meanERA = mean(ERA), salary_2016, meanSV = mean(SV),
                         meanBAOpp = mean(BAOpp))

View(yr_2013_2015_pitching)

yr_2013_2015_pitching$salary_2016 <- as.numeric(yr_2013_2015_pitching$salary_2016)

yr_2013_2015_edited <- yr_2013_2015_pitching[!duplicated(yr_2013_2015_pitching$playerID),] %>%
        filter(meanERA > 0 && meanSV > 0)

ggplot(yr_2013_2015_edited, aes(meanERA, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean ERA from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Average ERA vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2013_2015_edited, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Average Strikouts vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2013_2015_edited, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean HR from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Average Homeruns vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2013_2015_edited, aes(meanW, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Wins from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Average Wins vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2013_2015_edited, aes(meanSV, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Saves from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Average Saves vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2013_2015_edited, aes(meanBAOpp, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean BAOpp from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three Year Batting Opponent's Average vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

era_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanERA, yr_2013_2015_edited$salary_2016)
so_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanSO, yr_2013_2015_edited$salary_2016)
hr_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanHR, yr_2013_2015_edited$salary_2016)
w_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanW, yr_2013_2015_edited$salary_2016)
sv_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanSV, yr_2013_2015_edited$salary_2016)
baopp_2013_2015_salary_correlation <- cor(yr_2013_2015_edited$meanBAOpp, yr_2013_2015_edited$salary_2016)


#Now will look at the three year averages after 2016



yr_2017_2019_pitching <- selected_columns %>%
        filter(yearID > 2016, yearID < 2020) %>%
        mutate(salary_2016=salary_2016) %>%
        dplyr::mutate(salary_2016 = gsub("^.", "", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::select(-BB, -GS) %>%
        dplyr::summarize(meanW = mean(W), meanHR = mean(HR), meanSO = mean(SO), 
                         meanERA = mean(ERA), salary_2016, meanSV = mean(SV),
                         meanBAOpp = mean(BAOpp))

View(yr_2017_2019_pitching)

yr_2017_2019_pitching$salary_2016 <- as.numeric(yr_2017_2019_pitching$salary_2016)


yr_2017_2019_edited <- yr_2017_2019_pitching[!duplicated(yr_2017_2019_pitching$playerID),] %>%
        filter(meanERA > 0 && meanSV > 0 && salary_2016 > 600000)

ggplot(yr_2017_2019_edited, aes(meanERA, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean ERA from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Average ERA vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2017_2019_edited, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Average Strikouts vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2017_2019_edited, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Homeruns from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Average Homeruns vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2017_2019_edited, aes(meanW, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Wins from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Average Wins vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2017_2019_edited, aes(meanSV, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Saves from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Average Saves vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(yr_2017_2019_edited, aes(meanBAOpp, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean BAOpp from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three Year Batting Opponent's Average vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)


era_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanERA, yr_2017_2019_edited$salary_2016)
so_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanSO, yr_2017_2019_edited$salary_2016)
hr_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanHR, yr_2017_2019_edited$salary_2016)
w_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanW, yr_2017_2019_edited$salary_2016)
sv_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanSV, yr_2017_2019_edited$salary_2016)
baopp_2017_2019_salary_correlation <- cor(yr_2017_2019_edited$meanBAOpp, yr_2017_2019_edited$salary_2016)


######################

#t-test information

mean_salary_2016_pitch <- mean(seven_years_edited$salary_2016)
sd_salary_2016_pitch <- sd(seven_years_edited$salary_2016)

#Focusing on players who played 2013-2019

playerID_merge_pitch <- yr_2013_2015_edited %>%
        left_join(yr_2017_2019_edited, by = "playerID")

playerID_merge_na_pitch<- playerID_merge_pitch

table(is.na(playerID_merge_na_pitch$meanW.y))
test_merge_pitch <- subset(playerID_merge_na_pitch, is.na(meanW.y))
data_merge_pitch <- subset(playerID_merge_na_pitch, !is.na(meanW.y))


playerID_merge_13_15_pitch <- data_merge_pitch %>%
        select(-meanW.y, -meanHR.y, -meanSO.y, -meanERA.y, -meanSV.y, -meanBAOpp.y, -salary_2016.y)


playerID_merge_na_pitch<- playerID_merge_pitch


ERA_change_previous_latter_pitch <- data_merge_pitch$meanERA.y - data_merge_pitch$meanERA.x
W_change_previous_latter_pitch <- data_merge_pitch$meanW.y - data_merge_pitch$meanW.x
HR_change_previous_latter_pitch <- data_merge_pitch$meanHR.y - data_merge_pitch$meanHR.x
SO_change_previous_latter_pitch <- data_merge_pitch$meanSO.y - data_merge_pitch$meanSO.x
SV_change_previous_latter_pitch <- data_merge_pitch$meanSV.y - data_merge_pitch$meanSV.x
BAOpp_change_previous_latter_pitch <- data_merge_pitch$meanBAOpp.y - data_merge_pitch$meanBAOpp.x


standardized_salary_pitch <- (data_merge_pitch$salary_2016.x - mean_salary_2016_pitch) / (sd_salary_2016_pitch)

finalized_pitch_statistics_change_df <- data.frame(data_merge_pitch$playerID, data_merge_pitch$salary_2016.y, 
                                                   ERA_change_previous_latter_pitch, HR_change_previous_latter_pitch, 
                                                   W_change_previous_latter_pitch, SO_change_previous_latter_pitch,
                                                   SV_change_previous_latter_pitch,
                                                   BAOpp_change_previous_latter_pitch, standardized_salary_pitch)


salary_above_mean_pitch <- finalized_pitch_statistics_change_df %>%
        filter(standardized_salary_pitch > 0)

salary_below_mean_pitch <- finalized_pitch_statistics_change_df %>%
        filter(standardized_salary_pitch < 0)




hist(salary_above_mean_pitch$ERA_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in ERA", col="blue") 

hist(salary_below_mean_pitch$ERA_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in ERA", col="blue") 

hist(salary_above_mean_pitch$W_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Wins (W)", col="blue") 

hist(salary_below_mean_pitch$W_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Wins (W)", col="blue") 

hist(salary_above_mean_pitch$HR_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Home Runs (HR)", col="blue") 

hist(salary_below_mean_pitch$HR_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Home Runs (HR)", col="blue") 

hist(salary_above_mean_pitch$SO_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Strikeouts (SO)", col="blue") 

hist(salary_below_mean_pitch$SO_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Strikeouts (SO)", col="blue") 

hist(salary_above_mean_pitch$SV_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Saves (SV)", col="blue") 

hist(salary_below_mean_pitch$SV_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Saves (SV)", col="blue") 

hist(salary_above_mean_pitch$W_change_previous_latter_pitch, main="Pitchers with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in BAOpp", col="blue") 

hist(salary_below_mean_pitch$W_change_previous_latter_pitch, main="Pitchers with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in BAOpp", col="blue") 

avg_salary_above_mean_pitch_ERA<- mean(salary_above_mean_pitch$ERA_change_previous_latter_pitch)
avg_salary_below_mean_pitch_ERA <- mean(salary_below_mean_pitch$ERA_change_previous_latter_pitch)

sd_salary_above_mean_pitch_ERA <- sd(salary_above_mean_pitch$ERA_change_previous_latter_pitch)
sd_salary_below_mean_pitch_ERA <- sd(salary_below_mean_pitch$ERA_change_previous_latter_pitch)

avg_salary_above_mean_pitch_W<- mean(salary_above_mean_pitch$W_change_previous_latter_pitch)
avg_salary_below_mean_pitch_W <- mean(salary_below_mean_pitch$W_change_previous_latter_pitch)

sd_salary_above_mean_pitch_W <- sd(salary_above_mean_pitch$W_change_previous_latter_pitch)
sd_salary_below_mean_pitch_W <- sd(salary_below_mean_pitch$W_change_previous_latter_pitch)

avg_salary_above_mean_pitch_HR<- mean(salary_above_mean_pitch$HR_change_previous_latter_pitch)
avg_salary_below_mean_pitch_HR <- mean(salary_below_mean_pitch$HR_change_previous_latter_pitch)

sd_salary_above_mean_pitch_HR <- sd(salary_above_mean_pitch$HR_change_previous_latter_pitch)
sd_salary_below_mean_pitch_HR <- sd(salary_below_mean_pitch$HR_change_previous_latter_pitch)

avg_salary_above_mean_pitch_SO <- mean(salary_above_mean_pitch$SO_change_previous_latter_pitch)
avg_salary_below_mean_pitch_SO <- mean(salary_below_mean_pitch$SO_change_previous_latter_pitch)

sd_salary_above_mean_pitch_SO <- sd(salary_above_mean_pitch$SO_change_previous_latter_pitch)
sd_salary_below_mean_pitch_SO <- sd(salary_below_mean_pitch$SO_change_previous_latter_pitch)

avg_salary_above_mean_pitch_SV <- mean(salary_above_mean_pitch$SV_change_previous_latter_pitch)
avg_salary_below_mean_pitch_SV <- mean(salary_below_mean_pitch$SV_change_previous_latter_pitch)

sd_salary_above_mean_pitch_SV <- sd(salary_above_mean_pitch$SV_change_previous_latter_pitch)
sd_salary_below_mean_pitch_SV <- sd(salary_below_mean_pitch$SV_change_previous_latter_pitch)

avg_salary_above_mean_pitch_BAOpp <- mean(salary_above_mean_pitch$BAOpp_change_previous_latter_pitch)
avg_salary_below_mean_pitch_BAOpp <- mean(salary_below_mean_pitch$BAOpp_change_previous_latter_pitch)

sd_salary_above_mean_pitch_BAOpp <- sd(salary_above_mean_pitch$BAOpp_change_previous_latter_pitch)
sd_salary_below_mean_pitch_BAOpp <- sd(salary_below_mean_pitch$BAOpp_change_previous_latter_pitch)


std_error_pitch_ERA = sqrt(((sd_salary_above_mean_pitch_ERA**2)/27) + ((sd_salary_below_mean_pitch_ERA**2)/38))
t_statistic_pitch_ERA <- (avg_salary_above_mean_pitch_ERA - avg_salary_below_mean_pitch_ERA) / (std_error_pitch_ERA)

std_error_pitch_W = sqrt(((sd_salary_above_mean_pitch_W**2)/27) + ((sd_salary_below_mean_pitch_W**2)/38))
t_statistic_pitch_W <- (avg_salary_above_mean_pitch_W - avg_salary_below_mean_pitch_W) / (std_error_pitch_W)

std_error_pitch_HR= sqrt(((sd_salary_above_mean_pitch_HR**2)/27) + ((sd_salary_below_mean_pitch_HR**2)/38))
t_statistic_pitch_HR <- (avg_salary_above_mean_pitch_HR - avg_salary_below_mean_pitch_HR) / (std_error_pitch_HR)

std_error_pitch_SO = sqrt(((sd_salary_above_mean_pitch_SO**2)/27) + ((sd_salary_below_mean_pitch_SO**2)/38))
t_statistic_pitch_SO <- (avg_salary_above_mean_pitch_SO - avg_salary_below_mean_pitch_SO) / (std_error_pitch_SO)

std_error_pitch_SV = sqrt(((sd_salary_above_mean_pitch_SV**2)/27) + ((sd_salary_below_mean_pitch_SV**2)/38))
t_statistic_pitch_SV <- (avg_salary_above_mean_pitch_SV - avg_salary_below_mean_pitch_SV) / (std_error_pitch_SV)

std_error_pitch_BAOpp = sqrt(((sd_salary_above_mean_pitch_BAOpp**2)/27) + ((sd_salary_below_mean_pitch_BAOpp**2)/38))
t_statistic_pitch_BAOpp <- (avg_salary_above_mean_pitch_BAOpp - avg_salary_below_mean_pitch_BAOpp) / (std_error_pitch_BAOpp)

#machine learning

testing_set_13_15_pitch <- playerID_merge_13_15_pitch

mean_salary_test_pitch <-mean(testing_set_13_15_pitch$salary_2016.x)
sd_salary_test_pitch <-sd(testing_set_13_15_pitch$salary_2016.x)


standardized_salary_test_pitch <- (testing_set_13_15_pitch$salary_2016.x - mean_salary_test_pitch) / (sd_salary_test_pitch)

testing_set_13_15_pitch$labels <- as.integer(standardized_salary_test_pitch > 0)
trainIndex2=createDataPartition(testing_set_13_15_pitch$labels, p=0.7)

testing_set_13_15_pitch <- testing_set_13_15_pitch %>%
        select(-playerID, -meanW.x, -meanHR.x, -meanSO.x, -meanERA.x, -meanSV.x, -meanBAOpp.x, 
               -salary_2016.x) %>%
        ungroup()

testing_set_13_15_pitch <- testing_set_13_15_pitch %>%
        mutate(standardized_salary_test_pitch = standardized_salary_test_pitch)




testing_set_13_15_pitch$labels <- as.character(testing_set_13_15_pitch$labels)
testing_set_13_15_pitch$labels <- as.numeric(testing_set_13_15_pitch$labels)

testing_set_13_15_pitch <- testing_set_13_15_pitch[sample(nrow(testing_set_13_15_pitch)),]
testing_13_15_pitch_train <- testing_set_13_15_pitch[1:48,]
testing_13_15_pitch_test <- testing_set_13_15_pitch[48:65,]

testing_13_15_pitch_test$labels <- as.factor(testing_13_15_pitch_test$labels)
testing_13_15_pitch_train$labels <- as.factor(testing_13_15_pitch_train$labels)
nb_pitch <- naivebayes::naive_bayes(labels ~ ., data = testing_13_15_pitch_train)
plot(nb_pitch)

pitching.output <- cbind(testing_13_15_pitch_test, pred_pitch = predict(nb_pitch, testing_13_15_pitch_test))

pred_pitch  <- predict(nb_pitch, newdata = select(testing_13_15_pitch_test,-labels))

caret::confusionMatrix(pitching.output$pred_pitch, pitching.output$labels)

#Mean Salary 2016 = 4377682
#SD 2016 = 3165378


#(Position Players)
#Mean Salary 2016 = 5971553
#SD 2016 = 6710504



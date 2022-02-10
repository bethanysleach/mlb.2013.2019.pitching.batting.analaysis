Batting_2016 <- Batting %>%
        filter(yearID >= 2013, yearID <= 2019)

batting_salary <- merge(Batting_2016, mlb_salary_2016, by="playerID", all=TRUE)

batting_salary_n <- batting_salary

table(is.na(batting_salary_n$salary))
test2 <- subset(batting_salary_n, is.na(salary))
data3 <- subset(batting_salary_n, !is.na(salary))

batting_salary_cleaner <- batting_salary_n %>%
        group_by(playerID) %>%
        select(-active_majors_2016)
View(batting_salary_cleaner)
batting_salary_n <- batting_salary_cleaner %>%
        select(-traded_2016)
View(batting_salary_n)

batting_salary_adj <- batting_salary_n[complete.cases(batting_salary_n), ]
View(batting_salary_adj)

batting_selected_columns<- as_tibble(batting_salary_adj)


batting_selected_columns <- batting_selected_columns %>%
        select(-stint, -salary, -total_value, -years, -avg_annual, -team)

batting_revised_columns <- batting_selected_columns %>%
        select(-R, -IBB, -SH, -SF)
      
batting_altered_columns <- batting_revised_columns %>%
        mutate(salary_2016 = salary_2016, pos = pos) %>%
        dplyr::mutate(salary_2016 = gsub("^.","", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::summarize(meanG = mean(G), meanAB = mean(AB), meanH = mean(H), 
                         mean2B = mean(X2B), mean3B = mean(X3B), meanHR = mean(HR),
                         meanRBI = mean(RBI), meanSB = mean(SB), meanCS = mean(CS),
                         meanBB = mean(BB), meanSO = mean(SO), meanHBP = mean(HBP), 
                         meanGIDP = mean(GIDP), pos, salary_2016)

View(batting_altered_columns)

batting_altered_columns$salary_2016 <- as.numeric(batting_altered_columns$salary_2016)

batting_altered_columns_edited <- batting_altered_columns[!duplicated(batting_altered_columns$playerID),]

View(batting_altered_columns_edited)

unique(batting_altered_columns_edited[c("pos")])

#filter for position players
pos_filtered_batting <- dplyr::filter(batting_altered_columns_edited, pos %in% c("1B", "LF", "SS", "CF", 
                                                                 "2B", "3B", "C", "RF", "OF", "DH"))
View(pos_filtered_batting)

pos_filtered_batting <- pos_filtered_batting %>%
        filter(meanG > 81)

ggplot(pos_filtered_batting, aes(meanRBI, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean RBI from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average RBI vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Home Runs from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Home Runs vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanH, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hits from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Hits vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(mean2B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Doubles from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Doubles vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanBB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Walks from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Walks vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanGIDP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Grounding into DP from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Grounding into DP vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanHBP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hit by Pitch Occurences from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Hit by Pitch Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Strikeouts vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanSB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Stolen Bases from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Stolen Bases vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanCS, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Caught Stealing Occurences from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Caught Stealing Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(mean3B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Triples from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Triples vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanG, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Games from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average Games vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting, aes(meanAB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean At Bats from 2013-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Seven-Year Average At Bats vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

rbi_salary_correlation <- cor(pos_filtered_batting$meanRBI, pos_filtered_batting$salary_2016)
hr_salary_correlation <- cor(pos_filtered_batting$meanHR, pos_filtered_batting$salary_2016)
hits_salary_correlation <- cor(pos_filtered_batting$meanH, pos_filtered_batting$salary_2016)
dbls_salary_correlation <- cor(pos_filtered_batting$mean2B, pos_filtered_batting$salary_2016)
walks_salary_correlation <- cor(pos_filtered_batting$meanBB, pos_filtered_batting$salary_2016)
gidp_salary_correlation <- cor(pos_filtered_batting$meanGIDP, pos_filtered_batting$salary_2016)
hbp_salary_correlation <- cor(pos_filtered_batting$meanHBP, pos_filtered_batting$salary_2016)
so_salary_correlation <- cor(pos_filtered_batting$meanSO, pos_filtered_batting$salary_2016)
sb_salary_correlation <- cor(pos_filtered_batting$meanSB, pos_filtered_batting$salary_2016)
cs_salary_correlation <- cor(pos_filtered_batting$meanCS, pos_filtered_batting$salary_2016)
triples_salary_correlation <- cor(pos_filtered_batting$mean3B, pos_filtered_batting$salary_2016)
games_salary_correlation <- cor(pos_filtered_batting$meanG, pos_filtered_batting$salary_2016)
ab_salary_correlation <- cor(pos_filtered_batting$meanAB, pos_filtered_batting$salary_2016)

#Now will look at the three year avergages before 2016

yr_2013_2015_batting <- batting_revised_columns %>%
        filter(yearID > 2012, yearID < 2016) %>%
        mutate(salary_2016 = salary_2016, pos = pos) %>%
        dplyr::mutate(salary_2016 = gsub("^.","", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::summarize(meanG = mean(G), meanAB = mean(AB), meanH = mean(H), 
                         mean2B = mean(X2B), mean3B = mean(X3B), meanHR = mean(HR),
                         meanRBI = mean(RBI), meanSB = mean(SB), meanCS = mean(CS),
                         meanBB = mean(BB), meanSO = mean(SO), meanHBP = mean(HBP), 
                         meanGIDP = mean(GIDP), pos, salary_2016)
        
View(yr_2013_2015_batting)
yr_2013_2015_batting$salary_2016 <- as.numeric(yr_2013_2015_batting$salary_2016)

yr_2013_2015_batting_edited <- yr_2013_2015_batting[!duplicated(yr_2013_2015_batting$playerID),]

View(yr_2013_2015_batting_edited)

unique(yr_2013_2015_batting_edited[c("pos")])

pos_filtered_batting_13_15 <- dplyr::filter(yr_2013_2015_batting_edited, pos %in% c("1B", "LF", "SS", "CF", 
                                                                                 "2B", "3B", "C", "RF", "OF", "DH"))



pos_filtered_batting_13_15 <- pos_filtered_batting_13_15 %>%
        filter(meanG > 81)
View(pos_filtered_batting_13_15)

ggplot(pos_filtered_batting_13_15, aes(meanRBI, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean RBI from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average RBI vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Home Runs from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Home Runs vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanH, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hits from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Hits vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(mean2B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Doubles from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Doubles vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanBB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Walks from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Walks vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanGIDP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Grounding into DP from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Grounding into DP vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanHBP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hit by Pitch Occurences from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Hit by Pitch Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Strikeouts vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanSB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Stolen Bases from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Stolen Bases vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanCS, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Caught Stealing Occurences from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Caught Stealing Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(mean3B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Triples from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Triples vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanG, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Games from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average Games vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_13_15, aes(meanAB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean At Bats from 2013-2015")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Previous Three-Year Average At Bats vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)


batting_13_15_rbi <- cor(pos_filtered_batting_13_15$meanRBI, pos_filtered_batting_13_15$salary_2016)
batting_13_15_hr <- cor(pos_filtered_batting_13_15$meanHR, pos_filtered_batting_13_15$salary_2016)
batting_13_15_hits <- cor(pos_filtered_batting_13_15$meanH, pos_filtered_batting_13_15$salary_2016)
batting_13_15_dbls <- cor(pos_filtered_batting_13_15$mean2B, pos_filtered_batting_13_15$salary_2016)
batting_13_15_walks <- cor(pos_filtered_batting_13_15$meanBB, pos_filtered_batting_13_15$salary_2016)
batting_13_15_gidp <- cor(pos_filtered_batting_13_15$meanGIDP, pos_filtered_batting_13_15$salary_2016)
batting_13_15_hbp <- cor(pos_filtered_batting_13_15$meanHBP, pos_filtered_batting_13_15$salary_2016)
batting_13_15_so <- cor(pos_filtered_batting_13_15$meanSO, pos_filtered_batting_13_15$salary_2016)
batting_13_15_sb <- cor(pos_filtered_batting_13_15$meanSB, pos_filtered_batting_13_15$salary_2016)
batting_13_15_cs <- cor(pos_filtered_batting_13_15$meanCS, pos_filtered_batting_13_15$salary_2016)
batting_13_15_triples <- cor(pos_filtered_batting_13_15$mean3B, pos_filtered_batting_13_15$salary_2016)
batting_13_15_games <- cor(pos_filtered_batting_13_15$meanG, pos_filtered_batting_13_15$salary_2016)
batting_13_15_ab <- cor(pos_filtered_batting_13_15$meanAB, pos_filtered_batting_13_15$salary_2016)


#Now will look at the three year avergages after 2016


yr_2017_2019_batting <- batting_revised_columns %>%
        filter(yearID > 2016, yearID < 2020) %>%
        mutate(salary_2016 = salary_2016, pos = pos) %>%
        dplyr::mutate(salary_2016 = gsub("^.","", salary_2016)) %>%
        dplyr::mutate(salary_2016 = gsub(",","", salary_2016)) %>%
        group_by(playerID) %>%
        dplyr::summarize(meanG = mean(G), meanAB = mean(AB), meanH = mean(H), 
                         mean2B = mean(X2B), mean3B = mean(X3B), meanHR = mean(HR),
                         meanRBI = mean(RBI), meanSB = mean(SB), meanCS = mean(CS),
                         meanBB = mean(BB), meanSO = mean(SO), meanHBP = mean(HBP), 
                         meanGIDP = mean(GIDP), pos, salary_2016)

View(yr_2017_2019_batting)

yr_2017_2019_batting$salary_2016 <- as.numeric(yr_2017_2019_batting$salary_2016)

yr_2017_2019_batting_edited <- yr_2017_2019_batting[!duplicated(yr_2017_2019_batting$playerID),]

View(yr_2017_2019_batting_edited)


unique(yr_2017_2019_batting_edited[c("pos")])


pos_filtered_batting_17_19<- dplyr::filter(yr_2017_2019_batting_edited, pos %in% c("1B", "LF", "SS", "CF", 
                                                                                    "2B", "3B", "C", "RF", "OF", "DH"))


pos_filtered_batting_17_19 <- pos_filtered_batting_17_19 %>%
        filter(meanG > 81)


ggplot(pos_filtered_batting_17_19, aes(meanRBI, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean RBI from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average RBI vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanHR, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Home Runs from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Home Runs vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanH, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hits from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Hits vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(mean2B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Doubles from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Doubles vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanBB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Walks from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Walks vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanGIDP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Grounding into DP from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Grounding into DP vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanHBP, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Hit by Pitch Occurences from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Hit by Pitch Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanSO, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Strikeouts from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Strikeouts vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanSB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Stolen Bases from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Stolen Bases vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanCS, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Caught Stealing Occurences from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Caught Stealing Occurences vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(mean3B, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Triples from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Triples vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanG, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean Games from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average Games vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

ggplot(pos_filtered_batting_17_19, aes(meanAB, (salary_2016/1e6))) + geom_point() + 
        scale_x_continuous("Mean At Bats from 2017-2019")+ scale_y_continuous("Salary (millions of $)") + 
        ggtitle("Latter Three-Year Average At Bats vs 2016 Salary") + theme(plot.title = element_text(hjust = 0.5)) +
        geom_smooth(method=lm, level=0.99)

batting_17_19_rbi <- cor(pos_filtered_batting_17_19$meanRBI, pos_filtered_batting_17_19$salary_2016)
batting_17_19_hr <- cor(pos_filtered_batting_17_19$meanHR, pos_filtered_batting_17_19$salary_2016)
batting_17_19_hits <- cor(pos_filtered_batting_17_19$meanH, pos_filtered_batting_17_19$salary_2016)
batting_17_19_dbls <- cor(pos_filtered_batting_17_19$mean2B, pos_filtered_batting_17_19$salary_2016)
batting_17_19_walks <- cor(pos_filtered_batting_17_19$meanBB, pos_filtered_batting_17_19$salary_2016)
batting_17_19_gidp <- cor(pos_filtered_batting_17_19$meanGIDP, pos_filtered_batting_17_19$salary_2016)
batting_17_19_hbp <- cor(pos_filtered_batting_17_19$meanHBP, pos_filtered_batting_17_19$salary_2016)
batting_17_19_so <- cor(pos_filtered_batting_17_19$meanSO, pos_filtered_batting_17_19$salary_2016)
batting_17_19_sb <- cor(pos_filtered_batting_17_19$meanSB, pos_filtered_batting_17_19$salary_2016)
batting_17_19_cs <- cor(pos_filtered_batting_17_19$meanCS, pos_filtered_batting_17_19$salary_2016)
batting_17_19_triples <- cor(pos_filtered_batting_17_19$mean3B, pos_filtered_batting_17_19$salary_2016)
batting_17_19_games <- cor(pos_filtered_batting_17_19$meanG, pos_filtered_batting_17_19$salary_2016)
batting_17_19_ab <- cor(pos_filtered_batting_17_19$meanAB, pos_filtered_batting_17_19$salary_2016)

#t-test information

mean_salary_2016 <- mean(pos_filtered_batting$salary_2016)
sd_salary_2016 <- sd(pos_filtered_batting$salary_2016)

#Focusing on players who played 2013-2019

playerID_merge <- pos_filtered_batting_13_15 %>%
        left_join(pos_filtered_batting_17_19, by = "playerID")

playerID_merge_na<- playerID_merge

table(is.na(playerID_merge_na$pos.y))
test_merge <- subset(playerID_merge_na, is.na(pos.y))
data_merge <- subset(playerID_merge_na, !is.na(pos.y))


playerID_merge_13_15 <- data_merge %>%
  select(-meanG.y, -meanAB.y, -meanH.y, -mean2B.y, -mean3B.y, -meanHR.y, -meanRBI.y, -meanSB.y, 
         -meanCS.y, -meanBB.y, -meanSO.y, -meanHBP.y, -meanGIDP.y, -pos.y, -salary_2016.y)


rbi_change_previous_latter <- data_merge$meanRBI.y - data_merge$meanRBI.x
H_change_previous_latter <- data_merge$meanH.y - data_merge$meanH.x
BB_change_previous_latter <- data_merge$meanBB.y - data_merge$meanBB.x
HR_change_previous_latter <- data_merge$meanHR.y - data_merge$meanHR.x


standardized_salary <- (data_merge$salary_2016.x - mean_salary_2016) / (sd_salary_2016)

finalized_batting_stats_change_df <- data.frame(data_merge$playerID, data_merge$salary_2016.y, rbi_change_previous_latter,
                                      H_change_previous_latter, BB_change_previous_latter, HR_change_previous_latter,
                                      standardized_salary)


salary_above_mean <- finalized_batting_stats_change_df%>%
        filter(standardized_salary > 0)

salary_below_mean <- finalized_batting_stats_change_df %>%
        filter(standardized_salary < 0)


hist(salary_above_mean$rbi_change_previous_latter, main="Players with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in RBI", col="blue") 

hist(salary_below_mean$rbi_change_previous_latter, main="Players with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in RBI", col="blue") 

hist(salary_above_mean$H_change_previous_latter, main="Players with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Hits", col="blue") 

hist(salary_below_mean$H_change_previous_latter, main="Players with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Hits", col="blue") 

hist(salary_above_mean$BB_change_previous_latter, main="Players with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Walks", col="blue") 

hist(salary_below_mean$BB_change_previous_latter, main="Players with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Walks", col="blue") 

hist(salary_above_mean$HR_change_previous_latter, main="Players with Salary Above the Average in 2016",
     ylab="Count", xlab="Change in Home Runs", col="blue") 

hist(salary_below_mean$HR_change_previous_latter, main="Players with Salary Below the Average in 2016",
     ylab="Count", xlab="Change in Home Runs", col="blue") 

avg_salary_above_mean_rbi <- mean(salary_above_mean$rbi_change_previous_latter)
avg_salary_below_mean_rbi <- mean(salary_below_mean$rbi_change_previous_latter)

sd_salary_above_mean_rbi <- sd(salary_above_mean$rbi_change_previous_latter)
sd_salary_below_mean_rbi <- sd(salary_below_mean$rbi_change_previous_latter)

avg_salary_above_mean_H <- mean(salary_above_mean$H_change_previous_latter)
avg_salary_below_mean_H <- mean(salary_below_mean$H_change_previous_latter)

sd_salary_above_mean_H <- sd(salary_above_mean$H_change_previous_latter)
sd_salary_below_mean_H <- sd(salary_below_mean$H_change_previous_latter)

avg_salary_above_mean_BB <- mean(salary_above_mean$BB_change_previous_latter)
avg_salary_below_mean_BB <- mean(salary_below_mean$BB_change_previous_latter)

sd_salary_above_mean_BB <- sd(salary_above_mean$BB_change_previous_latter)
sd_salary_below_mean_BB <- sd(salary_below_mean$BB_change_previous_latter)

avg_salary_above_mean_HR <- mean(salary_above_mean$HR_change_previous_latter)
avg_salary_below_mean_HR <- mean(salary_below_mean$HR_change_previous_latter)

sd_salary_above_mean_HR <- sd(salary_above_mean$HR_change_previous_latter)
sd_salary_below_mean_HR <- sd(salary_below_mean$HR_change_previous_latter)


std_error_rbi = sqrt(((sd_salary_above_mean_rbi**2)/76) + ((sd_salary_below_mean_rbi**2)/78))
t_statistic_rbi <- (avg_salary_above_mean_rbi - avg_salary_below_mean_rbi) / (std_error_rbi)

std_error_H = sqrt(((sd_salary_above_mean_H**2)/76) + ((sd_salary_below_mean_H**2)/78))
t_statistic_H <- (avg_salary_above_mean_H - avg_salary_below_mean_H) / (std_error_H)

std_error_BB = sqrt(((sd_salary_above_mean_BB**2)/76) + ((sd_salary_below_mean_BB**2)/78))
t_statistic_BB <- (avg_salary_above_mean_BB - avg_salary_below_mean_BB) / (std_error_BB)

std_error_HR = sqrt(((sd_salary_above_mean_HR**2)/76) + ((sd_salary_below_mean_HR**2)/78))
t_statistic_HR <- (avg_salary_above_mean_HR - avg_salary_below_mean_HR) / (std_error_HR)


#machine learning

testing_set_13_15 <- playerID_merge_13_15

mean_salary_test <-mean(testing_set_13_15$salary_2016.x)
sd_salary_test <-sd(testing_set_13_15$salary_2016.x)


standardized_salary_test <- (testing_set_13_15$salary_2016.x - mean_salary_test) / (sd_salary_test)

testing_set_13_15$labels <- as.integer(standardized_salary_test > 0)
trainIndex=createDataPartition(testing_set_13_15$labels, p=0.7)

testing_set_13_15 <- testing_set_13_15 %>%
        select(-playerID, -pos.x, -meanG.x, -meanAB.x, -mean2B.x, -mean3B.x, -meanSB.x, 
               -meanCS.x, -meanHBP.x, -meanGIDP.x, -salary_2016.x) %>%
        ungroup()

testing_set_13_15 <- testing_set_13_15 %>%
        mutate(standardized_salary_test = standardized_salary_test)
  



testing_set_13_15$labels <- as.character(testing_set_13_15$labels)
testing_set_13_15$labels <- as.numeric(testing_set_13_15$labels)

testing_set_13_15 <- testing_set_13_15[sample(nrow(testing_set_13_15)),]
testing_13_15_train <- testing_set_13_15[1:120,]
testing_13_15_test <- testing_set_13_15[120:154,]

testing_13_15_test$labels <- as.factor(testing_13_15_test$labels)
testing_13_15_train$labels <- as.factor(testing_13_15_train$labels)
nb <- naivebayes::naive_bayes(labels ~ ., data = testing_13_15_train)
plot(nb)

batting.output <- cbind(testing_13_15_test, pred = predict(nb, testing_13_15_test))

pred <- predict(nb, newdata = select(testing_13_15_test,-labels))


caret::confusionMatrix(batting.output$pred, batting.output$labels)


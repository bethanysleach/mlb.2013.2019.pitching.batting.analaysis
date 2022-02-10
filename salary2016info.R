teamSalary <- Salaries %>%
        filter(yearID == 2016) %>%
        group_by(lgID, teamID, yearID) %>%
        summarize(Salary = sum(as.numeric(salary))) %>%
        group_by(yearID, lgID) %>%
        arrange(desc(Salary))

View(teamSalary)
View(Salaries)
individual_salary <- Salaries %>%
        filter(yearID==2016) %>%
        group_by(teamID)
View(individual_salary)

hist(teamSalary$Salary/1e6, main="2016 Distribution of MLB Salaries by Team",
     ylab="Count", xlab="Team Salary in Millions of $", col="green") 





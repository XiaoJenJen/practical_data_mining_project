summary(accidents)
summary(acc_factor)
ggplot(acc_factor, aes(Accident_Severity))+stat_count()+facet_grid(Sex_of_Driver~Age_Band_of_Driver)
ggplot(acc_factor, aes(Accident_Severity))+stat_count()+facet_grid(.~Driver_age_sex)

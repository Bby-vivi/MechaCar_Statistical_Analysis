# Part 1
library(dplyr)
mpg<- read.csv('MechaCar_mpg.csv', stringsAsFactors = F) 
lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data=mpg) 
summary(lm(mpg ~ vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data=mpg)) 


# Part 2
suspension<- read.csv("Suspension_Coil.csv")
head(suspension)
total_summary<- suspension %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups='keep')

lot_summary<- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI), Vehicles=n(),.groups='keep')

# Part 3
t.test((suspension$PSI), mu=1500) 


lot1<-suspension %>% filter(Manufacturing_Lot == "Lot1")
lot2<-suspension %>% filter(Manufacturing_Lot == "Lot2")
lot3<-suspension %>% filter(Manufacturing_Lot == "Lot3")


t.test(lot1$PSI,mu=1500) 
t.test(lot2$PSI, mu = 1500) 
t.test(lot3$PSI, mu=1500) 
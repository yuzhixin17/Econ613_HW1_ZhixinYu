############################# R Script for Econ613_HW#1 #############################
############################# Jan 23,2019 #############################

# import dataset datastu
datstu <- read.csv(choose.files(), header = T, na.strings = c("","NA") , stringsAsFactors = F)

######################## Exercise 1 ########################

# Question 1 #
# calculate the total number of students
length(unique(datstu$X))

# Question 2 #
# calculate the number of schools
# create a vector combining all schoolcode columns
c(datstu$schoolcode1,datstu$schoolcode2,datstu$schoolcode3,datstu$schoolcode4,datstu$schoolcode5,datstu$schoolcode6)
school_all <- c(datstu$schoolcode1,datstu$schoolcode2,datstu$schoolcode3,datstu$schoolcode4,datstu$schoolcode5,datstu$schoolcode6)
# omit NA values
school <- na.omit(school_all)
# unique schoolcode and then report the result
length(unique(school))

# Question 3 #
# report the number of programs
# create a vector combining all choicepgm columns
c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)
program_all <- c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)
# omit NA values
program <- na.omit(program_all)
# unique choicepgm and then report the result
length(unique(program))

# Question 4 #
# report number of choices
# combine schoolcode and choicepgm
choice_all <- cbind(program_all,school_all)
# omit NA values
choice <- na.omit(choice_all)
# unique choice and then report the result
nrow(unique(choice))

# Question 5 #
# report missing test score
sum(is.na(datstu$score))

# Question 6 #
# report #of students who applyed to the same school(different program)
# extract 6 choices of students respctively
stu_choice1 <- cbind(datstu$X,datstu$schoolcode1,datstu$choicepgm1)
stu_choice2 <- cbind(datstu$X,datstu$schoolcode2,datstu$choicepgm2)
stu_choice3 <- cbind(datstu$X,datstu$schoolcode3,datstu$choicepgm3)
stu_choice4 <- cbind(datstu$X,datstu$schoolcode4,datstu$choicepgm4)
stu_choice5 <- cbind(datstu$X,datstu$schoolcode5,datstu$choicepgm5)
stu_choice6 <- cbind(datstu$X,datstu$schoolcode6,datstu$choicepgm6)
# form a matrix that contains all students' choices.
stu_choice_all <- rbind(stu_choice1,stu_choice2,stu_choice3,stu_choice4,stu_choice5,stu_choice6)
# exclude the cases that some students applying to a school more than once
# unique school by school column
stu_school_unique <- unique(stu_choice_all[,c(1,2)])
# calculate the number of students applying to the same school
table(stu_school_unique[,2])
# create a data frame saving the result
E1Q6 <- as.data.frame(table(stu_school_unique[,2]))
# change column names
colnames(E1Q6) <-c("schoolcode","number of students applied") 
# save result as csv file
write.csv(E1Q6,"E1Q6.csv")

# Question 7 #
# omit na values in stu_choice_all
stu_choice <- na.omit(stu_choice_all)
# exclude duplicates within students' choices
stu_choice_unique <- unique(stu_choice[,c(1:3)])
# find the number of choices for each individual
apply_6 <- as.matrix(table(stu_choice_unique[,1]))
# find the frequency of students' number of choices
fre_apply_6 <- as.matrix(table(apply_6))
# calculate the total number of students who applied to exactly 6 choices
sum(fre_apply_6[6])
# calculate the total number of students who applied to less than 6 choices
length(unique(datstu$X))-sum(fre_apply_6[6])


######################## Exercise 2 ########################

# import datasets datsss
datsss <- read.csv(choose.files(), header = T, na.strings = c("","NA") , stringsAsFactors = F)
# remove duplicates rows in this dataset by schoolcode column
datsss_unique <- datsss[!duplicated(datsss[c("schoolcode")]),]
# remove column of X
datsss_unique1 <- subset(datsss_unique[,-1])
# change columns' names
colnames(stu_choice_all) <- c("stu_id","schoolcode","program")

# form a dataset containing admitted students' information based on every rankplace, respectively
# select columns of "stu_id""score""schoolcode""choicepgm""rankplace=1-6"
stu_admitby1 <- subset(datstu[,c(1:2,5,11,18)],rankplace=="1")
stu_admitby2 <- subset(datstu[,c(1:2,6,12,18)],rankplace=="2")
stu_admitby3 <- subset(datstu[,c(1:2,7,13,18)],rankplace=="3")
stu_admitby4 <- subset(datstu[,c(1:2,8,14,18)],rankplace=="4")
stu_admitby5 <- subset(datstu[,c(1:2,9,15,18)],rankplace=="5")
stu_admitby6 <- subset(datstu[,c(1:2,10,16,18)],rankplace=="6")
# change schoolcode&choicepgm columns' names
colnames(stu_admitby1)[3] <- "schoolcode"
colnames(stu_admitby1)[4] <- "choicepgm"
colnames(stu_admitby2)[3] <- "schoolcode"
colnames(stu_admitby2)[4] <- "choicepgm"
colnames(stu_admitby3)[3] <- "schoolcode"
colnames(stu_admitby3)[4] <- "choicepgm"
colnames(stu_admitby4)[3] <- "schoolcode"
colnames(stu_admitby4)[4] <- "choicepgm"
colnames(stu_admitby5)[3] <- "schoolcode"
colnames(stu_admitby5)[4] <- "choicepgm"
colnames(stu_admitby6)[3] <- "schoolcode"
colnames(stu_admitby6)[4] <- "choicepgm"
# form a matrix that contains all admitted students' information
stu_admit <- rbind(stu_admitby1,stu_admitby2,stu_admitby3,stu_admitby4,stu_admitby5,stu_admitby6)

# Cutoff #
# calculate the minimum score for each choice
cutoff <- aggregate(stu_admit$score, list(stu_admit$schoolcode, stu_admit$choicepgm), min)
# change columns' names
colnames(cutoff) <- c("schoolcode","choicepgm","cutoff")

# Quality #
# calculate the average score for each choice
quality <- aggregate(stu_admit$score, list(stu_admit$schoolcode, stu_admit$choicepgm), mean)
# change columns' names
colnames(quality) <- c("schoolcode","choicepgm","quality")

# Size #
# calculate the total number of students being admitted for each choice
size <- as.data.frame(table(stu_admit$schoolcode,stu_admit$choicepgm))
# change columns' names
colnames(size) <- c("schoolcode","choicepgm","size")

# merge matrix datsss_unique1, cutoff, quality, size
school_level_data <- merge(datsss_unique1,cutoff,by="schoolcode")
school_level_data1 <- merge(school_level_data,quality,by=c("schoolcode","choicepgm"))
school_level_data2 <- merge(school_level_data1,size,by=c("schoolcode","choicepgm"))

# save Exercise2 result as "Exercise2.csv"
write.csv(school_level_data2, "Exercise2.csv")


######################## Exercise 3 ########################

# change columns' names in order to match same information among multiple tables
colnames(datstu)[1]<- "stu_id"
colnames(stu_school_unique) <- c("stu_id","schoolcode")
# form a matrix contains stu_id, students' junior school district and senior schoolcode by which students was admitted
stu_sss_jss <- merge(datstu[,c(1,17)],stu_school_unique,by=c("stu_id"))
# import dataset datjss
datjss <- read.csv(choose.files(), header = T, na.strings = c("","NA") , stringsAsFactors = F)
# change column names
colnames(datjss) <- c("stu_id","jssdistrict","jsslong","jsslat")
# add information of senior school longtitude and latitude based on schoolcode
stu_sssd_jss <- merge(stu_sss_jss,datsss_unique1[,c(2,4,5)],by="schoolcode")
# add information of junior school longtitude and latitude based on jssdistrict
stu_sssd_jssd <- merge(stu_sssd_jss,datjss[,c(2:4)],by="jssdistrict")
# calculate distance for each individuals' choices and then save data as matrix
distance <- as.matrix(with(stu_sssd_jssd, sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2+(69.172*(ssslat-jsslat))^2)))
# change column name
colnames(distance) <- "distance"
# add distance information
stu_distance <- cbind(stu_sssd_jssd,distance)
# extract student_id and distance information
Exercise3 <- subset(stu_distance[,c(3,8)])
# save Exercise3 result as "Exercise3.csv"
write.csv(Exercise3, "Exercise3.csv")


######################## Exercise 4 ########################

## Question 1 ##
## report the average and sd of cutoff&quality&distance for each ranked choice ##
# creat a data frame contains information of "stu_id""choice""cutoff""quality""distance""rankplace"
colnames(stu_admit)[1] <- "stu_id"
Exercise4 <- merge(stu_admit[,c(1,3:5)], school_level_data2[,c(1:2,7:8)], by=c("schoolcode","choicepgm"))
Exercise4.1 <- merge(Exercise4[,c(1:6)], stu_distance[,c(2:3,8)], by=c("stu_id","schoolcode"))

# calculate mean and sd for rankplace 1-6 #
# extract students' information if rankplace equals to 1
E4_rank1 <- subset(Exercise4.1, rankplace=="1")
# calculate average cutoff, average quality and average distance
rank1.mean <- as.data.frame(lapply(E4_rank1[4:7], mean))
# calculate sd of cutoff,quality and distance
rank1.sd <- as.data.frame(lapply(E4_rank1[5:7], sd))
# change column name
colnames(rank1.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank1.sd) <- c("cutoff_sd","quality_sd","distance_sd")
# combine average and sd result for rankplace=1
rank1 <- cbind(rank1.mean, rank1.sd)

# same steps below when rankplace equals to 2,3,4,5,6, respectively
E4_rank2 <- subset(Exercise4.1, rankplace=="2")
rank2.mean <- as.data.frame(lapply(E4_rank2[4:7], mean))
rank2.sd <- as.data.frame(lapply(E4_rank2[5:7], sd))
colnames(rank2.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank2.sd) <- c("cutoff_sd","quality_sd","distance_sd")
rank2 <- cbind(rank2.mean, rank2.sd)

E4_rank3 <- subset(Exercise4.1, rankplace=="3")
rank3.mean <- as.data.frame(lapply(E4_rank3[4:7], mean))
rank3.sd <- as.data.frame(lapply(E4_rank3[5:7], sd))
colnames(rank3.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank3.sd) <- c("cutoff_sd","quality_sd","distance_sd")
rank3 <- cbind(rank3.mean, rank3.sd)

E4_rank4 <- subset(Exercise4.1, rankplace=="4")
rank4.mean <- as.data.frame(lapply(E4_rank4[4:7], mean))
rank4.sd <- as.data.frame(lapply(E4_rank4[5:7], sd))
colnames(rank4.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank4.sd) <- c("cutoff_sd","quality_sd","distance_sd")
rank4 <- cbind(rank4.mean, rank4.sd)

E4_rank5 <- subset(Exercise4.1, rankplace=="5")
rank5.mean <- as.data.frame(lapply(E4_rank5[4:7], mean))
rank5.sd <- as.data.frame(lapply(E4_rank5[5:7], sd))
colnames(rank5.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank5.sd) <- c("cutoff_sd","quality_sd","distance_sd")
rank5 <- cbind(rank5.mean, rank5.sd)

E4_rank6 <- subset(Exercise4.1, rankplace=="6")
rank6.mean <- as.data.frame(lapply(E4_rank6[4:7], mean))
rank6.sd <- as.data.frame(lapply(E4_rank6[5:7], sd))
colnames(rank6.mean) <- c("rankplace","cutoff_average","quality_average","distance_average")
colnames(rank6.sd) <- c("cutoff_sd","quality_sd","distance_sd")
rank6 <- cbind(rank6.mean, rank6.sd)

# combine each rankplace result
rank <- rbind(rank1, rank2, rank3, rank4, rank5, rank6)

# save Exercise4_Question1 reslut as "E4Q1.csv"
write.csv(rank, "E4Q1.csv")

## Question 2 ##
## report the average and sd of cutoff&quality&distance differentiating by student test score quantiles ##
# add test score information
Exercise4.3 <- merge(Exercise4.1[,c(-4)], stu_admit[,c(1:2)], by=c("stu_id"))

# calculate quartile for student test score
library(dplyr)
Exercise4.3$quartile <- ntile(Exercise4.3$score, 4) 

# same logic as Question1
# calculate average and sd of cutoff,quality and distance when quartile equals to 4,3,2,1, respectively
E4_quartile4 <- subset(Exercise4.3, quartile=="4")
quartile4.mean <- as.data.frame(lapply(E4_quartile4[c(4:6,8)], mean))
quartile4.sd <- as.data.frame(lapply(E4_quartile4[4:6], sd))
colnames(quartile4.mean) <- c("cutoff_average","quality_average","distance_average","quartile")
colnames(quartile4.sd) <- c("cutoff_sd","quality_sd","distance_sd")
quartile4 <- cbind(quartile4.mean,quartile4.sd)

E4_quartile3 <- subset(Exercise4.3, quartile=="3")
quartile3.mean <- as.data.frame(lapply(E4_quartile3[c(4:6,8)], mean))
quartile3.sd <- as.data.frame(lapply(E4_quartile3[4:6], sd))
colnames(quartile3.mean) <- c("cutoff_average","quality_average","distance_average","quartile")
colnames(quartile3.sd) <- c("cutoff_sd","quality_sd","distance_sd")
quartile3 <- cbind(quartile3.mean,quartile3.sd)

E4_quartile2 <- subset(Exercise4.3, quartile=="2")
quartile2.mean <- as.data.frame(lapply(E4_quartile2[c(4:6,8)], mean))
quartile2.sd <- as.data.frame(lapply(E4_quartile2[4:6], sd))
colnames(quartile2.mean) <- c("cutoff_average","quality_average","distance_average","quartile")
colnames(quartile2.sd) <- c("cutoff_sd","quality_sd","distance_sd")
quartile2 <- cbind(quartile2.mean,quartile2.sd)

E4_quartile1 <- subset(Exercise4.3, quartile=="1")
quartile1.mean <- as.data.frame(lapply(E4_quartile1[c(4:6,8)], mean))
quartile1.sd <- as.data.frame(lapply(E4_quartile1[4:6], sd))
colnames(quartile1.mean) <- c("cutoff_average","quality_average","distance_average","quartile")
colnames(quartile1.sd) <- c("cutoff_sd","quality_sd","distance_sd")
quartile1 <- cbind(quartile1.mean,quartile1.sd)

# combine average and sd information for all quartile
quartile <- rbind(quartile4,quartile3,quartile2,quartile1)
# reoder columns
quartile <- quartile[,c(4,1,2,3,5,6,7)]
# save Exercise4_Question2 reslut as "E4Q2.csv"
write.csv(quartile, "E4Q2.csv")

######################## Exercise 5 ########################

## Question1 ##
# calculate decile of selectibity(cutoffs)
library(dplyr)
cutoff$decile <- ntile(cutoff$cutoff, 10)
# change column name of stu_choice_all
colnames(stu_choice_all)[3] <- "choicepgm"
# combine information of students choices and cutoffs based on schoolcode and choicepgm
selectivity <- merge(stu_choice_all[,1:3], cutoff[,c(1:2,4)],by=c("schoolcode","choicepgm"))
# remove duplicated rows
selectivity_unique <- selectivity[!duplicated(selectivity[c("stu_id","decile")]),]
# report the frequency of decile based on student id
E5Q1 <- as.data.frame(table(selectivity_unique$stu_id))
# change column names
colnames(E5Q1) <- c("stu_id","number of decile groups")
# save Exercise5_Question1 reslut as "E5Q1.csv"
write.csv(E5Q1, "E5Q1.csv")

## Question2 ##
# calculate quartile of quality
library(dplyr)
quality$quartile <- ntile(cutoff$cutoff, 4) 
# combine information of students choices and qualitys based on schoolcode and choicepgm
quality_quartile <- merge(stu_choice_all[,1:3], quality[,c(1:2,4)],by=c("schoolcode","choicepgm"))
# remove duplicated rows
quality_q_unique <- quality_quartile[!duplicated(quality_quartile[c("stu_id","quartile")]),]
# report the frequency of quartile based on student id
E5Q2 <- as.data.frame(table(quality_q_unique$stu_id))
# change column names
colnames(E5Q2) <- c("stu_id","number of quartile quality")
# save Exercise5_Question2 reslut as "E5Q2.csv"
write.csv(E5Q2, "E5Q2.csv")

########## Thank you for reading! Feel free making comments! Have a good day! ##########


##################################### The End ###############################################



















library(tidyverse)
library(lubridate)
library(chron)
library(rebus)
library(stringr)
library(corrplot)
library(ggpubr)
library(ggthemes)
library(scales)

temp <- ba431 %>%
  group(Eventcontext) %>%
  summarise
#base classes
com101 <- logs_COM101_A1_COR220C_A1_GIS295_A1A1_2020FALLMain_20210226_0953  # 25 studnets T/TR 2:30 - 3:50
psy101 <- logs_COR220PW_B1_PSY101_B1B1_2020FALLMain_20210226_0959           # 25 students M/W  2:00 - 3:50
eng101 <- logs_ENG101Web_2020FALL01_20210225_1519                           # 2 students ? online
mth101 <- logs_MTH101A1_2020FALLMain_20210226_1001                          # 24 students T/TR 2:30 - 3:50
art157 <- logs_ART157_WEB_COR240A_WEBWEB_2021SPR01_20210226_0951            # 22 students online

#plugin classes
ge206 <- logs_GE_206A2_2021SPRMain_20210318_0944 #7 students MWF 9:00 - 9:50
ce435 <- logs_CE_435_A2_MCE535_A2A2_2021SPRMain_20210318_0947 # 8 students W 5:00 - 7:50

cor120 <- logs_COR120C2_2021SPRMain_20210318_0949 #14 students T/TR 12:30 - 2:20
eng210 <- logs_COR210YW_A2_ENG210_B2A2_2021SPRMain_20210318_0951 # 20 students MW 2:00 - 3:50

ba420 <- logs_BA_420A2_2021SPRMain_20210318_0959 # 23 students arranged
ba330 <- logs_BA_330A2_2021SPRMain_20210318_0954 # 25 students MWF 10:00 - 10:50
ba431 <- logs_BA_431A2_2021SPRMain_20210318_0957 # 13 students MWF 2:00 - 2:50
ba344 <- logs_BA_344A2_2021SPRMain_20210318_0955 # 17 students MWF 9:00 - 9:50

clean <- function(dataframe) {
  
  dataframe <- dataframe %>%
    rename(Eventcontext = `Event context`,
           Eventname = `Event name`,
             ) 
  
  dataframe$Time <- as.POSIXct(dataframe$Time, format = "%m/%d/%Y, %H:%M")
  dataframe$roundedtime <- round_date(dataframe$Time,unit="hour")
  
  dataframe$month = as.numeric(format(dataframe$roundedtime, "%m"))
  dataframe$day = as.numeric(format(dataframe$roundedtime, "%d"))
  dataframe$year = as.numeric(format(dataframe$roundedtime, "%Y"))
  dataframe$roundedtime = format(dataframe$roundedtime, "%H:%M")
  
  dataframe$weekday <- wday(dataframe$Time, label=TRUE)
  dataframe$week <- epiweek(dataframe$Time) #new week starts sunday
  
  dataframe$userid <- str_extract(dataframe$Description, pattern = 
                                    one_or_more("'") %R%
                                    capture(one_or_more(DGT)) %R%
                                    one_or_more("'")
  )
  
  dataframe$userid <- gsub("'", "", dataframe$userid)
  
  return(dataframe)
}

com101 <- clean(com101)
com101$classname <- "com101"
com101$testgroup <- 0
com101$numStudents <- 25


psy101 <- clean(psy101)
psy101$classname <- "psy101"
psy101$testgroup <- 0
psy101$numStudents <- 25


eng101 <- clean(eng101)
eng101$classname <- "eng101"
eng101$testgroup <- 0
eng101$numStudents <- 2


mth101 <- clean(mth101)
mth101$classname <- "mth101"
mth101$testgroup <- 0
mth101$numStudents <- 24


art157 <- clean(art157)
art157$classname <- "art157"

art157$testgroup <- 0
art157$numStudents <- 22
#############
ge206 <- clean(ge206)
ge206$classname <- "ge206"
ge206$testgroup <- 1
ge206$numStudents <- 7

ce435 <- clean(ce435)
ce435$classname <- "ce435"
ce435$testgroup <- 1
ce435$numStudents <- 8
#############
cor120 <- clean(cor120)
cor120$classname <- "cor120"
cor120$testgroup <- 1
cor120$numStudents <- 14

eng210 <- clean(eng210)
eng210$classname <- "eng210"
eng210$testgroup <- 1
eng210$numStudents <- 20

#############
ba420 <- clean(ba420)
ba420$classname <- "ba420"
ba420$testgroup <- 1
ba420$numStudents <- 23

ba330 <- clean(ba330)
ba330$classname <- "ba330"
ba330$testgroup <- 1
ba330$numStudents <- 25

ba431 <- clean(ba431)
ba431$classname <- "ba431"
ba431$testgroup <- 1
ba431$numStudents <- 13

ba344 <- clean(ba344)
ba344$classname <- "ba344"
ba344$testgroup <- 1
ba344$numStudents <- 17

alldata <- bind_rows(
  com101,
  psy101,
  eng101,
  mth101,
  art157,
  ge206,
  ce435,
  cor120,
  eng210,
  ba420,
  ba330,
  ba431,
  ba344
)

temp <- alldata %>%
  filter(Eventcontext == "Other" & Eventname == "Course module viewed")

classlist <- c(
  com101,
  psy101,
  eng101,
  mth101,
  art157,
  ge206,
  ce435,
  cor120,
  eng210,
  ba420,
  ba330,
  ba431,
  ba344
)

#############

  
##############################
# old standalone cleaning code #
##############################

# temp <- eng101 
# 
# temp <- clean(temp)

# art157$Time <- as.POSIXct(art157$Time, format = "%m/%d/%Y, %H:%M")
# art157$roundedtime <- round_date(art157$Time,unit="hour")
# 
# art157$month = as.numeric(format(art157$roundedtime, "%m"))
# art157$day = as.numeric(format(art157$roundedtime, "%d"))
# art157$year = as.numeric(format(art157$roundedtime, "%Y"))
# art157$roundedtime = format(art157$roundedtime, "%H:%M")
# 
# art157$weekday <- wday(art157$Time, label=TRUE)
# art157$week <- epiweek(art157$Time) #new week starts sunday
# 
# art157$userid <- str_extract(art157$Description, pattern = 
#                              one_or_more("'") %R%
#                              capture(one_or_more(DGT)) %R%
#                              one_or_more("'")
# )
# 
# art157$userid <- gsub("'", "", art157$userid)

#############################
#############################



temp2 <- art157 %>%
  filter(Eventname %in% c("Course viewed", "Quiz attempt started", "A submission has been submitted."))


ggplot(temp2, aes(x=roundedtime, fill=Eventname)) + 
  stat_count() +
  labs(x = "Rounded Time", y = "Moodle Interactions", title = "Interactions by Time of Day") +
  facet_grid(Eventname~., scales = "free", space = "free") +
  theme_minimal()

ggplot(temp2, aes(x=weekday, fill=Eventname)) + stat_count() + labs(x = "Week", y = "Moodle Interactions", title = "Interactions by Day of Week")


##########################
# Heat map of activity
##########################
activityheatmap <- bind_rows(  com101,
                               psy101,
                               eng101,
                               mth101,
                               art157,
                               ge206,
                               ce435,
                               cor120,
                               eng210,
                               ba420,
                               ba330,
                               ba431,
                               ba344)

##most class start at 9:00 or 2:00 so data is quite skewed can be used individually tho with a facet grid, 2 rows
activityheatmap <- activityheatmap %>%
  filter(Eventname == "Course viewed") %>%
  group_by(weekday, roundedtime) %>%
  summarise(count = n())

activityheatmap <- activityheatmap %>%
  filter(Eventname == "A submission has been submitted.") %>%
  group_by(weekday, roundedtime) %>%
  summarise(count = n())

activityheatmap <- activityheatmap %>%
  filter(Eventname %in% c("Course module viewed", "Quiz attempt started", "A submission has been submitted.")) %>%
  group_by(weekday, roundedtime) %>%
  summarise(count = n())

activityheatmap <- art157 %>%
  filter(Eventname == "Course viewed") %>%
group_by(weekday, roundedtime) %>%
  summarise(count = n())

ggplot(activityheatmap, aes(roundedtime, weekday)) + geom_tile(aes(fill = count),colour = "white", na.rm = TRUE) +
  scale_fill_gradient(low = "#d8e1cf", high = "#438484", na.value = "grey50") +  
  guides(fill=guide_legend(title="Total Interactions")) +
  theme_bw() + theme_minimal() + 
  labs(title = "Moodle Interactions by Day of Week and Hour", x = "Interactions Per Hour", y = "Day of Week") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 


###########################
# Q1  #  how long does it take for students to view their feedback
###########################
#join all data, group by class, graph, have total median in a line thing on y axis

q1 <- art157 %>%
  filter(Eventname %in% c("Feedback viewed", "The submission has been graded."))

q1$tempassignmentid <- str_extract_all(q1$Description, pattern = 
                               one_or_more("'") %R%
                               capture(one_or_more(DGT)) %R%
                               one_or_more("'")
)

q1$tempassignmentid <- sapply(q1$tempassignmentid, paste, collapse = ",")
q1$tempassignmentid <- str_replace_all(q1$tempassignmentid, "'", "")
q1 <- separate(q1, tempassignmentid, into = c("c1", "c2", "c3", "c4"))

#orginal
q1 <- q1 %>%
  mutate(
    assignmentid = if_else(Eventname == "Feedback viewed", q1$c3, q1$c4),
    usergraded = if_else(Eventname == "The submission has been graded.", q1$c3, "NA")
  ) %>%
  select(-c1, -c2, -c3, -c4) %>%  
  filter(Eventname == "The submission has been graded.") %>%
  group_by(assignmentid, usergraded) %>% 
  slice(which.max(Time)) 
  #selects only the first time the instructor graded an assignment (removes grade adjustments)



q1 <- q1 %>%
  mutate(
    assignmentid = if_else(Eventname == "Feedback viewed", q1$c3, q1$c4),
    usergraded = if_else(Eventname == "The submission has been graded.", q1$c3, "NA")
  ) %>%
  select(-c1, -c2, -c3, -c4) 

q11 <- q1 %>%  
  filter(Eventname == "The submission has been graded.") %>%
  group_by(assignmentid, usergraded) %>% 
  slice(which.max(Time)) 

q12 <- q1 %>%
  filter(Eventname == "Feedback viewed")

q1 <- rbind(q11, q12)

#makes sure this joined correctly
#goal is to make sure the feedback viewed group of data has the grade grouped of data joined on the correct coloum 
#use time differene when for sure
q1 <- left_join(q11, q12, by = c('assignmentid' = 'assignmentid','usergraded' = 'userid'))

q1fun <- function(dataframe) {
  
  dataframe <- dataframe %>%
    filter(Eventname %in% c("Feedback viewed", "The submission has been graded."))
  
  dataframe$tempassignmentid <- str_extract_all(dataframe$Description, pattern = 
                                           one_or_more("'") %R%
                                           capture(one_or_more(DGT)) %R%
                                           one_or_more("'")
  )
  
  dataframe$tempassignmentid <- sapply(dataframe$tempassignmentid, paste, collapse = ",")
  dataframe$tempassignmentid <- str_replace_all(dataframe$tempassignmentid, "'", "")
  dataframe <- separate(dataframe, tempassignmentid, into = c("c1", "c2", "c3", "c4"))
  
  dataframe <- dataframe %>%
    mutate(
      assignmentid = if_else(Eventname == "Feedback viewed", dataframe$c3, dataframe$c4),
      usergraded = if_else(Eventname == "The submission has been graded.", dataframe$c3, "NA")
    ) %>%
    select(-c1, -c2, -c3, -c4) 
  
  dataframe11 <- dataframe %>%  
    filter(Eventname == "The submission has been graded.") %>%
    group_by(assignmentid, usergraded) %>% 
    slice(which.min(Time)) #only the first time an instructor graded an assignment (removes changes)
  
  dataframe12 <- dataframe %>%
    filter(Eventname == "Feedback viewed")
  
  dataframe <- rbind(dataframe11, dataframe12)
  
  #makes sure this joined correctly
  #goal is to make sure the feedback viewed group of data has the grade grouped of data joined on the correct coloum 
  #use time differene when for sure
  dataframe <- left_join(dataframe11, dataframe12, by = c('assignmentid' = 'assignmentid','usergraded' = 'userid'))
  
  #select the first time a student viewed the assignment only
  dataframe <- dataframe %>%
    group_by(assignmentid, usergraded) %>% 
    slice(which.min(Time.y)) 
  
  return(dataframe)
}

q1<- q1fun(com101)
q2<- q1fun(psy101)
q3<- q1fun(eng101)
q4<- q1fun(mth101)
q5<- q1fun(art157)
q6<- q1fun(ge206)
q7<- q1fun(ce435)
q8<- q1fun(cor120)
q9<- q1fun(eng210)
q10<- q1fun(ba420)
q11<- q1fun(ba330)
q12<- q1fun(ba431)
q13<- q1fun(ba344)

q1data <- bind_rows(q1,
                    q2,
                    q3,
                    q4,
                    q5,
                    q6,
                    q7,
                    q8,
                    q9,
                    q10,
                    q11,
                    q12,
                    q13
)

q1data <- na.omit(q1data)

#thoughts
# seems like a low number of grade, 114 grades submitted and only feedback viewed 68 times
# i assume this is because status of submisson is enough to view grade or there might not be feedback to view
# because of the uncerteinty with status of submission viewed this is still the best way to view it
# it would be impossible to check status of submission viewed bc htere is so many rows and so many time its done before an assignment is turned on

# time difference
# summary(as.duration(interval(q5$Time.x, q5$Time.y)))
# interval(q5$Time.x, q5$Time.y)
# 
# difftime(q5$Time.x, q5$Time.y, units = "days") 

temp <- q1 %>%
  mutate(timetoviewfb = difftime(Time.x, Time.y, units = "days")) 

q1datasum <- q1data %>%
  group_by(classname.x) %>%
  mutate(timetoviewfb = difftime(Time.x, Time.y, units = "days")) %>%
  summarise(med = median(timetoviewfb), avg = mean(timetoviewfb))

q1datac <- q1data %>%
  mutate(timetoviewfb = difftime(Time.x, Time.y, units = "hours")) %>%
  filter(timetoviewfb < 0) %>% #removes a few data anomalies
  mutate(timetoviewfb = timetoviewfb * -1)

ggplot(q1datac, aes(x=classname.x, y=timetoviewfb)) + geom_boxplot(alpha=0) + geom_jitter(alpha=0.3)

q1quantile <- quantile(as.numeric(q1datac$timetoviewfb), .8)

ggplot(q1datac, aes(x= timetoviewfb)) + 
  geom_histogram(binwidth = 20, colour="white") +
  geom_vline(xintercept=q1quantile, color="blue", linetype="dashed", size=1) +
  scale_x_continuous(breaks = pretty_breaks(n = 15)) +
  theme_clean() +
  geom_text(aes(x=q1quantile + 15, label="80% of Students", y=75), colour="blue", angle=90) + 
  labs(x = "Time Before Feedback was Viewed (Hours)", y = "Count of Students", title = "Distribution of How Long it Takes for Students to View Feedback")

ggplot(q1datasum, aes(x=classname.x, y = med)) + geom_col()

summary(as.numeric(q1datac$timetoviewfb))


######################
# Q2 # Moodle resources (files, assignments, forums, etc) get the greatest number of 'hits' and which get the fewest
######################

#gather views per type
q2 <- art157 

q2$Moduletype <- str_extract(q2$Eventcontext, pattern = 
                                          capture(one_or_more(WRD)) %R%
                                          ":" 
)

q2$Moduletype <- gsub(":", "", q2$Moduletype)

q2 <- q2 %>%
  filter(Eventname == 'Course module viewed') %>%
  group_by(Moduletype) %>%
  summarise(TotalModuleviews = n())



####
#gather posts per type
#course was restore so Course module created is not very helpful
#instead group by event context, get n (placeholder), then count module types 
q22 <- art157 

q22 <- q22 %>%
  group_by(Eventcontext) %>%
  summarise(count = n())

q22$Moduletype <- str_extract(q22$Eventcontext, pattern = 
                               capture(one_or_more(WRD)) %R%
                               ":" 
)

q22$Moduletype <- gsub(":", "", q22$Moduletype)

q22 <- q22 %>%
  group_by(Moduletype) %>%
  summarise(ModulesAdded = n())

####
#join

q222 <- inner_join(q2, q22, by = c("Moduletype" = "Moduletype")) %>%
  mutate(viewsperadd = TotalModuleviews / ModulesAdded, viewsperstudent = viewsperadd / 22) %>% ##22 students 
  arrange(desc(viewsperstudent))


###function version
q2fun <- function(dataframe) {
  #gather views per type
  numstudent <- dataframe$numStudents[1]
  dataframe2 <- dataframe
  
  dataframe$Moduletype <- str_extract(dataframe$Eventcontext, pattern = 
                                 capture(one_or_more(WRD)) %R%
                                 ":" 
  )
  
  dataframe$Moduletype <- gsub(":", "", dataframe$Moduletype)
  
  dataframe <- dataframe %>%
    filter(Eventname == 'Course module viewed') %>%
    group_by(Moduletype) %>%
    summarise(TotalModuleviews = n())
  
  
  
  ####
  #gather posts per type
  #course was restore so Course module created is not very helpful
  #instead group by event context, get n (placeholder), then count module types
  dataframe2 <- dataframe2 %>%
    group_by(Eventcontext) %>%
    summarise(count = n())
  
  dataframe2$Moduletype <- str_extract(dataframe2$Eventcontext, pattern = 
                                  capture(one_or_more(WRD)) %R%
                                  ":" 
  )
  
  dataframe2$Moduletype <- gsub(":", "", dataframe2$Moduletype)
  
  dataframe2 <- dataframe2 %>%
    group_by(Moduletype) %>%
    summarise(ModulesAdded = n())
  
  ####
  #join
  
  dataframe22 <- inner_join(dataframe, dataframe2, by = c("Moduletype" = "Moduletype")) %>%
    mutate(viewsperadd = TotalModuleviews / ModulesAdded, viewsperstudent = viewsperadd / numstudent) %>% ##22 students 
    arrange(desc(viewsperstudent))
  
  return(dataframe22)
}

q21<- q2fun(com101)
q22<- q2fun(psy101)
q23<- q2fun(eng101)
q24<- q2fun(mth101)
q25<- q2fun(art157)
q26<- q2fun(ge206)
q27<- q2fun(ce435)
q28<- q2fun(cor120)
q29<- q2fun(eng210)
q210<- q2fun(ba420)
q211<- q2fun(ba330)
q212<- q2fun(ba431)
q213<- q2fun(ba344)

q2data <- bind_rows(q21,
                    q22,
                    q23,
                    q24,
                    q25,
                    q26,
                    q27,
                    q28,
                    q29,
                    q210,
                    q211,
                    q212,
                    q213
)

q2data <- q2data %>%
  mutate(Moduletype = str_replace(Moduletype, "tool", "External Tool")) %>%
  filter(!is.na(Moduletype))

q2datasum <- q2data %>%
  group_by(Moduletype) %>%
  summarise(
    TotalModuleviews = mean(TotalModuleviews),
    ModulesAdded = mean(ModulesAdded),
    viewsperadd = mean(viewsperadd),
    viewsperstudent = mean(viewsperstudent)
  )

q2datalong <- q2data %>%
  pivot_longer(
    cols = c(TotalModuleviews, ModulesAdded, viewsperadd, viewsperstudent),
    names_to = "Measure",
    values_to = "Value"
  )

ggplot(q2datalong, aes(x=Moduletype, y=Value, fill=Measure)) + geom_col() +facet_wrap(~Measure)

ggplot(q2data, aes(x=Moduletype, y = TotalModuleviews)) + geom_col()



######################
# Q3 # Information about how often students view lectures would be helpful
######################

#only math posted lectures of class
q31 <- mth101 %>%
  filter(str_detect(Eventcontext, "URL: Lecture Video") & Eventname == 'Course module viewed') %>%
  group_by(Eventcontext) %>%
  summarise(Views = n()) %>%
  mutate(views)

ce435 
cor120

temp <- alldata %>%
  filter(Eventname == 'Course module viewed')

temp <- cor120 %>%
  group_by(Eventcontext) %>%
  summarise(count = n())

#####################
#perusall
#####################

#use q2 table format, views per module, views per student etc
#also use check list format

p <- cor120 %>%
  filter(Component == 'External tool' & Eventname != "Course module instance list viewed") %>%
  group_by(Eventcontext, Eventname) %>%
  summarise(count = n())

p <- eng210 %>%
  filter(Component == 'External tool' & Eventname == 'Course module viewed') %>%
  group_by(Eventcontext) %>%
  summarise(count = n())


preadingsposted <- cor120 %>%
  filter(Component == 'External tool' & Eventname != "Course module instance list viewed") %>%
  group_by(Eventcontext, Eventname) %>%
  summarise(count = n()) 

preadingsposted <- nrow(preadingsposted)

p1 <- cor120 %>%
  filter(Component == 'External tool' & Eventname == 'Course module viewed') %>%
  group_by(week, Eventcontext) %>%
  summarise(count = n())

p2 <- eng210 %>%
  filter(Component == 'External tool' & Eventname == 'Course module viewed') %>%
  group_by(Eventcontext) %>%
  summarise(count = n())



#####################
# Checklist
#####################

##check activity by date to make sure usage stayed strong
c120
eng210 

#remove 6989 for professor
# still need to answer, how many times can each checklist be expected to view
                      # how many times will a student check the checklist per week

checklist <- alldata %>%
  filter(Component == 'Checklist') %>%
  group_by(classname) %>%
  summarise(count = n())

checklist <- cor120 %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed", 
                                                       "Report viewed", 
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated"))) %>% #edit page and report are done by instructor only
  group_by(week, Eventname) %>%
  summarise(count = n())

checklist2 <- eng210 %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed",
                                                       "Report viewed",
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated" ))) %>% #edit page and report are done by instructor only
  group_by(week, Eventname) %>%
  summarise(count = n())

checklist3 <- bind_rows(eng210, cor120) %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed",
                                                       "Report viewed",
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated"))) %>% #edit page and report are done by instructor only
  group_by(userid) %>%
  summarise(count = n())

checklist31 <- eng210 %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed",
                                                       "Report viewed",
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated"))) %>% #edit page and report are done by instructor only
  group_by(userid) %>%
  summarise(count = n())

checklist32 <- cor120 %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed",
                                                       "Report viewed",
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated"))) %>% #edit page and report are done by instructor only
  group_by(userid) %>%
  summarise(count = n())

checklist3 <- bind_rows(checklist31, checklist32)

checklist4 <- bind_rows(eng210, cor120) %>%
  filter(Component == 'Checklist' & userid != 6989 & !(Eventname %in% c("Edit page viewed",
                                                       "Report viewed",
                                                       "Course module instance list viewed",
                                                       "Checklist complete",
                                                       "Teacher checks updated"))) %>%
  group_by(week, userid) %>%
  summarise(count = n()) 

  

checklistdate <- cor120 %>%
  filter(Component == 'Checklist') %>%
  group_by(week) %>%
  summarise(count = n())

#usage over time
ggplot(checklistdate, aes(x=Time, y=count)) + 
  geom_point() 

#last checklist was posted week 9 which explains drop at the end -- students actually went back and checked off old check lists last 2 weeks
ggplot(checklist, aes(x=week, y = count, color = Eventname)) + 
  geom_line()
#high usage throughout
ggplot(checklist2, aes(x=week, y = count, color = Eventname)) + 
  geom_line() 
    
#over half used a lot, most used a little at least -- instructor has 2 dots
ggplot(checklist3, aes(x=reorder(userid, count), y = count)) + 
  geom_point(color = "red", size = 4) + 
  coord_flip() + 
  theme_minimal() +
  labs(x="Student", y = "Count", title = "Number of Checklist Views + Checks Made by Each Student")

intersect(cor120$userid, eng210$userid)

#activity did trend down but not by a lot
ggplot(checklist4, aes(x=week, y=count)) + geom_jitter(width = 0.1, alpha=0.5) + geom_smooth(se = FALSE, method = lm) + 
  stat_cor(label.y = 35) 

ggplot(checklist4, aes(x=week, y=count)) + geom_line() + geom_smooth(se = FALSE, method = lm) + 
  stat_cor(label.y = 35) +facet_wrap(~userid) 

slope(checklist4$week, checklist4$count)



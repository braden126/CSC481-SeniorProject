library(tidyverse)
library(lubridate)
library(chron)
library(rebus)
library(stringr)
library(corrplot)
library(ggpubr)
library(ggthemes)

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



#thoughts
# seems like a low number of grade, 114 grades submitted and only feedback viewed 68 times
# i assume this is because status of submisson is enough to view grade or there might not be feedback to view
# because of the uncerteinty with status of submission viewed this is still the best way to view it
# it would be impossible to check status of submission viewed bc htere is so many rows and so many time its done before an assignment is turned on

# time difference
summary(as.duration(interval(q1$Time.x, q1$Time.y)))


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

######################
# Q3 # Information about how often students view lectures would be helpful
######################

#only math posted lectures of class
q3 <- mth101 %>%
  filter(str_detect(Eventcontext, "URL: Lecture Video") & Eventname == 'Course module viewed') %>%
  group_by(Eventcontext) %>%
  summarise(Views = n()) %>%
  mutate(views)




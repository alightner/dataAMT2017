library(readr)
library(readxl)
library(dplyr)
library(devtools)
library(tidyverse)

c <- read_csv("raw_data/ultimatum_windfall_vignette (accessed 2017-08-18) (2).csv")
c2 <- read_csv("raw_data/ultimatum_t1 (accessed 2017-08-18) (2).csv")
c$condition <- "windfall"
c2$condition <- "control"
surv2 <- read_csv("raw_data/Vignette%2Fcontrol+survey_August+18%2C+2017_15.11.csv")
# saving pre-survey and pre-attention check copies
psc <- rbind(c,c2)
pac <- surv2

c <- subset(c, c$player.valid_code %in% surv2$mTurkCode)
c2 <- subset(c2, c2$player.valid_code %in% surv2$mTurkCode)
cf <- rbind(c,c2)
surv2 <- subset(surv2, surv2$userid %in% cf$participant.code &
                  Q13=='$1.00')
cf <- cf %>%
  rename(userid = participant.code)
cf <- left_join(cf, surv2, by ='userid')
cf <- subset(cf, player.valid_code==mTurkCode)
#dim(cf); table((data.frame(table(cf$userid)))$Freq)
cf <- cf %>%
  #dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    #code = participant.code,
    label = participant.label,
    isbot = participant._is_bot,
    pagenumber = participant._index_in_pages,
    maxpage = participant._max_page_index,
    app = participant._current_app_name,
    round = participant._round_number,
    pagename = participant._current_page_name,
    ip = participant.ip_address,
    time = participant.time_started,
    exclude = participant.exclude_from_data_analysis,
    visited = participant.visited,
    #mturk.id = participant.mturk_worker_id,
    mturk.assignid = participant.mturk_assignment_id,
    #XX = participant.payoff,
    playerid = player.id_in_group,
    validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number,
    duration = `Duration (in seconds)`,
    currencyfee = Q42_2,
    brokerfee = Q42_3,
    servicefee = Q42_4,
    ATMfee = Q42_5,
    offerexp = Q59_1,
    age = Q1_1,
    occupation = Q1_2,
    nationality = Q1_3,
    gender = Q30,
    attcheck1 = Q13,
    fairoutcome = Q7_1,
    attcheck2 = Q7_2,
    forex_exp = Q8_1,
    travel_exp = Q8_2,
    recognizeUG = Q54
  ) #%>%
#dplyr::filter(validcode != "")

cf <- subset(cf, select= -c(
  participant_id,
  app,
  label,
  isbot,
  pagenumber,
  maxpage,
  round,
  pagename,
  time,
  exclude,
  visited,
  participant.mturk_worker_id,
  mturk.assignid,
  subsessionid,
  subsessionround,
  session.code,
  session.label,
  session.experimenter_name,
  session.mturk_HITId,
  session.mturk_HITGroupId,
  session.comment,
  session.is_demo,
  StartDate,
  EndDate,
  Status,
  Progress,
  Finished,
  RecordedDate,
  ResponseId,
  RecipientLastName,
  RecipientFirstName,
  RecipientEmail,
  ExternalReference,
  DistributionChannel
))

d <- read_csv("raw_data/ultimatum_banker_vignette (accessed 2017-08-17) (2).csv")
d2 <- read_csv("raw_data/ultimatum_customer_vignette (accessed 2017-08-17) (2).csv")
d$condition <- "banker"
d2$condition <- "customer"
d$group.amount_offered <- 100-d$group.amount_offered
surv <- read_csv("raw_data/Vignette+survey_August+17%2C+2017_16.46.csv")

# adding to the pre-survey and pre-attention check copies
psc <- rbind(psc,d,d2)
pac <- rbind(pac, surv)
psc <- subset(psc, session.is_demo==0)
psc <- psc %>%
  rename(userid = participant.code)
pac <- subset(pac, Finished == "True" | Finished == "False")
#data.frame(sort(table(psc$participant.code), decreasing = TRUE))
repdf <- data.frame(sort(table(pac$userid), decreasing = TRUE))
reps <- subset(repdf, Freq > 1)
  #repdf$Var1[repdf$Freq>1]
pac <- subset(pac, !(userid %in% reps$Var1))
pac <- subset(pac, Status != "Survey Preview")

psc <- psc %>%
  #dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    #code = participant.code,
    label = participant.label,
    isbot = participant._is_bot,
    pagenumber = participant._index_in_pages,
    maxpage = participant._max_page_index,
    app = participant._current_app_name,
    round = participant._round_number,
    pagename = participant._current_page_name,
    ip = participant.ip_address,
    time = participant.time_started,
    exclude = participant.exclude_from_data_analysis,
    visited = participant.visited,
    #mturk.id = participant.mturk_worker_id,
    mturk.assignid = participant.mturk_assignment_id,
    #XX = participant.payoff,
    playerid = player.id_in_group,
    validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number
  )

pac <- pac %>%
  #dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    duration = `Duration (in seconds)`,
    currencyfee = Q42_2,
    brokerfee = Q42_3,
    servicefee = Q42_4,
    ATMfee = Q42_5,
    offerexp = Q59_1,
    age = Q1_1,
    occupation = Q1_2,
    nationality = Q1_3,
    gender = Q30,
    attcheck1 = Q13,
    fairoutcome = Q7_1,
    attcheck2 = Q7_2,
    forex_exp = Q8_1,
    travel_exp = Q8_2,
    recognizeUG = Q54
  ) #%>%
#dplyr::filter(validcode != "")

psc <- subset(psc, select= -c(
  participant_id,
  app,
  label,
  isbot,
  pagenumber,
  maxpage,
  round,
  pagename,
  time,
  exclude,
  visited,
  participant.mturk_worker_id,
  mturk.assignid,
  subsessionid,
  subsessionround,
  session.code,
  session.label,
  session.experimenter_name,
  session.mturk_HITId,
  session.mturk_HITGroupId,
  session.comment,
  session.is_demo
))
pac <- subset(pac, select= -c(
  StartDate,
  EndDate,
  #Status,
  #Progress,
  #Finished,
  RecordedDate,
  ResponseId,
  RecipientLastName,
  RecipientFirstName,
  RecipientEmail,
  ExternalReference,
  DistributionChannel
))

#p3 <- subset(pp, (Q13=="International airport" | Q13=='$1.00') &
 #              (Q7_2=='Strongly agree'))
# cleaned up pac and psc
#data.frame(sort(table(psc$participant.code), decreasing = TRUE))
#data.frame(sort(table(pac$userid), decreasing = TRUE))
# back to df...

d <- subset(d, d$player.valid_code %in% surv$mTurkCode)
d2 <- subset(d2, d2$player.valid_code %in% surv$mTurkCode)
df <- rbind(d,d2)
surv <- subset(surv, surv$userid %in% df$participant.code &
                 Q13=='International airport')
df <- df %>%
  rename(userid = participant.code)
df <- left_join(df, surv, by="userid")
df <- subset(df, player.valid_code==mTurkCode)
#dim(df); table((data.frame(table(df$userid)))$Freq)
df <- df %>%
  #dplyr::filter(participant.mturk_worker_id != "") %>%
  rename(
    participant_id = participant.id_in_session,
    #code = participant.code,
    label = participant.label,
    isbot = participant._is_bot,
    pagenumber = participant._index_in_pages,
    maxpage = participant._max_page_index,
    app = participant._current_app_name,
    round = participant._round_number,
    pagename = participant._current_page_name,
    ip = participant.ip_address,
    time = participant.time_started,
    exclude = participant.exclude_from_data_analysis,
    visited = participant.visited,
    #mturk.id = participant.mturk_worker_id,
    mturk.assignid = participant.mturk_assignment_id,
    #XX = participant.payoff,
    playerid = player.id_in_group,
    validcode = player.valid_code,
    payoff = player.payoff,
    subsessionid = group.id_in_subsession,
    offer = group.amount_offered,
    accept = group.offer_accepted,
    subsessionround = subsession.round_number,
    duration = `Duration (in seconds)`,
    currencyfee = Q42_2,
    brokerfee = Q42_3,
    servicefee = Q42_4,
    ATMfee = Q42_5,
    offerexp = Q59_1,
    age = Q1_1,
    occupation = Q1_2,
    nationality = Q1_3,
    gender = Q30,
    attcheck1 = Q13,
    fairoutcome = Q7_1,
    attcheck2 = Q7_2,
    forex_exp = Q8_1,
    travel_exp = Q8_2,
    recognizeUG = Q54
  ) #%>%
  #dplyr::filter(validcode != "")

df <- subset(df, select= -c(
  participant_id,
  label,
  isbot,
  app,
  pagenumber,
  maxpage,
  round,
  pagename,
  time,
  exclude,
  visited,
  participant.mturk_worker_id,
  mturk.assignid,
  subsessionid,
  subsessionround,
  session.code,
  session.label,
  session.experimenter_name,
  session.mturk_HITId,
  session.mturk_HITGroupId,
  session.comment,
  session.is_demo,
  StartDate,
  EndDate,
  Status,
  Progress,
  Finished,
  RecordedDate,
  ResponseId,
  RecipientLastName,
  RecipientFirstName,
  RecipientEmail,
  ExternalReference,
  DistributionChannel
))

df <- rbind(cf,df)

df$duration <- as.numeric(df$duration)
df$LocationLatitude <- as.numeric(df$LocationLatitude)
df$LocationLongitude <- as.numeric(df$LocationLongitude)
df$currencyfee <- as.numeric(df$currencyfee)
df$brokerfee <- as.numeric(df$brokerfee)
df$servicefee <- as.numeric(df$servicefee)
df$ATMfee <- as.numeric(df$ATMfee)
df$offerexp <- as.numeric(df$offerexp)
df$age <- as.numeric(df$age)
df$fairoutcome <- factor(df$fairoutcome, levels = c(
  'Strongly disagree', 'Somewhat disagree',
  'Neither agree nor disagree', 'Somewhat agree',
  'Strongly agree'
))
df$forex_exp <- factor(df$forex_exp, levels = c(
  'No experience at all', 'Lower than average',
  'Some experience', 'Higher than average',
  'Extensive experience'
))
df$travel_exp <- factor(df$travel_exp, levels = c(
  'No experience at all', 'Lower than average',
  'Some experience', 'Higher than average',
  'Extensive experience'
))

UG.data <- df
UG.presurv <- psc
UG.preatt <- pac
rm(a); rm(c); rm(c2); rm(cf); rm(check); rm(d)
rm(d2); rm(df); rm(dr); rm(ip); rm(p3); rm(pac)
rm(pp); rm(ppp); rm(psc); rm(repdf); rm(reps)
rm(surv); rm(surv2); rm(codes)
### saves data to package with git commit ####
#setwd("~/Desktop/MA Thesis/framingdata2017")
use_data(UG.data, overwrite = TRUE)
use_data(UG.presurv, overwrite = TRUE)
use_data(UG.preatt, overwrite = TRUE)



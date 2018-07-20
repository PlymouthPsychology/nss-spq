library(readr)
NSSdata <- read_csv("NSSdata.csv")

#View(NSSdata)

as.factor(NSSdata$quiz)
as.factor(NSSdata$year)


#NSSItems <- read_csv("NSSItems.csv")
#View(NSSItems)

library(tidyverse)


school <- NSSdata %>% filter(quiz=='nss')
sector <- NSSdata %>% filter(quiz=='psy')

target<-'Q27'
item<-NSSItems[which(NSSItems$QID==target),5]

ratings<-NSSdata %>% 
  reshape2::melt(id.vars=c("year","quiz"), variable.name="item", value.name="agree")

library(ggplot2)
# plot a few items for all surveys for all years
p <-ratings %>% filter(item=="Q01" | item == "Q02" | item == "Q03") %>%
  ggplot(aes(x=year, y=agree, colour=item, group=item)) +
  xlab("") +
  ylab("") +
  geom_line() +
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  facet_grid(quiz ~ .)
p

# plot a T&L category mean for all surveys for all years
p <-ratings %>% filter(item=="Q01" | item == "Q02" | item == "Q03" | item == "Q04") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Teaching and Learning (Q01-Q04)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# plot a T&L category mean for all surveys for all years
p <-ratings %>% filter(item=="Q05" | item == "Q06" | item == "Q07" ) %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle(" Learning Opportunities (Q05-Q08)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(limits=c(2017,2018), breaks=c(2017:2018),labels=c(2017:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# plot a T&L category mean for all surveys for all years
p <-ratings %>% filter(item=="Q08" | item == "Q09" | item == "Q10" | item == "Q11") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Assessment and feedback (Q08-Q11)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

p <-ratings %>% filter(item=="Q12" | item == "Q13" | item == "Q14") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Academic support (Q12-Q14)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

#Organisation and management

p <-ratings %>% filter(item=="Q15" | item == "Q16" | item == "Q17") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Organisation and management (Q15-Q17)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# Learning resources

p <-ratings %>% filter(item=="Q18" | item == "Q19" | item == "Q20") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Learning resources (Q18-Q20)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# Learning community

p <-ratings %>% filter(item=="Q21" | item == "Q22" ) %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Learning community (Q21-Q22)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(limits=c(2017,2018), breaks=c(2017:2018),labels=c(2017:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p
# plot Student Voice  mean for all surveys for all years
p <-ratings %>% filter(item=="Q23" | item == "Q24" | item == "Q25" | item == "Q26") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  group_by(year,quiz) %>%
  summarise(category = round(mean(agree, na.rm=TRUE), digits=1)) %>%
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  ggplot(aes(x=year, y=category, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Student Voice (Q23-Q26)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(category),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(limits=c(2017,2018), breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# plot Q27 for all surveys for all years
p <-ratings %>% filter(item=="Q27") %>% 
  # EDIT CATEGORY IN LINE ABOVE HERE
  mutate(quiz = recode(quiz,"nss"="School","psy"="Sector","uop"="University", "spq"="SPQ")) %>%
  mutate(agree = round(agree, 1)) %>%
  group_by(year,quiz) %>%
  ggplot(aes(x=year, y=agree, colour=quiz, group=quiz)) +
  xlab("") + ylab("percentqge agree")+
  ggtitle("Overall (Q27)") +   
  #  EDIT TITLE IN LINE ABOVE HERE
  geom_line(size=1) + geom_point(shape=21) +
  geom_text(aes(label=ifelse(year>2016,as.character(agree),'')),hjust=0,vjust=0)+
  guides(colour=guide_legend(title="Survey"))+
  scale_x_continuous(breaks=c(2008:2018),labels=c(2008:2018))+
  # EDIT YEARS TO PLOT ABOVE HERE
  scale_y_continuous(limits=c(50,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        panel.grid.minor = element_blank())
p

# school performance on all sections by year



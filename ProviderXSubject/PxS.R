

library(readr)
library(tidyverse)

df<-read_csv("nss2018.csv")

df2<-read_csv("nss2017.csv")
df<-rbind(df,df2)

df2<-read_csv("nss2016.csv")
df<-rbind(df,df2)

df2<-read_csv("nss2015.csv")
df<-rbind(df,df2)

df2<-read_csv("nss2014.csv")
df<-rbind(df,df2)

df2<-read_csv("nss2013.csv")
df<-rbind(df,df2)

df2<-read_csv("nss2012.csv")
df<-rbind(df,df2)

#correct lack of leading zero in item labels for some years
df$Item<-recode(df$Item, 'Q1'='Q01','Q2'='Q02','Q3'='Q03','Q4'='Q04','Q5'='Q05',
           'Q6'='Q06','Q7'='Q07','Q8'='Q08','Q9'='Q09')

# add category, Question Labels and Text with default values
df$Category<-'Category'
df$Question<-'Q00'
df$Text<-'Item content'

# create Question lables for >=2017 surveys
df$Question[df$Year>=2017]<-df$Item[df$Year>=2017]

# create Question lables for pre2017 surveys
df$Question[df$Year<2017 & df$Item=='Q01']<-'Q01'
df$Question[df$Year<2017 & df$Item=='Q02']<-'Q02'
df$Question[df$Year<2017 & df$Item=='Q03']<-'OLD03'
df$Question[df$Year<2017 & df$Item=='Q04']<-'Q03'
df$Question[df$Year<2017 & df$Item=='Q05']<-'Q08'
df$Question[df$Year<2017 & df$Item=='Q06']<-'Q09'
df$Question[df$Year<2017 & df$Item=='Q07']<-'Q10'
df$Question[df$Year<2017 & df$Item=='Q08']<-'Q11'
df$Question[df$Year<2017 & df$Item=='Q09']<-'OLD09'
df$Question[df$Year<2017 & df$Item=='Q10']<-'Q13'
df$Question[df$Year<2017 & df$Item=='Q11']<-'Q12'
df$Question[df$Year<2017 & df$Item=='Q12']<-'Q14'
df$Question[df$Year<2017 & df$Item=='Q13']<-'Q16'
df$Question[df$Year<2017 & df$Item=='Q14']<-'Q17'
df$Question[df$Year<2017 & df$Item=='Q15']<-'Q15'
df$Question[df$Year<2017 & df$Item=='Q16']<-'Q19'
df$Question[df$Year<2017 & df$Item=='Q17']<-'Q18'
df$Question[df$Year<2017 & df$Item=='Q18']<-'Q20'
df$Question[df$Year<2017 & df$Item=='Q19']<-'OLD19'
df$Question[df$Year<2017 & df$Item=='Q20']<-'OLD20'
df$Question[df$Year<2017 & df$Item=='Q21']<-'OLD21'
df$Question[df$Year<2017 & df$Item=='Q22']<-'Q27'

# create Category Labels
df$Category[df$Question=='OLD03'|df$Question=='Q01'|df$Question=='Q02'|df$Question=='Q03'|df$Question=='Q04']<-'The teaching on my course'
df$Category[df$Question=='OLD05'|df$Question=='Q06'|df$Question=='Q07']<-'Learning opportunities'
df$Category[df$Question=='OLD09'|df$Question=='Q08'|df$Question=='Q09'|df$Question=='Q10'|df$Question=='Q11']<-'Assessment and feedback'
df$Category[df$Question=='Q12'|df$Question=='Q13'|df$Question=='Q14']<-'Academic support'
df$Category[df$Question=='Q15'|df$Question=='Q16'|df$Question=='Q17']<-'Organisation and management'
df$Category[df$Question=='Q18'|df$Question=='Q19'|df$Question=='Q20']<-'Learning resources'
df$Category[df$Question=='OLD18'|df$Question=='OLD19'|df$Question=='OLD20']<-'Personal development'
df$Category[df$Question=='Q21'|df$Question=='Q22']<-'Learning community'
df$Category[df$Question=='Q23'|df$Question=='Q24'|df$Question=='Q25']<-'Student voice'
df$Category[df$Question=='Q26']<-'Student Union'
df$Category[df$Question=='Q27']<-'Overall Satisfaction'

# create text (Use new text for modified items)
df$Text[df$Question=='Q01']<-'1 Staff are good at explaining things'
df$Text[df$Question=='Q02']<-'2 Staff have made the subject interesting'
df$Text[df$Question=='OLD03']<-'Staff are enthusiastic about what they are teaching'
df$Text[df$Question=='Q03']<-'3 The course is intellectually stimulating'
df$Text[df$Question=='Q04']<-'4 My course has challenged me to achieve my best work'
df$Text[df$Question=='Q05']<-'5 My course has provided me with opportunities to explore ideas or concepts in depth'
df$Text[df$Question=='Q06']<-'6 My course has provided me with opportunities to bring information and ideas together from different topics'
df$Text[df$Question=='Q07']<-'7 My course has provided me with opportunities to apply what I have learnt'
df$Text[df$Question=='Q08']<-'8 The criteria used in marking have been clear in advance'
df$Text[df$Question=='Q09']<-'9 Marking and assessment has been fair'
df$Text[df$Question=='Q10']<-'10 Feedback on my work has been timely'
df$Text[df$Question=='Q11']<-'11 I have received helpful comments on my work'
df$Text[df$Question=='OLD09']<-'Feedback on my work has helped me clarify things I did not understand'
df$Text[df$Question=='Q13']<-'13 I have received sufficient advice and guidance in relation to my course'
df$Text[df$Question=='Q12']<-'12 I have been able to contact staff when I needed to'
df$Text[df$Question=='Q14']<-'14 Good advice was available when I needed to make study choices on my course'
df$Text[df$Question=='Q16']<-'16 The timetable works efficiently for me'
df$Text[df$Question=='Q17']<-'17 Any changes in the course or teaching have been communicated effectively'
df$Text[df$Question=='Q15']<-'15 The course is well organised and running smoothly'
df$Text[df$Question=='Q19']<-'19 The library resources (e.g. books, online services and learning spaces) have supported my learning well'
df$Text[df$Question=='Q18']<-'18 The IT resources and facilities provided have supported my learning well'
df$Text[df$Question=='Q20']<-'20 I have been able to access course-specific resources (e.g. equipment, facilities, software, collections) when I needed to'
df$Text[df$Question=='OLD19']<-'The course has helped me present myself with confidence.'
df$Text[df$Question=='OLD20']<-'My communication skills have improved.'
df$Text[df$Question=='OLD21']<-'As a result of the course, I feel confident in tackling unfamiliar problems'
df$Text[df$Question=='Q21']<-'21 I feel part of a community of staff and students'
df$Text[df$Question=='Q22']<-'22 I have had the right opportunities to work with other students as part of my course'
df$Text[df$Question=='Q23']<-'23 I have had the right opportunities to provide feedback on my course'
df$Text[df$Question=='Q24']<-'24 Staff value students views and opinions about the course'
df$Text[df$Question=='Q25']<-'25 It is clear how students’ feedback on the course has been acted on'
df$Text[df$Question=='Q26']<-'26 The students union (association or guild) effectively represents students academic interests'
df$Text[df$Question=='Q27']<-'27 Overall, I am satisfied with the quality of the course'



# some variables are factors not strings or numbers
df$Provider<-as.factor(df$Provider)
df$Subject<-as.factor(df$Subject)
df$Item<-as.factor(df$Item)

#table(df$Item,df$Year)
#boxplot(Agree ~ Year, data=df)

# retain ratings for single item from Psychology for all providers, all years
pdata<-df %>% filter(Item=='Q01' & grepl('Psychology', Subject)) %>% select(Provider, Year, Agree)

# compute ratings for single category from Psychology for each providers, all years
pdata<-df %>% filter(Category=='The teaching on my course' & grepl('Psychology', Subject)) %>% 
              select(Provider, Year, Agree) %>% 
              group_by(Provider, Year) %>%
              summarise(Agree=mean(Agree))


# isolate Plymouth's data
uop<-pdata %>% filter(  Provider=='University of Plymouth' 
                        | Provider=="UNIVERSITY OF PLYMOUTH" 
                        | Provider=='Plymouth University' )

# compute quantiles over all providers for each year
pmeans<-pdata %>% group_by(Year)  %>% summarise(yearvlow=quantile(Agree,0.10),
                                                yearlower=quantile(Agree,0.25),
                                                yearmedian=quantile(Agree,0.5),
                                                yearupper=quantile(Agree,0.75),
                                                yearvhigh=quantile(Agree,0.90))

#  density plots with 25%, median, 75% and plymouth line, faceted by year
p<-ggplot(pdata,  aes(x=Agree))+
  ylab("")+scale_x_continuous(limits = c(.7, 1))+
  geom_density(adjust=.5, fill="blue", alpha=0.6, linetype=0)+
  geom_vline(data=pmeans,aes(xintercept=yearmean),  size=1) +
  geom_vline(data=pmeans,aes(xintercept=yearlower),  size=.5) +
  geom_vline(data=pmeans,aes(xintercept=yearupper),  size=.5) +
  geom_vline(data=uop,aes(xintercept=Agree),  colour="red", size=.5) +
  theme(axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        plot.background=element_rect(fill = "white"))+
    facet_grid(Year ~ .)
p

# ribbon plot with 10-90 in light grey, 25-75 mid grey, median line,  plymouth in red
p<-ggplot(uop, aes(x=Year, y=Agree))+
  scale_y_continuous(limits = c(.7, 1))+
  geom_ribbon(data=pmeans, aes(x=Year, y=yearmedian, ymin=yearvlow, ymax=yearvhigh),fill="grey90")+
  geom_ribbon(data=pmeans, aes(x=Year, y=yearmedian, ymin=yearlower, ymax=yearupper),fill="grey80")+
  geom_line(colour="red")+
  geom_line(data=pmeans, aes(x=Year, y=yearmedian))+
  ggtitle("The teaching on my course")+
  theme(panel.background=element_blank())
p
ggsave("Teaching.png", width=20, height = 12, units='cm')


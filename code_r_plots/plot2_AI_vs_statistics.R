# Read CSV file
data <- read.csv("raw_data3.csv")

library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)
counts <- data %>%
  group_by(year, expl_group) %>%
  summarise(Count = n()) %>%
  ungroup()

sumatory <- plyr::ddply(counts, c('year'), function(x) sum(x$Count))

counts <-merge(counts, sumatory, by = 'year')
counts$relative_count <- counts$Count/counts$V1

dum <- counts
dum$franja <- "<96"
dum$franja[which(dum$year<1996)] <- "<1996"
dum$franja[which(dum$year>=1996 &dum$year<=2000)] <- "1996-2000"
dum$franja[which(dum$year>=2001 &dum$year<=2005)] <- "2001-2005"
dum$franja[which(dum$year>=2006 &dum$year<=2010)] <- "2006-2010"
dum$franja[which(dum$year>=2011 &dum$year<=2015)] <- "2011-2015"
dum$franja[which(dum$year>=2016 &dum$year<=2020)] <- "2016-2020"
dum$franja[which(dum$year>=2021)] <- "2021-2022"


dum<-arrange(dum, expl_group,year)

dum2<-plyr::ddply(dum, 'expl_group', function(dx){
  dx$diff<-c(0,diff(dx$relative_count))
  dx$diff_abs<-c(0,diff(dx$Count))
  return(dx)
})



#Barplot per la derivada
a<-ggplot(dum2, aes(x = as.factor(year), y = diff,fill = expl_group)) +
  scale_colour_brewer(palette = 'Set1')+
  facet_wrap(~expl_group, ncol=1)+
  geom_bar(position="dodge",stat="identity" ,width=0.5)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))



dum3<-dum2
dum3<-dum3[!dum3$expl_group %in% c('Others', 'Descriptive Analysis', ""),]
dum3$newvar<-'AI (ML/DL)'
dum3$newvar[grep('Liniar/Logistic', dum3$expl_group)]<-'Statistics (Liniar/Logistic)'

b<-ggplot(dum3, aes(x = as.factor(year), y = Count,fill = newvar)) +
  scale_colour_brewer(palette = 'Set1')+
  geom_bar(position="dodge",stat="identity" ,width=0.5)+
  geom_vline(xintercept =2017-1995, linetype='dashed') +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.title = element_blank(),
    legend.position = "bottom"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = 'top')+
  #scale_y_continuous('free')+
  scale_x_discrete(breaks = dum3$year[seq(1, length(dum3$year), 2)]) +  # Plotting every 2 years
  labs(x='Year', y = 'Abstract count  [Abstracts]')+
  facet_wrap(~newvar, nrow=2, scales = 'free_y')
b

c<-ggplot(dum3, aes(x = as.factor(year), y = diff_abs, fill = newvar)) +
  scale_colour_brewer(palette = 'Set1') +
  geom_bar(position = "dodge", stat = "identity", width = 0.5) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.title = element_blank(),
    legend.position = "top"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_x_discrete(breaks = dum3$year[seq(1, length(dum3$year), 2)]) +  # Plotting every 2 years
  labs(x = 'Year', y = 'Abstract count time derivative [Abstracts/year]', fill = 'Legend')

c
d<-ggarrange(plotlist = list(b,c), nrow=2, ncol=1, common.legend = T)
annotate_figure(b, top = text_grob("Evolution of publishments", size = 14))



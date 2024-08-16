data <- read.csv("raw_data3.csv")

library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)

data$expl_group[grep('ML', data$expl_group)]<-'ML'
data$field[grep('Biomarker research', data$field)]<-'Biomarker research'
data$field[grep('Disease monitoring', data$field)]<-'Disease monitoring'
dum <- data
dum <- dum[dum$expl_group %in% c('DL'),]
dum <- dum[dum$field %in% c('Disease detection','Disease course prediction','Biomarker research','Disease monitoring'),]
#dum <- dum[dum$Country %in% c('US','EU','China'),]

counts <- dum %>%
  group_by(year, field) %>%
  summarise(Count = n()) %>%
  ungroup()

ggplot(counts, aes(x = as.factor(year), y = Count, color = field)) +
  geom_point(size = 0.5, alpha = 0.7) +
  facet_wrap(~ field, scales = "free_x") +
  scale_y_log10() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dum2 <- data
dum2 <- dum2[dum2$field %in% c('Disease detection','Disease course prediction','Biomarker research','Disease monitoring'),]

counts <- dum2 %>%
  group_by(year, field,expl_group) %>%
  summarise(Count = n()) %>%
  ungroup()

sumatory <- plyr::ddply(counts, c('year','field'), function(x) sum(x$Count))
counts <-merge(counts, sumatory, by = c('year','field'))
counts$relative_count <- counts$Count/counts$V1


counts$franja <- "<96"
counts$franja[which(counts$year<1996)] <- "<1996"
counts$franja[which(counts$year>=1996 &counts$year<=2000)] <- "1996-2000"
counts$franja[which(counts$year>=2001 &counts$year<=2005)] <- "2001-2005"
counts$franja[which(counts$year>=2006 &counts$year<=2010)] <- "2006-2010"
counts$franja[which(counts$year>=2011 &counts$year<=2015)] <- "2011-2015"
counts$franja[which(counts$year>=2016 &counts$year<=2021)] <- "2016-2021"


DL <- counts%>%filter(expl_group=='DL')
a<-ggplot(DL, aes(x = as.factor(year), y = Count, group = field, color = field)) +
  geom_point(size = 0.5, alpha = 0.7) +
  scale_y_continuous(trans='log10')+
  facet_wrap(~ franja, ncol = 7, scales = "free_x") +
  geom_smooth(method = "lm", se = FALSE , size = 1.5, alpha=0.9) +#
  geom_smooth(method = "lm", se = FALSE , size = 1.35,color='white') +#
  geom_smooth(method = "lm", se = FALSE , size = 0.45) +
  theme_bw() +
  #scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = 'top')+
  ggtitle('Regression of total publications of DL per field')+
  xlab('Year')+
  ylab('Total article count on DL [Articles]')

df<-plyr::ddply(DL,c('franja','field'), function(dum3){
  mod<-lm(Count~year, data=dum3)
  summod <- summary(mod)
  data.frame(
    year_beta=mod$coefficients[2]#,
    #sign=summod$coefficients[2,4]
    
    #std_beta=summod$coefficients[2,2]
  )
})

#df$signCorr <- p.adjust(df$sign,method = 'BH')

#df<- df[!is.nan(df$std_beta),]
df$growth<-'springgreen3'
df$growth[df$year_beta<0]<-'tomato3'
#df$growth[df$signCorr>0.05]<-'gold'

b<-ggplot(df, aes(x = field, y = year_beta)) +
  geom_bar(stat="identity",width=0.3, fill = df$growth)+
  facet_wrap(~ franja, ncol = 7, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  ggtitle('Beta values of regression for 5 years group and field')+
  ylab(expression(paste("Regression slope (", beta, ") [articles/year]")))+
  xlab('Field')
b


c<-ggarrange(plotlist = list(a,b), nrow=2, ncol=1, common.legend = T,heights = c(3, 2))
c
annotate_figure(c, top = text_grob("Evolution of publishments per field", size = 14))



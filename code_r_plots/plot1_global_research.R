# Read CSV file
data <- read.csv("raw_data3.csv")
#data2<-read.csv("raw_data2.csv")
library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)
# counts <- data %>%
#   group_by(year, Continent) %>%
#   summarise(Count = n()) %>%
#   ungroup()
# 
# 
# dum <- counts
# dum$franja <- "<96"
# dum$franja[which(dum$year<1996)] <- "<1996"
# dum$franja[which(dum$year>=1996 &dum$year<=2000)] <- "1996-2000"
# dum$franja[which(dum$year>=2001 &dum$year<=2005)] <- "2001-2005"
# dum$franja[which(dum$year>=2006 &dum$year<=2010)] <- "2006-2010"
# dum$franja[which(dum$year>=2011 &dum$year<=2015)] <- "2011-2015"
# dum$franja[which(dum$year>=2016 &dum$year<=2020)] <- "2016-2020"
# dum$franja[which(dum$year>=2021)] <- "2021-2022"
# 
# 
# a<-ggplot(dum, aes(x = as.factor(year), y = Count, group = Continent, color = Continent)) +
#   geom_point(size = 0.5, alpha = 0.7) +
#   facet_wrap(~ franja, ncol = 7, scales = "free_x") +
#   scale_y_log10() +
#   geom_smooth(method = "lm", se = FALSE , size = 2, alpha=0.9) +#
#   geom_smooth(method = "lm", se = FALSE , size = 1.75,color='white') +#
#   geom_smooth(method = "lm", se = FALSE , size = 0.55) +
#   theme_bw() +
#   scale_colour_brewer(palette = 'Set1')+
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
# 
# a
# #ggarrange(plotlist = list(a,b), nrow=2, ncol=1, common.legend = T)
# 
# #Liniar regression 
# dum2<- dum[!dum$Continent %in% c("","Others"),]
# 
# df<-plyr::ddply(dum2,c('franja','Continent'), function(dum3){
#   mod<-lm(Count~year, data=dum3)
#   summod <- summary(mod)
#   data.frame(
#   year_beta=mod$coefficients[2],
#   sign=summod$coefficients[2,4]
#   
#   #std_beta=summod$coefficients[2,2]
#   )
# })
# 
# df$signCorr <- p.adjust(df$sign,method = 'BH')
# 
# #df<- df[!is.nan(df$std_beta),]
# df$growth<-'springgreen3'
# df$growth[df$year_beta<0]<-'tomato3'
# #df$growth[df$signCorr>0.05]<-'gold'
# 
# b<-ggplot(df, aes(x = Continent, y = year_beta)) +
#   geom_bar(stat="identity",fill = df$growth,width=0.3)+
#   facet_wrap(~ franja, ncol = 7, scales = "free_x") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
#   scale_y_continuous(limits = c(-3250,3250),breaks = scales::pretty_breaks(n = 10))
# 
# 
# ggarrange(plotlist = list(a,b), nrow=2, ncol=1, common.legend = T)

counts <- data %>%
   group_by(year, Continent) %>%
   summarise(Count = n()) %>%
   ungroup()

yearly_research <-plyr::ddply(counts, c('year'), function(x) sum(x$Count))
counts <- merge(counts, yearly_research, by='year')
counts$relative_count <- (counts$Count/counts$V1)*100

#counts<-counts[!counts$Continent %in% c('Others'),]

ggplot(counts, aes(x = as.factor(year), y = relative_count, group = Continent, color = Continent)) +
  geom_line(size = 0.5, alpha = 0.7) +
  # geom_smooth(method = "lm", se = FALSE , size = 2, alpha=0.9) +#
  # geom_smooth(method = "lm", se = FALSE , size = 1.75,color='white') +#
  # geom_smooth(method = "lm", se = FALSE , size = 0.55) +
  theme_bw() +
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle('Relative publications each continent has respect to total ')+
  xlab('Year')+
  ylab('Relative count [%]')

ggplot (counts, aes(x = as.factor(year), y = Count, group = Continent, color = Continent)) +
  geom_line(size = 0.5, alpha = 0.7) +
  # geom_smooth(method = "lm", se = FALSE , size = 2, alpha=0.9) +#
  # geom_smooth(method = "lm", se = FALSE , size = 1.75,color='white') +#
  # geom_smooth(method = "lm", se = FALSE , size = 0.55) +
  theme_bw() +
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

a<-ggplot(counts, aes(x=year, y=Count, fill=Continent)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  #scale_fill_viridis(discrete = T) +
  #theme_ipsum() + 
  theme_bw() +
  ggtitle("Total published articles per continent")+
  xlab('Year')+
  ylab('Total articles count [Articles]')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))



b<-ggplot(counts, aes(x=year, y=relative_count, fill=Continent)) + 
  geom_area(alpha=0.6 , size=.5, colour="white") +
  #scale_fill_viridis(discrete = T) +
  #theme_ipsum() + 
  theme_bw()+
  ggtitle("Relative published articles per continent")+
  xlab('Year')+
  ylab('Relative article count [% articles]')+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 12))

c<-ggarrange(plotlist = list(a,b), nrow=1, ncol=2, common.legend = T)
annotate_figure(c, top = text_grob("Articles per continent", size = 14))

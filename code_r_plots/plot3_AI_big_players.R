# Big players en IA (ONLY ML I DL) Quin % representa cada un (US, CHina i EU)

#PENSAR COM FER UN RANKING DELS BIG PLAYERS
#PENSAR COM ARREGLAR LA BBDD

data <- read.csv("raw_data3.csv")

library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)
data$AI<-'Other'
#data$AI[grep('ML|DL', data$expl_group)]<-'AI'
data$AI[grep('DL|ML', data$expl_group)]<-'AI'
data$affiliations[grep('USA|US|United States|EEUU', data$affiliations)]<-'US'
data$affiliations[grep("CN|CHN|China|People's republic of China|中国", data$affiliations)]<-'China'
data$affiliations[grep("European Union|EU|AT|AUT|Austria|BE|BEL|Belgium|BG|BGR|Bulgaria|HR|HRV|Croatia|CY|CYP|Cyprus|CZ|CZE|Czech Republic|DK|DNK|Denmark|EE|EST|Estonia|FI|FIN|Finland|FR|FRA|France|DE|DEU|Germany|EL|GRC|Greece|HU|HUN|Hungary|IE|IRL|Ireland|IT|ITA|Italy|LV|LVA|Latvia|LT|LTU|Lithuania|LU|LUX|Luxembourg|MT|MLT|Malta|NL|NLD|Netherlands|PL|POL|Poland|PT|PRT|Portugal|RO|ROU|Romania|SK|SVK|Slovakia|SI|SVN|Slovenia|ES|ESP|Spain|SE|SWE|Sweden", data$affiliations)]<-'US'

counts <- data %>%
  filter(AI=='AI')%>%
  group_by(year, Country) %>%
  summarise(Count = n()) %>%
  ungroup()

sumatory <- plyr::ddply(counts, c('year'), function(x) sum(x$Count))
counts <-merge(counts, sumatory, by = c('year'))
counts$relative_count <- (counts$Count/counts$V1)*100
  
  
counts <- counts[counts$Country %in% c('US','EU','China'),]

total_count<- ggplot(counts, aes(x = as.factor(year), y = Count, group = Country, color = Country)) +
  geom_point(size = 2, alpha = 0.7) +
  theme_bw() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 15)) +  # Plotting every 2 years
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle('AI publications per year')+
  xlab('Year')+
  ylab('Total articles count [articles]')+
  geom_smooth(method = "loess", se = FALSE , size = 2, alpha=0.9)+
  geom_smooth(method = "loess", se = FALSE , size = 1.75,color='white')+
  geom_smooth(method = "loess", se = FALSE , size = 0.55) 
total_count
imputChina<-as.data.frame(rbind(c(year=1996, 
                                  Country= 'China',
                                  Count =0, 
                                  V1 =0, 
                                  relative_count =0),
                                c(year=1997, 
                                  Country= 'China',
                                  Count =0, 
                                  V1 =0, 
                                  relative_count =0),
                                c(year=1998, 
                                  Country= 'China',
                                  Count =0, 
                                  V1 =0, 
                                  relative_count =0)))

imputChina2<-rbind(counts, imputChina)
imputChina2$year<- as.numeric(imputChina2$year)
imputChina2$Count<- as.numeric(imputChina2$Count)
imputChina2$V1<- as.numeric(imputChina2$V1)
imputChina2$relative_count<- as.numeric(imputChina2$relative_count)

relative_count <- ggplot(imputChina2, aes(x = as.factor(year), y = relative_count, group = Country, color = Country)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE ,  size = 2, alpha=0.9) +#
  geom_smooth(method = "loess", se = FALSE ,  size = 1.75,color='white') +#
  geom_smooth(method = "loess", se = FALSE ,size = 0.55)+
   theme_bw() +
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle('Percentage of Ai publications relative to AI publications eahc year')+
  xlab('Year')+
  ylab('Relative articles count [% articles]')+
  scale_x_discrete(breaks = scales::pretty_breaks(n = 15))   # Plotting every 2 years
  
# geom_smooth(method = "lm", se = FALSE , size = 2, alpha=0.9) +#
# geom_smooth(method = "lm", se = FALSE , size = 1.75,color='white') +#
# geom_smooth(method = "lm", se = FALSE , size = 0.55) 
relative_count
plot3 <- ggarrange(plotlist = list(total_count,relative_count), nrow=1, ncol=2, common.legend = T)
annotate_figure(plot3, top = text_grob("Top countries in AI publications", 
                                       size = 14))


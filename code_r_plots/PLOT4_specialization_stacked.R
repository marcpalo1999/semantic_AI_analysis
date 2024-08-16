data <- read.csv("raw_data3.csv")

library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)

# data$AlgorithmAcronym[grep('CNN',data$abstract)]<- 'CNN'
# data$AlgorithmAcronym[grep('CNN',data$AlgorithmType)]<- 'CNN'
# data$AlgorithmAcronym[grep('GAN',data$abstract)]<- 'GANs'
# data$AlgorithmAcronym[grep('GAN',data$AlgorithmType)]<- 'GANs'
# #data$AlgorithmType[grep('Neural Net',data$abstract)]<- 'NN'
# data$AlgorithmAcronym[grep('Neural Networks',data$AlgorithmType)]<- 'NN'
# data$AlgorithmAcronym[grep('RNN',data$abstract)]<- 'RNN'
# data$AlgorithmAcronym[grep('RNN',data$AlgorithmType)]<- 'RNN'
# data$AlgorithmAcronym[grep('LSTM',data$abstract)]<- 'LSTM'
# data$AlgorithmAcronym[grep('LSTM',data$AlgorithmType)]<- 'LSTM'
# 
# data$expl_group[grep('ML',data$expl_group)]<-'ML'
# data$expl_group[grep('CNN|GAN|RNN',data$AlgorithmAcronym)]<- 'DL'
# 
# dum5<-data%>%filter(expl_group=='ML')
# dum <- data
# dum <- dum[dum$expl_group %in% c('DL','ML'),]
# dum <- dum[dum$Country %in% c('US','EU','China'),]
# 
# counts <- dum %>%
#   group_by(year, Country, AlgorithmAcronym) %>%
#   summarise(Count = n()) %>%
#   ungroup()
# 
# 
# # Create the stacked plot
# a<-ggplot(counts, aes(x = as.factor(AlgorithmAcronym), # x-axis factor label
#                         
#                         # y-axis numerical parameter
#                         y = Count)) +	
#   
#   # the bar height will represent
#   # the actual value of the data
#   geom_bar(stat = "identity",
#            fill=alpha("green", 0.5)) + # define bar color
#   
#   # define size of inner circle
#   # and the size of the bar
#   ylim(-100,120) +
#   
#   # define the polar coordinate
#   coord_polar(start = 0)
# 
# # plot
# a
# 
# dum <- dum[dum$expl_group %in% c('DL','ML'),]
# dum <- dum[dum$Country %in% c('US','EU','China'),]
# 
counts <- dum %>%
   group_by(year, expl_group) %>%
   summarise(Count = n()) %>%
   ungroup()
 
# b<-ggplot(counts, aes(x = AlgorithmAcronym, y = Count, fill = Country)) +
#   geom_bar(position = 'fill',stat = "identity") +
#   labs(x = "AlgorithmAcronym", y = "Count", fill = "Country") +
#   theme_bw()+
#   ggtitle('Quantity ')
# b
# 
# c<-ggplot(counts, aes(x = year, y = Count, group = Country, color=Country)) +
#   geom_line(stat = "identity") +
#   facet_wrap(~AlgorithmAcronym,ncol = 2, scales = "free_y")+
#   
#   labs(x = "AlgorithmAcronym", y = "Count", fill = "Country") +
#   theme_bw()+
#   ggtitle('Quantity of publications about DL and its topic ')
# c
# 
# d<-ggplot(counts, aes(x = AlgorithmAcronym, y = Count, fill = Country)) +
#   geom_bar(position="dodge",stat = "identity") +
#   labs(x = "AlgorithmAcronym", y = "Count", fill = "Country") +
#   theme_bw()+
#   ggtitle('Quantity ')+
#   xlab('Key DL Algorithm Families')+
#   ylab('Count')
# d

ML_vs_DL<-ggplot(counts, aes(x = year, y = Count, group = expl_group, color = expl_group)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "loess", se = T , size = 1, alpha=0.2) +
  labs(x = "Year", y = "Count") +
  theme_bw()+
  ggtitle('Publications in DL and ML over the years.')+
  scale_colour_brewer(palette = 'Set1')

counts <- dum %>%
  group_by(year, expl_group, Country) %>%
  summarise(Count = n()) %>%
  ungroup()%>%
  filter(expl_group == 'DL')

DL_vs_country<-ggplot(counts, aes(x = year, y = Count, group = Country, color = Country)) +
  geom_point(stat = "identity") +
  geom_smooth(method = "loess", se = FALSE , size = 1, alpha=0.2) +
  labs(x = "Year", y = "Count") +
  theme_bw()+
  ggtitle('Publications in DL and country over the years.')

plot3 <- ggarrange(plotlist = list(ML_vs_DL,DL_vs_country), nrow=1, ncol=2)
annotate_figure(plot3, top = text_grob("Tendency in DL publications", 
                                       color = "#6CA6CD", size = 14))


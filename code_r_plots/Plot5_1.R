data <- read.csv("raw_data3.csv")

library(dplyr)
library(plotly)
library(ggplot2)
library("RColorBrewer")
library(ggpubr)

data$expl_group[grep('ML', data$expl_group)]<-'ML'
dum <- data
dum <- dum[dum$field %in% c('Disease detection','Disease course prediction','Disease etiology (Biomarker research)','Disease monitoring (KPIs monitoring )'),]
counts <- dum %>%
  group_by(year, field,expl_group) %>%
  summarise(Count = n()) %>%
  ungroup()

sumatory <- plyr::ddply(counts, c('year','field'), function(x) sum(x$Count))
counts <-merge(counts, sumatory, by = c('year','field'))
counts$relative_count <- counts$Count/counts$V1


ggplot(counts%>%filter(expl_group=='DL')  , aes(x = as.factor(year), y = relative_count, group = field, color = field)) +
  geom_line(size = 0.5, alpha = 0.7) +
  #scale_y_log10() +
  theme_bw() +
  scale_colour_brewer(palette = 'Set1')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  ggtitle('Relative publications of DL respect to total by field')

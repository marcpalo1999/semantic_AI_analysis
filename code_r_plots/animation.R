library(ggplot2)
library(gganimate)
dx<-read.csv('curated_embedding_data.csv')
head(dx)

# Nueva semantica!!! Problemes de comunicació, alineament

idx<-c(
  grep('Liniar', dx$expl_group),
  grep('ML', dx$expl_group),
  grep(c('DL'), dx$expl_group)
)
ids<-c(
  grep('ML', dx$expl_group),
  grep(c('DL'), dx$expl_group)
)
id<-c(
  grep('L', dx$expl_group)
)

id
# especializacion por año
plot <- ggplot(dx[ idx,], aes(x= PC1, y = PC2, col = factor(expl_group)))+
  geom_point(size = 0.05)+
  facet_wrap(~year)+
  theme_bw()+
  theme(legend.position ='bottom')+
  xlim(c(-20,20))+
  ylim(c(-20,20))+
  labs(color = 'Groups')+
  ggtitle('Creation of new semantics in AI')+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  labs(color = 'Groups',
       x = paste("PC1 (",round(var(dx[idx, ]$PC1) , 1), "% variance )"),
       y = paste("PC2 (",round(var(dx[idx, ]$PC2) , 1), "% variance )"))


plot2<-ggplot(dx[ idx,], aes(x= PC1, y = PC2, col = factor(expl_group)))+
  geom_point(size = 0.05)+
  facet_wrap(~year, nrow = 3, ncol = 9)+
  theme_bw()+
  xlim(c(-20,20))+
  ylim(c(-20,20))+
  labs(color = 'Groups')+
  ggtitle('Creation of new semantics')+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  scale_color_manual(values = c( "ML predictive models" = "green", "DL" = "blue"))

#PCs cuadrades, variança explicada

dx$expl_group[grep('Liniar/Logistic',dx$expl_group)]<- 'Medical statistics'
dx$expl_group[grep('ML',dx$expl_group)]<- 'ML'



# especializacion por año
ggplot(dx[ ids,], aes(x= PC1, y = PC2, col = factor(expl_group)))+
  geom_point(size = 0.05)+
  facet_wrap(~year, nrow = 3, ncol = 9)+
  theme_bw()+
  xlim(c(-20,20))+
  ylim(c(-20,20))





# especializacion por año
ggplot(dx[ idx,], aes(x= PC1, y = PC2, col = factor(field)))+
  geom_point(size = 0.15)+
  facet_wrap(~year, nrow = 3, ncol = 9)+
  theme_bw()+
  xlim(c(-20,20))+
  ylim(c(-20,20))


# especializacion por año
ggplot(dx[ idx,], aes(x= field, y = PC1, col = factor(field)))+
  geom_boxplot()+
  facet_wrap(~year, nrow = 3, ncol = 9)+
  theme_bw()


# especializacion por año
ggplot(dx[ idx,], aes(x= PC2, y = PC3, col = field))+
  geom_point(size = 0.05)+
  facet_wrap(~year, nrow = 3, ncol = 9)+
  theme_bw()+
  xlim(c(-20,20))+
  ylim(c(-20,20))



library(ggplot2)
library(gganimate)
library(gifski)
# Convert fill variable to character type
dx<-read.csv('curated_embedding_data.csv')
dx$expl_group <- as.character(dx$expl_group)

# Create the base plot
plot1 <- ggplot(dx[idx, ], aes(x = PC1, y = PC2, col = factor(expl_group))) +
  geom_point(size = 0.05) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  xlim(c(-20, 20)) +
  ylim(c(-20, 20)) +
  ggtitle('Creation of new semantics') +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(
    color = 'Groups',
    x = paste("PC1 (", round(var(dx[idx, ]$PC1), 1), "% variance )"),
    y = paste("PC2 (", round(var(dx[idx, ]$PC2), 1), "% variance )")
  )

base_plot <- plot1 + transition_time(year) +
  labs(title = "Semantic representation of articles \n published in {frame_time}")

base_plot





# Animate the plot
animate(base_plot, height = 4,
        width = 3.5, units = "in", res = 200,nframes = 27,fps = 1,end_pause = 10)
anim_save("embedding_gganimate.gif")
animated_plot


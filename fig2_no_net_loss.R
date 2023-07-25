
# Preamble ----------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(grid)
library(ggpubr)
library(readxl)
library(tidyverse)

import_public_sans()
# panel a -----------------------------------------------------------------

dx2 <- data.frame(Predictor = c(1:100, 1:100), 
                  Richness = c(seq(14, 11, length.out = 100), seq(45, 80, length.out = 100)),
                 Scale = c(rep("Small plots", 100), rep("Large plots", 100))) %>%
  filter(Predictor == 10 | Predictor == 90) %>% 
  mutate(stage = factor(rep(c("early-successional", 
                            "late-successional"), 2), 
                      levels = c("late-successional" ,
                                 "early-successional")  ))

data.segm1 <- data.frame(x=20,y=20,xend=12,yend=42,
                         Scale="Large plots")
data.segm2 <- data.frame(x=80,y=53,xend=90,yend=70,
                         Scale="Large plots")

data.frame(Predictor = c(1:100, 1:100), 
           Richness = c(seq(14, 11, length.out = 100), seq(45, 80, length.out = 100)),
           Scale = c(rep("Small plots", 100), rep("Large plots", 100))) %>% 
  ggplot(aes(x= Predictor, y = Richness)) +
  facet_wrap(~Scale)+
  geom_line(lwd = 1) +
  geom_point(data = dx2, aes(x = Predictor, y = Richness, col = stage, pch = Scale), size = 4) +
  
  geom_curve(data = data.segm1, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = -0.1, inherit.aes=FALSE) +
  geom_text(data= data.segm1, aes(x = 21, y = 19), 
            label = "early-successional", col = "#1ac899", fontface = 3,
            inherit.aes = F, hjust = 0, size = 4, family = "Arial Narrow") +
  
  
  geom_curve(data = data.segm2, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = 0.2, inherit.aes=FALSE) +
  
  geom_text(data= data.segm2, aes(x = 78, y = 50), 
            label = "late-successional", col = "#7e5dea", fontface = 3,
            inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow") +
  
  labs(x = "Time", y = "Mean species richness", 
       title = "Successional trends in local species richness are frequently positive, but likely scale-dependent") +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  scale_color_manual(values=c("#7e5dea", "#5deac5")) +
  theme(
    text = element_text(size = 14),
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position = "none",
    legend.title = element_blank() ,
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.line = element_line() ) +
  coord_cartesian(ylim=c(0, 90)) -> fig2a

# panel b -----------------------------------------------------------------

bind_rows(
  list(
    data.frame(Predictor = c(1:100, 1:100), 
               Richness = c(seq(14, 11, length.out = 100), seq(45, 80, length.out = 100)),
               Scale = c(rep("Plot level", 100), rep("Landscape level", 100))) %>%
      filter(Predictor == 10 | Predictor == 70) %>% 
      mutate(luc = factor(rep(c("disturbed", 
                                "natural"), 2), 
                          levels = c("natural" ,
                                     "disturbed")  )),
    
    data.frame(Predictor = c(1:100, 1:100), 
               Richness = c(seq(14, 11, length.out = 100), seq(45, 80, length.out = 100)),
               Scale = c(rep("Plot level", 100), rep("Landscape level", 100))) %>%
      filter(Predictor == 10 | Predictor == 70) %>%  
      mutate(luc = factor(rep(c("baseline survey", 
                                "resurvey"), 2)))
    
  )
) %>%
  mutate(type = rep(c("Space-for-time studies", "Temporal studies"), each = 4)) %>% 
  ggplot(aes(x= luc, y = Richness, group = Scale)) +
  facet_wrap(~factor(type, levels = c("Temporal studies", "Space-for-time studies")), scales = "free") +
  geom_line(lty = 3, size = 1) +
  geom_point(size = 4, aes( col = luc , pch = Scale)) +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14), axis.text.y = element_blank(), 
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()) +
  coord_cartesian(ylim=c(0, 80)) +
  scale_color_manual(values=c("#7e5dea", "#5deac5", "#5deac5", "#7e5dea"),guide = FALSE) +
  labs(y = "Mean species richness", x = " ", 
       title = "Succession predicts contrasting richness trends for temporal vs. space-for-time studies") -> fig2b


# panel c -----------------------------------------------------------------

# load Vellend et al 2013 data
d <- read_excel("Data/sd01.xls")
length(unique(d$Data_set))


# potentially exclude datasets with post disturbance...
dt <- d %>% 
  filter(SR_analysis == 1) %>% 
  filter(CT == "mean") %>% 
  #filter(Driver != "Post-disturbance") %>% 
  select(Data_set, Habitat, Plot_size_max, Duration, SR_Year1_CT, SR_Year2_CT, log_SR_ratio)
head(dt)

# Forest
# 133 dataset of 346 datasets in Vellend et al
dt %>% filter(Habitat == "Forest") %>% nrow
m <- lm(log_SR_ratio ~ Duration + log(Plot_size_max), data = dt %>% filter(Habitat == "Forest"))
summary(m)


dt %>% filter(Habitat == "Forest") %>% 
  ggplot(aes(y = log_SR_ratio, x = Plot_size_max) ) +
  geom_smooth(method = "lm", fill= "#7e5dea", col = "black", alpha = 0.2) +
  geom_point(size = 3, col = "#7e5dea", alpha =0.4) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_x_log10() +
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), 
           label.y = -1.3, p.accuracy = 0.001, r.accuracy = 0.01)+
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14), 
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()) +
  labs(y = "Change in\n species richness\n(log-ratios)", x = "Plot size (sqm)", 
       title = "In temporal studies, richness trends shift from negative to positive with increasing scale") -> fig2c


# plot together -----------------------------------------------------------
ggarrange(
  ggarrange(
    fig1a, fig1b, ncol = 1, labels = c("a", "b")),
  fig1c, nrow = 2, heights = c(2, 1),  labels = c("","c")
)
  

ggsave(bg = "white",
       "Figures/fig_nonetloss_pubs.png",
       width = 6.8,
       height = 6.52,
       dpi = 600
)

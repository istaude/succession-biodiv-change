
# Preamble ----------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(grid)
library(ggpubr)
library(showtext)
library(patchwork)
showtext_auto()
import_public_sans()
font_add_google("Roboto Condensed") #just run once


# panel a -----------------------------------------------------------------

dt1 <- data.frame( turnover = 100 * exp(-0.07 * (c(1:100)) ), 
                   predictor = c(1:100))
dt2 <- data.frame( turnover = 100 * exp(-0.07 * (c(1:100)) ), 
                   predictor = c(111:210))

ggplot() +
  geom_line(data = dt1, aes(x= predictor, y = turnover), lwd = 1) +
  geom_line(data = dt2, aes(x= predictor, y = turnover), lwd = 1) +
  geom_point(aes(x= 5, y = 70.46881), size = 4, col = "#5deac5") +
  geom_point(aes(x= 80, y = 0.3697864), size = 4, col = "#7e5dea") +
  
  geom_point(aes(x= 115, y = 70.46881), size = 4, col = "#5deac5") +
  geom_point(aes(x= 190, y = 0.3697864), size = 4, col = "#7e5dea") +

  annotate("segment", x = 105, xend = 105, y = 100, yend = 0, 
           col = "grey20", size = 1.4, 
           arrow = arrow(angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x=100, y=55, 
           label= "Disturbance", 
           angle =90,
           family = "Arial Narrow",
           fontface= "italic", 
           col = "grey20",
           size = 4) +
  geom_curve(aes(x = 75, y = 30, xend = 80, yend = 4),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = -0.1, inherit.aes=FALSE) +
  geom_text(aes(x = 75, y = 35), 
            label = "late-successional", col = "#7e5dea", fontface = 3,
            inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow") +
  
  geom_curve(aes(x = 20, y = 75, xend = 8, yend = 70.46881),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = -0.1, inherit.aes=FALSE) +
  geom_text(aes(x = 22, y = 75), 
            label = "early-successional", col = "#1ac899", fontface = 3,
            inherit.aes = F, hjust = 0, size = 4, family = "Arial Narrow") +
  
  labs(x = "Time", y = "Turnover rate", 
       title="Natural rates of temporal turnover during succession") +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  coord_cartesian(ylim=c(-3, 100)) +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14),
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.line = element_line() ) -> figa


# panel b -----------------------------------------------------------------

(ggplot()+
  geom_point(aes(y= .1, x = 1), size = 4, col = "#7e5dea") +
  geom_point(aes(y= 93, x = 2), size = 4, col = "#5deac5") +
  
  geom_point(aes(y= 60, x = 2), size = 4, col = "#ea5dc9", alpha =0.4) +
  
  
  annotate("segment", x = 1.2, xend = 1.2, y = 100, yend = 0, 
           col = "grey20", size = 1.4, 
           arrow = arrow(angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x=1.15, y=55, 
           label= "Disturbance", 
           angle =90,
           family = "Arial Narrow",
           fontface= "italic", 
           col = "grey20",
           size = 4) +
  
  annotate("segment", x = 2, xend = 2, y = 86, yend = 69, 
           col = "#ea5dc9", size = 1,
           arrow = arrow(length = unit(.12,"cm"))) +
  
  geom_curve(aes(x = 1.88, y = 83, xend = 1.97, yend = 93),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = -0.1, inherit.aes=FALSE) +
  geom_text(aes(x = 1.85, y =83 ), 
            label = "natural turnover rates\nof early-successional stages", 
            col = "#1ac899", fontface = 3,
            inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow") +
  
  
  geom_curve(aes(x = 1.88, y = 50, xend = 1.97, yend = 60),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = -0.1, inherit.aes=FALSE) +
   geom_text(aes(x = 1.85, y =40 ), 
            label = "global change turnover rates\nof early-successional stages?", 
            col = "#ea5dc9", fontface = 3,
            inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow") +
  
  coord_cartesian(ylim=c(-5, 110), xlim = c(0.95, 2)) +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14), axis.text.y = element_blank(), 
    axis.text.x = element_blank(),
    axis.line = element_line() ) +
  labs(y = "Turnover rate", x = "Successional stage", 
       title = "Natural vs. global change turnover rates") -> figb)





# panel c -----------------------------------------------------------------

dt <- data.frame(predictor = c(1:100), similarity = seq(0.8,0.2, length.out = 100))

ggplot() +
  geom_line(data = dt, aes(x= predictor, y = similarity), lwd = 1, lty = 2, 
            col = "black") +
  coord_cartesian(ylim=c(0, 1)) +
  labs(x = "Global change intensity", 
       y = "Turnover rate\nin early succession", 
       title = "Global change, slowed turnover rates?") +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14),
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.line = element_line() ) -> figc





# plot together -----------------------------------------------------------

# plot
figa /
  (figb + figc) + plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(family = 'Roboto Condensed'))

ggarrange(figa, labels = c("a"),
  ggarrange(figb, figc, ncol = 2, labels = c("b", "c")), nrow = 2)


ggsave(bg = "white",
       "Figures/fig_accturn_pubs.png",
       width = 7.7,
       height = 4.97,
       dpi = 1200
)


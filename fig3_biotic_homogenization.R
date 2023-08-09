
# Preamble ----------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(grid)
library(ggpubr)
import_public_sans()


# panel a -----------------------------------------------------------------
data.text <- data.frame(x = c(Inf, Inf), y = c(0,0), 
                        text = c(paste(sprintf('\u2192'),"Heterogenization"), 
                                 paste(sprintf('\u2192'),"Homogenization")), 
                        Scale = c("Plot level", "Landscape level"))


dx <- data.frame(Predictor = c(1:100, 1:100), 
                 Beta = c(seq(0.4, 0.8, length.out = 100), 
                          seq(0.8, 0.55, length.out = 100)),
                 Scale = c(rep("Plot level", 100), 
                           rep("Landscape level", 100))) %>%
  filter(Predictor == 10 | Predictor == 90) %>% 
  mutate(luc = factor(rep(c("disturbed\n(early-successional)", 
                            "natural\n(late-successional)"), 2), 
                      levels = c("natural\n(late-successional)" ,
                                 "disturbed\n(early-successional)")  ))

data.segm1 <- data.frame(x=30,y=0.9,xend=10,yend=0.8,
                         Scale="Landscape level")
data.segm2 <- data.frame(x=70,y=0.45,xend=90,yend=0.55,
                         Scale="Landscape level")

data.frame(Predictor = c(1:100, 1:100), 
           Beta = c(seq(0.4, 0.8, length.out = 100), seq(0.8, 0.55, length.out = 100)),
           Scale = c(rep("Plot level", 100), rep("Landscape level", 100))) %>% 
  ggplot(aes(x= Predictor, y = Beta)) +
  facet_wrap(~Scale) +
  geom_line(lwd = 1) +
  geom_point(data = dx, aes(x=Predictor, y=Beta, col = luc), size = 4) +
  geom_text(data = data.text, aes(x=x, y=y, label = text, col = text),
           fontface= 4,
           family = "Arial Narrow",
           size = 4,
           hjust = 1, vjust = -0.5) +
  geom_curve(data = data.segm1, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = 0.3, inherit.aes=FALSE) +
  geom_curve(data = data.segm2, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
             color = "grey30", curvature = 0.3, inherit.aes=FALSE) +
  geom_text(data= data.segm1, aes(x = 32, y = 0.9), 
            label = "early-successional", col = "#1ac899", fontface = 3,
            inherit.aes = F, hjust = 0, size = 4, family = "Arial Narrow") +
  geom_text(data= data.segm2, aes(x = 68, y = 0.45), 
            label = "late-successional", col = "#7e5dea", fontface = 3,
            inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow") +
  labs(x = "Time", y = "Beta diversity", 
       title = "Beta diversity between landscapes and between plots during succession") +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  coord_cartesian(ylim=c(0, 1)) +
  scale_color_manual(values=c("#5dea7e", "#ea5dc9", "#5deac5", "#7e5dea")) +
  theme(
    text = element_text(size = 14),
    plot.margin=unit(rep(0.2,4),"cm"),
    legend.position = "none",
    legend.title = element_blank() ,
    axis.text.x = element_blank(), axis.text.y = element_blank(),
    axis.line = element_line() ) -> fig3a




# panel b -----------------------------------------------------------------

data.text2 <- data.frame(x = c(Inf,Inf), y = c(0,0), 
                         text = c(paste(sprintf('\u2192'),"Homogenization"), 
                                  paste(sprintf('\u2192'),"Heterogenization")), 
                        Scale = c("Plot level", "Landscape level"))

data.frame(Predictor = c(1:100, 1:100), 
           Beta = c(seq(0.4, 0.8, length.out = 100), seq(0.8, 0.55, length.out = 100)),
           Scale = c(rep("Plot level", 100), rep("Landscape level", 100))) %>%
      filter(Predictor == 10 | Predictor == 90) %>% 
      mutate(luc = factor(rep(c("disturbed", 
                                "natural"), 2), 
                          levels = c("natural" ,
                                     "disturbed")  )) %>%
  ggplot(aes(x= luc, y = Beta, col = luc)) +
  facet_wrap(~Scale) +
  geom_point(size = 4) +
  geom_text(data = data.text2, aes(x=x, y=y, label = text, col =text),
            fontface= 4,
            family = "Arial Narrow",
            size = 4,
            hjust = 1, vjust = -0.5, inherit.aes = F) +
  theme_ipsum(axis_title_size = 12, 
              plot_title_size = 12, axis_title_just = "mm") +
  theme(
    plot.margin=unit(rep(0.2,4),"cm"),
    text = element_text(size = 14), axis.text.y = element_blank(), 
    axis.line = element_line(),
    legend.position = "none",
    legend.title = element_blank()) +
  scale_color_manual(values=c("#5dea7e", "#ea5dc9", "#5deac5", "#7e5dea")) +
  coord_cartesian(ylim=c(0, 1)) +
  labs(y = "Beta diversity", x = " ", 
       title = "Beta diversity change upon disturbance as predicted from succession") -> fig3b



# plot together -----------------------------------------------------------

ggarrange(
  fig3a, fig3b,  ncol = 1, nrow = 2, labels = c("a", "b"),
  font.label=list(color="black",
                  size=12,
                  family = "Arial Narrow")
)

showtext_opts(dpi=600)
ggsave(bg = "white",
  "Figures/fig_biohom_pubs.pdf",
  width = 5.67,
  height = 4.58,
  dpi = 600
)
showtext_opts(dpi=96)

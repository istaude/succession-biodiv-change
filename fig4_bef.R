
# Preamble ----------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(grid)
library(ggpubr)
library(gridExtra)
import_public_sans()

# Create data -------------------------------------------------------------

df1 <- data.frame(x = 0.02, y = 1.5, xend = 0.1, yend = 1.1)
df2 <- data.frame(x = 0.55, y = 0.55, xend = 0.45, yend = 1.2)
x <- seq(0, 1, length.out = 100)

(rbind(data.frame(functioning = seq(2, 0.4, length.out = 100), 
                  richness = dbeta(x, 2, 2) + 0.5, 
                  time = seq(0, 1, length.out = 100),
                  group = rep("Successional mechanisms alone", 100))
       ,
       data.frame(functioning = seq(2, 0.4, length.out = 100) + dbeta(x, 2, 2)*0.3, 
                  richness = dbeta(x, 2, 2) + 0.5, 
                  time = seq(0, 1, length.out = 100),
                  group = rep("with BEF mechanisms", 100))
) %>% 
    #pivot_longer(c(1,2), names_to = "axis") %>% 
    ggplot(aes(x = time, group = group, color = group)) +
    geom_line(aes(y= functioning, lty = group), lwd = 1) +
    geom_line(aes(y= richness), lwd = 1, color = "#4a1dde") +
    coord_cartesian(ylim=c(0, 2.5)) +
    scale_y_continuous(
      name = "Diversity",
      sec.axis = sec_axis( trans=~.*1, name="Productivity")) +
    geom_text(x=0.02, y=1.5, label = "Diversity",
              fontface= 4, col = "#4a1dde",
              family = "Arial Narrow",
              size = 4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    geom_text(x=0.58, y=0.4, label = "Productivity",
              fontface= 4, col = "#b1de1d",
              family = "Arial Narrow",
              size = 4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    geom_curve(data = df1, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = F,
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "#4a1dde", curvature = 0.2) +
    geom_curve(data = df2, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = F,
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "#b1de1d", curvature = -0.2) +
    theme_ipsum(axis_title_size = 12, 
                plot_title_size = 12, axis_title_just = "mm") +
    scale_color_manual(values=c("#b1de1d", "#b1de1d")) +
    scale_linetype_manual(values = c(1, 3)) +
    theme(
      text = element_text(size = 14),
      plot.margin=unit(rep(0.2,4),"cm"),
      legend.position = "none",
      legend.title = element_blank(),
      aspect.ratio = 1,
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.line = element_line() ) +
    xlab("Time") -> plot4a)





d2 <- rbind(data.frame(functioning = seq(2, 0.4, length.out = 100), 
                       richness = dbeta(x, 2, 2) + 0.5, 
                       time = seq(0, 1, length.out = 100),
                       group = rep("Successional mechanisms alone", 100))
            ,
            data.frame(functioning = seq(2, 0.4, length.out = 100) + dbeta(x, 2, 2)*0.3, 
                       richness = dbeta(x, 2, 2) + 0.5, 
                       time = seq(0, 1, length.out = 100),
                       group = rep("with BEF mechanisms", 100))
)


(ggplot(d2)+
    geom_smooth(data = d2 %>% 
                  filter(group == "Successional mechanisms alone") %>% 
                  filter(time <= 0.3), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#b1de1d", lty = 1) +
    geom_smooth(data = d2 %>% 
                  filter(group == "Successional mechanisms alone") %>% 
                  filter(time >= 0.7), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#b1de1d", lty = 1) +
    geom_smooth(data = d2 %>% 
                  filter(group == "with BEF mechanisms") %>% 
                  filter(time <= 0.3), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#b1de1d", lty = 3) +
    geom_smooth(data = d2 %>% 
                  filter(group == "with BEF mechanisms") %>% 
                  filter(time >= 0.7), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#b1de1d", lty = 3) +
    theme_ipsum(axis_title_size = 12, 
                plot_title_size = 12, axis_title_just = "mm") +
    coord_cartesian(ylim=c(0, 2.5)) +
    geom_text(x=0.54, y=2.1, label = "early-successional",
              fontface= 3, col = "black",
              family = "Arial Narrow",
              size = 3.4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    geom_text(x=0.54, y=.16, label = "late-successional",
              fontface= 3, col = "black",
              family = "Arial Narrow",
              size = 3.4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    theme(
      text = element_text(size = 14),
      plot.margin=unit(rep(0.2,4),"cm"),
      legend.position = "none",
      aspect.ratio = 1,
      legend.title = element_blank() ,
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.line = element_line() ) +
    scale_color_manual(values=c("#4a1dde", "#b1de1d")) +
    labs(x = "Diversity", y = "Productivity") -> plot4b)



# plot together -----------------------------------------------------------
shared_legend <- get_legend(plot4a)
(g <- ggarrange(plot4a, plot4b, align = "hv",
                legend.grob = shared_legend,
                labels = c("a", "b")))

ggsave(bg = "white",
       "Figures/fig_bef_pubs.png", g,
       width = 7.02,
       height = 3.86,
       dpi = 600
)



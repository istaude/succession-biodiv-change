
# Preamble ----------------------------------------------------------------
library(tidyverse)
library(cowplot)
library(hrbrthemes)
library(extrafont)
library(grid)
library(ggpubr)
library(gridExtra)

# Create data -------------------------------------------------------------

df1 <- data.frame(x = 0.02, y = 1.5, xend = 0.1, yend = 1.1)
df2 <- data.frame(x = 0.55, y = 0.55, xend = 0.45, yend = 1.2)
x <- seq(0, 1, length.out = 100)

(rbind(data.frame(functioning = seq(2, 0.4, length.out = 100), 
                  richness = dbeta(x, 2, 2) + 0.5, 
                  time = seq(0, 1, length.out = 100),
                  group = rep("Succession alone", 100))
       ,
       data.frame(functioning = seq(2, 0.4, length.out = 100) + dbeta(x, 2, 2)*0.3, 
                  richness = dbeta(x, 2, 2) + 0.5, 
                  time = seq(0, 1, length.out = 100),
                  group = rep("with BEF mechanisms", 100))
) %>% 
    #pivot_longer(c(1,2), names_to = "axis") %>% 
    ggplot(aes(x = time, group = group, color = group)) +
    geom_line(aes(y= functioning, lty = group), lwd = 1) +
    geom_line(aes(y= richness), lwd = 1, color = "#5dea7e") +
    coord_cartesian(ylim=c(0, 2.5)) +
    scale_y_continuous(
      name = "Diversity",
      sec.axis = sec_axis( trans=~.*1, name="Productivity")) +
    geom_text(x=0.02, y=1.5, label = "Diversity",
              fontface= 3, col = "#5dea7e",
              family = "Arial Narrow",
              size = 4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    geom_text(x=0.58, y=0.4, label = "Productivity",
              fontface= 3, col = "#ea5dc9",
              family = "Arial Narrow",
              size = 4,
              hjust = 0.1, vjust = -0.5, check_overlap = T ) +
    geom_curve(data = df1, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = F,
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey30", curvature = 0.2) +
    geom_curve(data = df2, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = F,
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey30", curvature = -0.2) +
    theme_ipsum(axis_title_size = 12, 
                plot_title_size = 12, axis_title_just = "mm") +
    scale_color_manual(values=c("#ea5dc9", "#ea5dc9")) +
    scale_linetype_manual(values = c(1, 3)) +
    theme(
      text = element_text(size = 14),
      plot.margin=unit(rep(0.2,4),"cm"),
      #legend.position = "top",
      legend.title = element_blank(),
      aspect.ratio = 1,
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.line = element_line() ) +
    guides(linetype = guide_legend(override.aes = list(color = "gray30"))) +
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



data.segm1 <- data.frame(x=1.1,y=2.3,xend=1.3,yend=1.85)
data.segm2 <- data.frame(x=1.4,y=0.25,xend=1.55,yend=0.9)


(ggplot(d2)+
    geom_smooth(data = d2 %>% 
                  filter(group == "Successional mechanisms alone") %>% 
                  filter(time <= 0.3), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#5deac5", lty = 1) +
    geom_smooth(data = d2 %>% 
                  filter(group == "Successional mechanisms alone") %>% 
                  filter(time >= 0.7), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#7e5dea", lty = 1) +
    geom_smooth(data = d2 %>% 
                  filter(group == "with BEF mechanisms") %>% 
                  filter(time <= 0.3), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#5deac5", lty = 3) +
    geom_smooth(data = d2 %>% 
                  filter(group == "with BEF mechanisms") %>% 
                  filter(time >= 0.7), aes(x = richness , y = functioning), 
                method = "lm", se = F, col = "#7e5dea", lty = 3) +
    theme_ipsum(axis_title_size = 12, 
                plot_title_size = 12, axis_title_just = "mm") +
    coord_cartesian(ylim=c(0, 2.5)) +
    geom_curve(data = data.segm1, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey30", curvature = -0.3, inherit.aes=FALSE) +
    geom_curve(data = data.segm2, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
               color = "grey30", curvature = 0.3, inherit.aes=FALSE) +
    
    geom_text(aes(x = 0.54, y = 2.32), 
              label = "early-successional", col = "#1ac899", fontface = 3,
              inherit.aes = F, hjust = 0, size = 4, family = "Arial Narrow",
              check_overlap = T) +
    geom_text(aes(x = 1.35, y = 0.2), 
              label = "late-successional", col = "#7e5dea", fontface = 3,
              inherit.aes = F, hjust = 1, size = 4, family = "Arial Narrow",
              check_overlap = T) +
    theme(
      text = element_text(size = 14),
      plot.margin=unit(rep(0.2,4),"cm"),
      legend.position = "none",
      aspect.ratio = 1,
      legend.title = element_blank() ,
      axis.text.x = element_blank(), axis.text.y = element_blank(),
      axis.line = element_line() ) +
    labs(x = "Diversity", y = "Productivity") -> plot4b)



# plot together -----------------------------------------------------------
shared_legend <- get_legend(plot4a)

(g <- ggarrange(plot4a, plot4b, align = "hv",
                legend.grob = shared_legend,
                font.label=list(color="black",
                                size=12,
                                family = "Arial Narrow"),
                labels = c("a", "b")))

annotate_figure(g, top = text_grob("Succession can modify the relationship between biodiversity and ecosystem function", 
                                   face = "bold", size = 14, 
                                   family = "Arial Narrow"
))


showtext_opts(dpi=600)
ggsave(bg = "white",
       "Figures/fig_bef_pubs.pdf",
       width = 6.84,
       height = 4.32,
       dpi = 600
)
showtext_opts(dpi=96)


########################################################################################################################
# Rotina {R} : Graficos de tendencia paralela 
# Data: 2022 
# Autor: Alvaro Roberio
# Website: alvaroroberio.github.io
########################################################################################################################

library(tidyverse)
library(foreign)
install.packages('did')
library(did)
data(mpdta)

df <- df_fundos_1

ggplot(df, aes(ANO, TNVBPN, color = first.treat)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2002) +
  theme_minimal()

df <-data.frame(Dbase)

df %>% group_by(ANO, treat, gc) %>% 
  summarise(TNVPM=mean(TNVPM)) -> GTNVPM

cols <- c("grey", "black")

GGTNVPM <- ggplot(GTNVPM, aes(x=ANO, y=TNVPM, color=gc )) +
  geom_line(size=1) +
  geom_vline(xintercept = 2000, size=1, line=2, linetype=2, color="black") +
  scale_x_continuous(breaks = seq(1996, 2015, 2)) +
  scale_y_continuous(breaks = seq(0, 18, 2)) +
  labs(
    x = "Ano",
    y = "",
    color= " ",
    title = "C. Taxa de prematuridade",
    subtitle = " ")+
    scale_color_manual(values = cols)+
  theme_classic() + theme(legend.position = "bottom") +
    annotate(geom="text", x=2002, y=18, label="Pós-tratamento",
           color="black") +
    annotate(geom="text", x=1998, y=18, label="Pré-tratamento",
           color="black")
GGTNVPM

grid.arrange(GGTNVBPN, GGTNVBAP, GGTNVPM, ncol=3)


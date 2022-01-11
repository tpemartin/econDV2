library(ggplot2)
library(showtext)
sysfonts::font_add_google('Noto Sans TC')
showtext_auto()
theme_set(
  theme(
    text=element_text(family = "Noto Sans TC")
  )+
    theme_classic()
)
library(econDV2)
gg <- list(
  dash = econDV2::ggdash,
  geom = econDV2::ggbrowse,
  aes = econDV2::ggaes#,
  # resize_image = econDV2::resize_image
)
library(dplyr)
library(readxl)
xfun::download_file("https://github.com/jilufu/110-1-Economic-Data-Visualization/blob/main/NYdata.xlsx?raw=true")
NYdata <- read_excel("NYdata.xlsx")
#View(NYdata)
library(stringr)
library(ggplot2)
NYdata1 <- NYdata
#NYdata1$Industry %>% factor() %>% levels()

replacement <- c(
  "and" = "&",
  "-" = "",
  "s$"=""
)

NYdata1$Industry<- str_replace_all(
  NYdata1$Industry,
  replacement
)

NYdata1[NYdata1=="Consumer FInance"] <- "Consumer Finance"
NYdata1[NYdata1=="Pharmaceuticals & Biotechnolog"] <- "Pharmaceuticals & Biotechnology"
NYdata1[NYdata1=="Technology Hardware & Equipme"] <- "Technology Hardware & Equipment"
NYdata1[NYdata1=="Technology Hardware & Equipmen"] <- "Technology Hardware & Equipment"

library(dplyr)
NYdata1 %>%
  count(Industry,sort = TRUE) -> plotdata
#View(plotdata)
plotdata[1:10,]->plotdata1

plotdata1$axis <- seq(1:10)



ggplot(plotdata1,
       aes(x=axis) )+
  geom_col(
    aes(y=n),
    fill="#1f78b4",
    width = 0.6 #input$w
  ) +
  geom_text(aes(
    label = str_pad(Industry,60,side="right"),
    y=0.5         ),
    color="white",
    size=4.85,#input$s
    vjust=0.5, #input$v
    hjust=-0.01#input$h
  )+
  scale_y_continuous(
    expand = expansion(0, 0),
    position = "right") +

  labs( title="The Industry of All Non-U.S. Issuers",
        subtitle="top10 (as of May 31, 2021)",
        caption="Source:https://www.nyse.com/index"
  )+
  theme(  plot.title =element_text( size=22 ),
          plot.subtitle =element_text( size=14 ),
          axis.text.y =element_blank(),
          axis.text.x = element_text( size=12 ),
          axis.line.x = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major.x = element_line(
            color = "grey"
          )
  )+ coord_flip( ) + scale_x_reverse()

clipr::read_clip()-> rprofile_content

econDV2::rprofile_content
library(ggplot2)
library(showtext)
library(econDV2)
sysfonts::font_add_google('Noto Sans TC')
showtext_auto()
theme_set(
  theme(
    text=element_text(family = "Noto Sans TC")
  )+
    theme_classic()
)
gg <- list(
  dash = econDV2::ggdash,
  geom = econDV2::ggbrowse,
  aes = econDV2::ggaes
)

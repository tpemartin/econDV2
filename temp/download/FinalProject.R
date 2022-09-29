## ---------------------------
library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(stringr)
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

## ---------------------------
#引入資料~
##用INSERT EXCEL導進GITHUB的DATA，這樣老師比較方便~

finalProj <- list()
finalProj$BureauEnergy$source <- "https://www.moeaboe.gov.tw/ECW/populace/content/SubMenu.aspx?menu_id=3010"

#台灣能源總消費量
##單位：公秉油當量kloe, kiloliter of oil equivalent
# url <- "https://github.com/hereisjulia/R_Visualization/blob/main/Tasks/Final%20Project/DomesticEnergyConsumption.xlsx?raw=true"
destfile <- "DomesticEnergyConsumption.xlsx"
# curl::curl_download(url, destfile)
finalProj$BureauEnergy$data$Dom_Energy_Consumption <- read_excel(destfile, skip = 1)

#台灣進口能源比例
##單位：公秉油當量
# url <- "https://github.com/hereisjulia/R_Visualization/blob/main/Tasks/Final%20Project/ImpEnergySource.xlsx?raw=true"
destfile <- "ImpEnergySource.xlsx"
# curl::curl_download(url, destfile)
finalProj$BureauEnergy$data$Imp_EnergySource<- read_excel(destfile, skip = 1)

#台灣對進口能源依賴比率
# url <- "https://github.com/hereisjulia/R_Visualization/blob/main/Tasks/Final%20Project/Import_Reliance.xlsx?raw=true"
destfile <- "Import_Reliance.xlsx"
# curl::curl_download(url, destfile)
finalProj$BureauEnergy$data$Imp_EnergyRelience <- read_excel(destfile, skip = 1)

#View(finalProj$BureauEnergy$data$Imp_EnergyRelience)


## ---------------------------
pivot_longer(
  finalProj$BureauEnergy$data$Dom_Energy_Consumption,
  cols = -c(1),
  names_to = "month",
  values_to = "Consump_kloe"
) -> finalProj$BureauEnergy$data$Dom_Energy_Consumption


## ---------------------------
paste(finalProj$BureauEnergy$data$Dom_Energy_Consumption$month, "-01") -> finalProj$BureauEnergy$data$Dom_Energy_Consumption$month
lubridate::ymd(finalProj$BureauEnergy$data$Dom_Energy_Consumption$month) -> finalProj$BureauEnergy$data$Dom_Energy_Consumption$month


## ---------------------------
pivot_longer(
  finalProj$BureauEnergy$data$Imp_EnergySource,
  cols = -c(1),
  names_to = "month",
  values_to = "Import_kloe"
) -> finalProj$BureauEnergy$data$Imp_EnergySource

## ---------------------------
#轉date
paste(finalProj$BureauEnergy$data$Imp_EnergySource$month, "-01") -> finalProj$BureauEnergy$data$Imp_EnergySource$month
lubridate::ymd(finalProj$BureauEnergy$data$Imp_EnergySource$month) -> finalProj$BureauEnergy$data$Imp_EnergySource$month


## ---------------------------
names(finalProj$BureauEnergy$data$Imp_EnergySource)[[1]] <- "EnergySource"
#View(finalProj$BureauEnergy$data$Imp_EnergySource)


## ---------------------------
pivot_longer(
  finalProj$BureauEnergy$data$Imp_EnergyRelience,
  cols = -c(1),
  names_to = "month",
  values_to = "relience"
) -> finalProj$BureauEnergy$data$Imp_EnergyRelience

## ---------------------------
#轉date
paste(finalProj$BureauEnergy$data$Imp_EnergyRelience$month, "-01") -> finalProj$BureauEnergy$data$Imp_EnergyRelience$month
lubridate::ymd(finalProj$BureauEnergy$data$Imp_EnergyRelience$month) -> finalProj$BureauEnergy$data$Imp_EnergyRelience$month


## ---------------------------
#先確定一下資料格式正確：month是date，consump是numeric
finalProj$BureauEnergy$data$Dom_Energy_Consumption$month|>class()
finalProj$BureauEnergy$data$Dom_Energy_Consumption$Consump_kloe |>class()


## ---------------------------
#先確定一下資料格式正確：month是date，Import_kloe量是numeric
finalProj$BureauEnergy$data$Imp_EnergySource$month|>class()
finalProj$BureauEnergy$data$Imp_EnergySource$Import_kloe|>class()


## ---------------------------
finalProj$plot1 <- list()
finalProj$plot1$Dom_Energy_Consumption <- function(){
  geom_area(
    data = finalProj$BureauEnergy$data$Dom_Energy_Consumption,
    mapping = aes(
      x=month,
      y=Consump_kloe
    ),
    fill = "#5CADAD"
)
}
finalProj$plot1$Imp_EnergySource <- function(){
  geom_area(
    data = finalProj$BureauEnergy$data$Imp_EnergySource,
    mapping = aes(
      x=month,
      y=Import_kloe,
      group = EnergySource,
      fill= EnergySource,
      alpha=0.5
    ),
    position= "stack"
  )}
finalProj$plot1$Imp_EnergyRelience <- function(){
  geom_line(
    data = finalProj$BureauEnergy$data$Imp_EnergyRelience,
    mapping = aes(
      x= month,
      y= transferInv_leftbreaks(relience)
    ),
    size=1,
    color="#272727"
  )
}
view(finalProj$BureauEnergy$data$Imp_EnergyRelience)


## ---------------------------
#調整右軸的位置
transfer_leftBreaks = function(breaks){
      scales::rescale(breaks,
        from=c(0,8200000), to=c(90,100))
}
#調整要放在右軸的資料
transferInv_leftbreaks = function(breaks){
  scales::rescale(breaks,
        to=c(0,8200000), from=c(90,100))
}


finalProj$plot1$scale_y_area <- function(...){
  scale_y_continuous(
    name="Energy Consumption(kloe)",
    limits = c(0, 8200000),
    breaks = seq(0, 8200000, by=2000000),
    labels = seq(0, 8200000, by=2000000),
    sec.axis = sec_axis(name = "Import Relience",
                        trans = transfer_leftBreaks,
                        breaks = (seq(90,100, by=2)))
  )
}


## ---------------------------
finalProj$plot1$twoplot <- function(){
  ggplot()+
    finalProj$plot1$Dom_Energy_Consumption()+
    finalProj$plot1$Imp_EnergySource()+
    finalProj$plot1$Imp_EnergyRelience()+
    finalProj$plot1$scale_y_area()+
    scale_fill_discrete(name = "能源種類")
}

##未加入雙軸前，能源種類都有被呈現出來
finalProj$plot1$Imp_EnergySource |> body()
newImp_EnergySource = function(){
  list(
    geom_area(data = finalProj$BureauEnergy$data$Imp_EnergySource,
      mapping = aes(x = month, y = Import_kloe, group = EnergySource,
        fill = EnergySource, alpha = 0.5), position = "stack")
  )
}
ggplot() + newImp_EnergySource()

# stop(
#   ggplot()+finalProj$plot1$Imp_EnergySource()
# )
# # finalProj$plot1$Imp_EnergySource 裡的 data並不是data.frame




##加入後，有幾個被消失了
finalProj$plot1$twoplot()


## ---------------------------
finalProj$IEA$source <- "https://www.iea.org/data-and-statistics/data-browser?country=TAIPEI&fuel=Renewables%20and%20waste&indicator=SDG72"
ReNew_World <- read_csv("https://raw.githubusercontent.com/hereisjulia/R_Visualization/main/Tasks/Final%20Project/Renewable%20share%20in%20final%20energy%20consumption%20(SDG%207.2)%20%20-%20World.csv", skip = 3)
Renew_Taiwan <- read_csv("https://raw.githubusercontent.com/hereisjulia/R_Visualization/main/Tasks/Final%20Project/Renewable%20share%20in%20final%20energy%20consumption%20(SDG%207.2)%20%20-%20Chinese%20Taipei.csv", skip = 3)
ReNew_EU <- read_csv("https://raw.githubusercontent.com/hereisjulia/R_Visualization/main/Tasks/Final%20Project/Renewable%20share%20in%20final%20energy%20consumption%20(SDG%207.2)%20%20-%20European%20Union%20-%2028.csv",skip=3)
ReNew_USA <- read_csv("https://raw.githubusercontent.com/hereisjulia/R_Visualization/main/Tasks/Final%20Project/Renewable%20share%20in%20final%20energy%20consumption%20(SDG%207.2)%20%20-%20United%20States.csv", skip = 3)
ReNew_China <- read_csv("https://raw.githubusercontent.com/hereisjulia/R_Visualization/main/Tasks/Final%20Project/Renewable%20share%20in%20final%20energy%20consumption%20(SDG%207.2)%20%20-%20China%20(People's%20Republic%20of%20China%20and%20Hong%20Kong%20China).csv", skip=3)


## ---------------------------
quickMerge <- function(df1, df2){
  names(df1)[[1]] <- "Year"
  names(df2)[[1]] <- "Year"
  a <- merge(x=df1, y=df2, by= "Year")
  select(a, -contains("Units"))
}
a <- quickMerge(ReNew_World,Renew_Taiwan)
a <- quickMerge(a, ReNew_EU)
names(a) <- c("Year", "World", "Taiwan", "EU")
a <- quickMerge(a, ReNew_USA)
a <- quickMerge(a, ReNew_China)
names(a) <- c("Year", "World", "Taiwan", "EU", "USA","China")
finalProj$IEA$data$ReNew_EnCon <-a

## ---------------------------
pivot_longer(
  finalProj$IEA$data$ReNew_EnCon,
  cols = -c(1),
  names_to = "Country",
  values_to = "Renewable share"
) -> finalProj$IEA$data$ReNew_EnCon


## ---------------------------
lineColor= c("#3C3C3C", "#3C3C3C", "#4F9D9D", "#3C3C3C", "#3C3C3C")

ggplot()+
  geom_line(
    data = finalProj$IEA$data$ReNew_EnCon,
    mapping = aes(
      x=Year,
      y=`Renewable share`,
      group = Country,
      color = Country
      ),
    size =1
  )+
  scale_x_continuous(
    breaks= seq(1990, 2020, by=5),
    labels = seq(1990, 2020, by=5)
  )


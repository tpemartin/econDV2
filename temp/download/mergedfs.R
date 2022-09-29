library(dplyr)
finalProj$BureauEnergy$data$Dom_Energy_Consumption |>
  select(month, Consump_kloe) |>
  rename(
    x=month, y=Consump_kloe
  ) |>
  mutate(
    source="Dom_Energy_Consumption"
  ) -> .df1
finalProj$BureauEnergy$data$Imp_EnergySource |>
  select(month, Import_kloe) |>
  rename(
    x=month, y=Import_kloe
  ) |>
  mutate(
    source="Imp_EnergySource"
  ) -> .df2

dplyr::bind_rows(
  .df1, .df2
) -> .dfall
View(.dfall)
ggplot(data=.dfall)+
  geom_area(
    aes(x=x, y=y, fill=source)
  ) +
  scale_fill_manual(
    limits=c("Dom_Energy_Consumption", "Imp_EnergySource"),
    values=c("pink", "green")
  )

geom_area(
  data = finalProj$BureauEnergy$data$Dom_Energy_Consumption,
  mapping = aes(
    x=month,
    y=Consump_kloe
  ),
  fill = "#5CADAD"
)
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
)

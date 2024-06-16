dat <- computeTable(tempDirekt, Generation)

dat <- dat |> select(-Prozent)

tab <- dat |> 
  mutate(Vorfahren = c((2^(1:14)), sum(2^(1:14)))) |> 
  mutate(Prozent = Anzahl / Vorfahren) |> 
  mutate(Prozent = Prozent |> percent(accuracy = 0.1, decimal.mark = ","))

tab <- tab |> 
  add_column(
    computeTable(dataDIREKT, Generation) |> 
      select(Anzahl) |> 
      rename(`Anzahl Personen IST` = Anzahl),
    .after = "Anzahl"
  ) |> 
  rename(`Anzahl Vorfahren IST` = Anzahl,
         `Anzahl Vorfahren SOLL` = Vorfahren,
         `Anteil Vorfahren IST an SOLL in %` = Prozent)

#
temp <- dataDIREKT |> 
  group_by(Generation) |> 
  summarise(von = min(year(GEBURTSDATUM), na.rm = TRUE),
            bis = max(year(TODESDATUM), na.rm = TRUE)) |> 
  suppressWarnings() |> 
  mutate_if(is.numeric, list(~na_if(., Inf))) |> 
  mutate_if(is.numeric, list(~na_if(., -Inf)))

# und jetzt einfügen
tab <- tab |> left_join(temp) |> 
  relocate(c(von, bis), .after = Generation)

tab[nrow(tab), 6:7] <- NA

tab

# ..............................................................................
# Grafik logarithmisch?
dat <- tab |> 
  filter(Generation != "Total") |> 
  select(Generation, `Anzahl Vorfahren IST`, `Anzahl Vorfahren SOLL`) |> 
  pivot_longer(cols = 2:3, names_to = "Variable", values_to = "Anzahl")



#dat$Variable <- factor(dat$Variable)

ggplot(dat, aes(y = Generation, x = Anzahl, fill = Variable, group = Variable)) + 
  geom_bar_interactive(stat = "identity", position = position_dodge(), 
                           aes(tooltip = str_c(Generation, ": n = ", Anzahl))) +
  scale_y_discrete(limits=rev, name = "Generation / Ebene") +
  scale_x_log10(expand = expansion(mult = c(0, 0.5))) +
  scale_fill_manual(values = c("#08519C", "grey"))+
  geom_text(aes(label = Anzahl), position = position_dodge(width = 1),
            vjust = 0.3, hjust = -0.5, size = 8, size.unit = "pt") +
  ggtitle("Anzahl direkter Vorfahren nach Generation",
          subtitle = "Soll-Ist-Vergleich (Log-Skala)") +
  myTheme +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(size = 8, color = "#5a5a5a"),
        legend.position = "top",
        legend.text = element_text(size = 8),
        legend.key.size = unit(1, "line"))

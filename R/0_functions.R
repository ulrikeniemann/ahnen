# ..............................................................................
#
# Ahnen
#
# functions
#
# ..............................................................................

# functions für hübsche Tabelle
computeTable <- function(data = data, var) {
  data |> 
    group_by({{var}}) |> 
    summarise(Anzahl = n()) |> 
    mutate(Prozent = (Anzahl/sum(Anzahl))) |> 
    adorn_totals("row") |> 
    mutate(Prozent = Prozent |> percent(accuracy = 0.1, decimal.mark = ",")) |> 
    mutate({{var}} := factor({{var}}) |> fct_inorder())
}
getTable <- function(tab) {
  tab |> 
    kable(align = c("l", "c", "c")) |> 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = F) |> 
    row_spec(dim(tab)[1], bold = T)
} 

# theme ggplot
myTheme <- theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8, color = "#5a5a5a"),
    axis.title.y = element_text(size = 8, color = "#5a5a5a"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 10, face="bold", color = "#5a5a5a",
                              margin = margin(0, 0, 10, 0)),
    plot.title.position = "plot"
  )

# ..............................................................................
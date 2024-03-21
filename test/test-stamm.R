

dataDIREKT <- data |> filter(Vorfahren == "Direkt")


dataDIREKT <- dataDIREKT |> 
  mutate(Stamm = 
           case_when(
             KekuleNr %in% 2^(0:15) ~ "Wolfgang",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% 2^(0:15) ~ "Wolfgang",
             KekuleNr %in% (5*2^(0:15)) ~ "Wolfgang",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% (5*2^(0:15)) ~ "Wolfgang",
             KekuleNr %in% (9*2^(0:15)) ~ "Wolfgang",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% (9*2^(0:15)) ~ "Wolfgang",
             #
             KekuleNr %in% (3*2^(0:15)) ~ "Renate",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% (3*2^(0:15)) ~ "Renate",
             KekuleNr %in% (7*2^(0:15)) ~ "Renate",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% (7*2^(0:15)) ~ "Renate",
             KekuleNr %in% (11*2^(0:15)) ~ "Renate",
             #KekuleNr > 3 & KekuleNr %% 2 == 1 & (KekuleNr-1) %in% (11*2^(0:15)) ~ "Renate"
 
           ), .after = KekuleNr)




2^(1:15) # Wolfgang
3*2^(1:15) # Renate

5*2^(0:15) # Wolfgang
7*2^(0:15) # Renate

9*2^(0:15) # Wolfgang
11*2^(0:15) # Wolfgang


dataDIREKT$KekuleNr %% 2 == 1
(dataDIREKT$KekuleNr-1) %in% 2^(0:15)

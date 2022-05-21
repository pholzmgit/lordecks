get_faction_lut <- function(){

  data.frame(
    "Version"  = c(1, 1, 1, 1, 1, 1, 2, 2, 3, 4, 5),
    "Integer_Identifier" = c(0, 1, 2, 3, 4, 5, 6, 9, 7, 10, 12),  #not ordered!
    "Faction_Identifier" = c("DE", "FR", "IO", "NX",
                             "PZ", "SI", "BW", "MT",
                             "SH", "BC", "RU"),
    "Faction_Name" = c("Demacia", "Freljord", "Ionia", "Noxus",
                       "Piltover & Zaun", "Shadow Isles", "Bilgewater", "Mount Targon",
                       "Shurima", "Bandle City", "Runeterra")
  )

}

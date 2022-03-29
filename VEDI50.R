require(tidyverse)
require(wordbankr)

vd <- read.csv("VEDI50.csv", header=T)

vedi_ws <- get_instrument_data(language = "English (American)",
                                   form = "WS",
                                   #items = items,
                                   administrations = TRUE,
                                   iteminfo = TRUE) %>%
  filter(definition %in% vd$definition)

vedi_aoas <- fit_aoa(vedi_ws)

# for comprehension:
vedi_aoas_comp <- fit_aoa(vedi_ws, measure="understands")

write.csv(vedi_aoas, "VEDI_AoAs.csv")
write.csv(vedi_aoas_comp, "VEDI_AoAs_comprehension.csv") 

vedi_aoas$aoa == vedi_aoas_comp$aoa
# exactly the same...?
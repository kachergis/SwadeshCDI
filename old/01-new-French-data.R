lang = "French (French)"
load(paste("data/WS/",lang,"_WS_data.Rdata", sep=''))
rm(d_demo, d_prod, d_long)
# colnames(d_prod) = paste("item_", 1:690)

# add new French WS data that is not yet in Wordbank (but stabilizes the French IRT paramters)
load("data/fr_WG_WS_combined_data.Rds")
# d_demo and d_wide - how does d_wide have 694 columns?

d_demo <- d_demo %>% filter(form=="WS") %>% # 1410 subjects
  select(data_id, age, sex, production) %>%
  mutate(language="French (French)")
d_wide <- d_wide %>% filter(is.element(data_id, d_demo$data_id)) 

#d_wide$data_id = NULL
d_wide$attends = NULL # WG word
d_wide$veux = NULL
d_wide$dedans = NULL

colnames(d_wide)[which(colnames(d_wide)=="aïe")] = "aie"
colnames(d_wide)[which(colnames(d_wide)=="allô")] = "allo"
colnames(d_wide)[which(colnames(d_wide)=="bêe bêe")] = "bêê bêê"
colnames(d_wide)[which(colnames(d_wide)=="coin-coin")] = "coin coin"
colnames(d_wide)[which(colnames(d_wide)=="miam-miam")] = "miam miam"
colnames(d_wide)[which(colnames(d_wide)=="ohoh")] = "oh oh"
colnames(d_wide)[which(colnames(d_wide)=="ouaf-ouaf")] = "ouaf ouaf"
colnames(d_wide)[which(colnames(d_wide)=="s'il te plaît")] = "s'il te plait"
colnames(d_wide)[which(colnames(d_wide)=="pingouin")] = "pinguoin"
colnames(d_wide)[which(colnames(d_wide)=="carotte")] = "carottes"
colnames(d_wide)[which(colnames(d_wide)=="Centre-ville")] = "centre-ville"
colnames(d_wide)[which(colnames(d_wide)=="doigt de pied")] = "doigt de pieds"
colnames(d_wide)[which(colnames(d_wide)=="plein/ne")] = "plein/e"
colnames(d_wide)[which(colnames(d_wide)=="par dessus")] = "par-dessus"
colnames(d_wide)[which(colnames(d_wide)=="bâton")] = "baton"
colnames(d_wide)[which(colnames(d_wide)=="traîneau")] = "traineau"
colnames(d_wide)[which(colnames(d_wide)=="grand")] = "grand/e"
colnames(d_wide)[which(colnames(d_wide)=="sec/che")] = "sec/he"
colnames(d_wide)[which(colnames(d_wide)=="vagin/zezette…")] = "vagin/zezette"
colnames(d_wide)[which(colnames(d_wide)=="faire du vélo/de la moto")] = "faire du vélo/moto"
colnames(d_wide)[which(colnames(d_wide)=="serpillière")] = "serpillère"
colnames(d_wide)[which(colnames(d_wide)=="fou/folle")] = "fou/lle"
colnames(d_wide)[which(colnames(d_wide)=="mignon/ne")] = "mignon/e"
colnames(d_wide)[which(colnames(d_wide)=="donner un coup de pied")] = "donner un coup"
colnames(d_wide)[which(colnames(d_wide)=="figure")] = "figure/visage"
colnames(d_wide)[which(colnames(d_wide)=="le/la/les/quel(les)")] = "le/la/les/quelles"
colnames(d_wide)[which(colnames(d_wide)=="nom de l'animal domestique")] = "nom de l'animal"
colnames(d_wide)[which(colnames(d_wide)=="station service")] = "station-service"
colnames(d_wide)[which(colnames(d_wide)=="copain/ine")] = "copain/e"
colnames(d_wide)[which(colnames(d_wide)=="dégoûtant/e")] = "dégoutant/e"
colnames(d_wide)[which(colnames(d_wide)=="les courses")] = "faire les courses"
colnames(d_wide)[which(colnames(d_wide)=="Il")] = "il"

# WS duplicates:
# aller, dehors, eau, faire, goûter, orange, parc, poisson, pot, sucette 
items <- items %>% mutate(item_definition = case_when(
  item_definition=="dent" & category=="body_parts" ~ "dent (body part)",
  item_definition=="parc" & category=="places" ~ "parc (place)",
  item_definition=="aller" & category=="action_words" ~ "aller (verb)",
  item_definition=="dehors" & category=="places" ~ "dehors (place)",
  item_definition=="eau" & category=="food_drink" ~ "eau (drink)",
  item_definition=="faire" & category=="action_words" ~ "faire (verb)",
  item_definition=="goûter" & category=="action_words" ~ "goûter (verb)",
  item_definition=="orange" & category=="food_drink" ~ "orange (food)",
  item_definition=="poisson" & category=="food_drink" ~ "poisson (food)",
  item_definition=="pot" & category=="furniture_rooms" ~ "pot (furniture)", # also household...actual duplicate?
  item_definition=="sucette" & category=="food_drink" ~ "sucette (food)",
  TRUE ~ item_definition
))


length(intersect(items$item_definition, colnames(d_wide)))
# 690 match 

setdiff(items$item_definition, colnames(d_wide)) # --
setdiff(colnames(d_wide), items$item_definition) # --

d_long <- d_wide %>% pivot_longer(2:691, names_to="item_definition", values_to="produces")
d_long <- d_long %>% left_join(items)


d_prod <- d_long %>% select(data_id, item_id, produces) %>%
  pivot_wider(id_cols = data_id, names_from = item_id, 
              values_from = produces) %>%
  data.frame() 

rownames(d_prod) = d_prod$data_id
d_prod$data_id = NULL
d_prod <- d_prod %>% data.matrix

save(d_demo, items, d_long, d_prod, 
     file=paste("data/WS/",lang,"_WS_data.Rdata", sep=''))
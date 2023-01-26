library(wordbankr)
library(tidyverse)


# run once to save wordbank data in data/WG and data/WS
# ToDo: re-run using form_type to get WS/WG forms that don't have form=="WG"/"WS"

instr <- wordbankr::get_instruments() %>% 
  #filter(form=="WG") %>%
  arrange(desc(unilemma_coverage))



# MB@home: wide age range, diff items for different age ranges?

get_wg_data <- function(language, save=T, form="WG") {
  d_demo <- 
    get_administration_data(language = language, form = form) 
  
  items <- get_item_data(language = language, form=form) %>%
    filter(item_kind=="word") # wordbankr update: 'type' now 'item_kind'
  
  d_long_wg <- get_instrument_data(language = language, form = form) %>% # 418 items
    left_join(items %>% select(-complexity_category), by="item_id") %>%
    filter(item_kind=="word") # wordbankr update: 'num_item_id' now 'item_id'
  
  if(!"" %in% unique(d_long_wg$value)) print(paste("No blank responses in",language,"-- replace NAs with ''?"))
  
  # ToDo: are Danish and Norwegian missing values fixed?
  if(language=="Danish" | language=="Norwegian") {
    d_long_wg <- d_long_wg %>% mutate(value = replace_na(value, ""))
  }
  
  d_long_wg <- d_long_wg %>%
    mutate(produces = as.numeric(value == "produces"),
           comprehends = as.numeric(value == "understands"))
  print(paste("retrieved data for",length(unique(d_long_wg$data_id)),language,"participants"))
  #table(d_long_wg$value)
  
  # negative correlation (because if you produce, you don't understand and vice-versa?)
  #d_long_wg %>% group_by(data_id) %>% 
  #  summarise(produces = sum(produces, na.rm=T), comprehends = sum(comprehends, na.rm=T)) %>%
  #  ggplot(aes(x=produces, y=comprehends)) + geom_point()
  
  d_prod <- d_long_wg %>% select(data_id, item_id, produces) %>%
    pivot_wider(id_cols = data_id, names_from = item_id, 
                values_from = produces) %>%
    data.frame() 
  
  rownames(d_prod) = d_prod$data_id
  d_prod$data_id = NULL
  d_prod <- d_prod %>% data.matrix
  
  d_comp <- d_long_wg %>% select(data_id, item_id, comprehends) %>%
    pivot_wider(id_cols = data_id, names_from = item_id, 
                values_from = comprehends) %>%
    data.frame()
  
  rownames(d_comp) = d_comp$data_id
  d_comp$data_id = NULL
  d_comp <- d_comp %>% data.matrix
  
  d_comp <- d_prod + d_comp # anything you produce, you also comprehend
  
  if(save) save(d_demo, items, d_long_wg, d_prod, d_comp,
                file=paste("data/WG/",language,"_WG_data.Rdata", sep=''))
}


get_wg_data("English (British)", form="Oxford CDI") # 1515
get_wg_data("Mandarin (Beijing)", form="IC") # 230

# get_wg_data("Cantonese") # nrow(instruments) not greater than 0

# not many ASL subjects, but how much do the forms overlap? can we combine..?
#get_wg_data("American Sign Language", form="FormA") # 6 subjects..
get_wg_data("American Sign Language", form="FormBTwo") # 20 subjects
#get_wg_data("American Sign Language", form="FormBOne") # 19 subjects
#get_wg_data("American Sign Language", form="FormC") # 18 subjects

# add these to languages for IRT fits
langs_different_forms = c("English (British)", "Mandarin (Beijing)", "American Sign Language")

# generalization test: try using proposed short lists on Portuguese 
# ToDo: code uni_lemmas for short list

# do real-data simulations of CATs for each language

languages = c("Kigiriama", "Kiswahili", "British Sign Language",
              "Croatian","Danish","English (American)",
              "Italian","Mandarin (Taiwanese)","French (French)", 
              "Korean", "Latvian", "Hebrew", "Norwegian", "French (Quebecois)",
              "Slovak", "Spanish (European)", "Spanish (Mexican)", "Swedish",
              "Russian", "Turkish", "Portuguese (European)", 
              "Dutch", "Spanish (Chilean)", "Persian", "Spanish (Peruvian)") # last 4 are new in WB2
# 25 langs here, but have 29 downloaded in WG folder

# get_wg_data("Spanish (Chilean)")
# "No blank responses in Spanish (Chilean) -- replace NAs with ''?"
# "retrieved data for 74 Spanish (Chilean) participants"

# no WG: Cantonese
for(lang in languages) {
  tryCatch(get_wg_data(lang), 
           error=function(e) {
             message(lang)
             message(e)
           })
}

# now WS data
#wg_langs <- instr %>% filter(form=="WG")
ws_langs <- instr %>% filter(form=="WS") 
# form_type=="WS" -> 43, but some redundant langs: let's start with true WS


get_ws_data <- function(language, save=T, form="WS") {
  d_demo <- 
    get_administration_data(language = language, form = form) 
  
  items <- get_item_data(language = language, form=form) %>%
    filter(item_kind=="word") # wordbankr update: 'type' now 'item_kind'
  
  d_long <- get_instrument_data(language = language, form = form) %>% # 418 items
    left_join(items %>% select(-complexity_category), by="item_id") %>%
    filter(item_kind=="word") # wordbankr update: 'num_item_id' now 'item_id'
  
  if(!"" %in% unique(d_long$value)) print(paste("No blank responses in",language,"-- replace NAs with ''?"))
  
  # Danish and Norwegian no longer full of NAs
  #if(language=="Danish" | language=="Norwegian") {
  #  d_long <- d_long %>% mutate(value = replace_na(value, ""))
  #}
  
  d_long <- d_long %>%
    mutate(produces = as.numeric(value == "produces"))
  print(paste("retrieved data for",length(unique(d_long$data_id)),language,"participants"))
  #table(d_long$value)
  
  # negative correlation (because if you produce, you don't understand and vice-versa?)
  #d_long_wg %>% group_by(data_id) %>% 
  #  summarise(produces = sum(produces, na.rm=T), comprehends = sum(comprehends, na.rm=T)) %>%
  #  ggplot(aes(x=produces, y=comprehends)) + geom_point()
  
  d_prod <- d_long %>% select(data_id, item_id, produces) %>%
    pivot_wider(id_cols = data_id, names_from = item_id, 
                values_from = produces) %>%
    data.frame() 
  
  rownames(d_prod) = d_prod$data_id
  d_prod$data_id = NULL
  d_prod <- d_prod %>% data.matrix
  
  if(save) save(d_demo, items, d_long, d_prod, 
                file=paste("data/WS/",language,"_",form,"_data.Rdata", sep=''))
}


for(lang in ws_langs$language) {
  tryCatch(get_ws_data(lang), 
           error=function(e) {
             message(lang)
             message(e)
           })
}

# "retrieved data for 1295 Cantonese participants" # remove Tardif data??
# "retrieved data for 377 Croatian participants"
# "retrieved data for 3714 Danish participants"
# "retrieved data for 8650 English (American) participants" # now 8853
# "retrieved data for 930 French (Quebecois) participants" 
# "retrieved data for 1181 German participants"
# "retrieved data for 520 Hebrew participants"
# "retrieved data for 752 Italian participants"
# "retrieved data for 1056 Mandarin (Beijing) participants"
# "retrieved data for 9304 Norwegian participants"
# "retrieved data for 1037 Russian participants"
# "retrieved data for 1066 Slovak participants"
# "retrieved data for 1853 Spanish (Mexican) participants"
# "retrieved data for 900 Swedish participants"
# "retrieved data for 2422 Turkish participants"
# "retrieved data for 100 Kigiriama participants"
# "retrieved data for 90 Kiswahili participants"
# "retrieved data for 493 Czech participants"
# "retrieved data for 1520 English (Australian) participants"
# "retrieved data for 500 Latvian participants"
# "retrieved data for 1376 Korean participants"
# "retrieved data for 665 French (French) participants" <- more French participants added in 01-new-French-data.R
# "retrieved data for 593 Spanish (European) participants"
# "retrieved data for 3012 Portuguese (European) participants"
# "retrieved data for 1897 Mandarin (Taiwanese) participants"
# "retrieved data for 99 English (Irish) participants"
# "retrieved data for 99 Irish participants"
# "retrieved data for 70 Finnish participants"
# "retrieved data for 303 Dutch participants"
# "retrieved data for 372 Hungarian participants"
# "retrieved data for 784 Spanish (Argentinian) participants"
# "retrieved data for 105 Spanish (Peruvian) participants"
# "retrieved data for 50 Persian participants"
# "retrieved data for 176 Greek (Cypriot) participants"
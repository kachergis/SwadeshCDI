# generic functions to 
# 1) grab 

library(wordbankr)
library(tidyverse)
library(mirt)

instr <- wordbankr::get_instruments() %>% 
  #filter(form=="WG") %>%
  arrange(desc(unilemma_coverage))
  

# MB@home: wide age range, diff items for different age ranges?

get_wg_data <- function(language, save=T, form="WG") {
  d_demo <- 
    get_administration_data(language = language, form = form) 
  
  items <- get_item_data(language = language, form=form) %>%
    filter(type=="word")
  
  d_long_wg <- get_instrument_data(language = language, form = form) %>% # 418 items
    left_join(items %>% select(-complexity_category), by="num_item_id") %>%
    filter(type=="word")
  
  if(!"" %in% unique(d_long_wg$value)) print(paste("No blank responses in",language,"-- replace NAs with ''?"))
  
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
  
  # only swap in definition as colnames if they are unique
  if(length(unique(items$definition))==length(items$definition)) {
    colnames(d_comp) = items$definition
    colnames(d_prod) = items$definition
  }
  
  bad_words_prod = which(colSums(d_prod, na.rm=T) == 0)
  bad_words_comp = which(colSums(d_comp, na.rm=T) == 0)
  #bad_subjs_prod = which(rowSums(d_prod) == 0) # mirt can remove
  #bad_subjs_comp = which(rowSums(d_comp) == 0) 
  
  print(paste(length(bad_words_comp),"words with all 0 responses removed from",language,"comprehension"))
  print(paste(length(bad_words_prod),"words with all 0 responses removed from",language,"production"))
  #if(length(bad_words_comp)!=0) d_comp = d_comp[,-bad_words_comp]
  #if(length(bad_words_prod)!=0) d_prod = d_prod[,-bad_words_prod]
  
  if(save) save(d_demo, items, d_long_wg, d_prod, d_comp,
                file=paste("data/",language,"_WG_data.Rdata", sep=''))
}

#get_wg_data("Spanish (European)") # "26 words with all 0 responses removed from Spanish (European) production"
# manually adding uni-lemmas GK coded
get_wg_data("Kiswahili") # 8 all 0 responses removed from comprehension, 76 removed from production
get_wg_data("Kigiriama") # 32 removed from production
get_wg_data("Turkish")
get_wg_data("Mandarin (Taiwanese)") # need to manually code and add uni_lemmas
get_wg_data("English (American)")
get_wg_data("Croatian") 
get_wg_data("Danish")
get_wg_data("Italian")
get_wg_data("Russian")
get_wg_data("Spanish (Mexican)")


get_wg_data("British Sign Language") # 161 subjects; removed 11 comprehension words and 16 production words
get_wg_data("English (British)", form="Oxford CDI")
get_wg_data("Mandarin (Beijing)", form="IC")

# not many ASL subjects, but how much do the forms overlap? can we combine..?
#get_wg_data("American Sign Language", form="FormA") # 6 subjects..
get_wg_data("American Sign Language", form="FormBTwo") # 20 subjects
#get_wg_data("American Sign Language", form="FormBOne") # 19 subjects
#get_wg_data("American Sign Language", form="FormC") # 18 subjects

languages = c("British Sign Language", "Mandarin (Beijing)", "American Sign Language",
              "Portuguese (European)", "English (British)")

# should we try adding WS data from languages with no WG data? e.g., German
get_wg_data("German", form="WS")

add_new_unilemmas <- function(language) {
  load(paste("data/",language,"_WG_data.Rdata", sep=''))
  # load item table with newly-coded uni_lemma
  #its_uni <- read_csv("updated_unilemmas/[Spanish_European_WG].csv") 
  #its_uni <- read_csv("updated_unilemmas/[Mandarin_Beijing_IC].csv")
  #its_uni <- read_csv("updated_unilemmas/[Mandarin_Taiwanese_WG].csv")
  items <- items %>% select(-uni_lemma) %>% 
    left_join(its_uni %>% select(definition, uni_lemma))
  d_long_wg <- d_long_wg %>% select(-uni_lemma) %>%
    left_join(its_uni %>% select(definition, uni_lemma))
  save(d_demo, items, d_long_wg, d_prod, d_comp,
     file=paste("data/",language,"_WG_data.Rdata", sep=''))
}

get_wg_data("Swedish") # "44 words with all 0 responses removed from Swedish production"
get_wg_data("Norwegian") 
get_wg_data("French (Quebecois)") # "83 words with all 0 responses removed from French (Quebecois) production"
get_wg_data("Slovak")
# "2 words with all 0 responses removed from Slovak comprehension"
# "3 words with all 0 responses removed from Slovak production"
get_wg_data("Latvian")

# a lot removed..very low rates of "produces"
get_wg_data("French (French)")
# "79 words with all 0 responses removed from French (French) comprehension"
# "486 words with all 0 responses removed from French (French) production"

# generalization test: try using proposed short lists on this (need to code uni_lemmas for short list)
get_wg_data("Portuguese (European)") # 222 subjects
# do real-data simulations

get_wg_data("Korean") 
get_wg_data("Hebrew") # "4 words with all 0 responses removed from Hebrew production"


languages = c("Kiswahili","Kigiriama",
              "Croatian","Danish","English (American)","Korean","Spanish (Mexican)",
              "Italian","Mandarin (Taiwanese)","French (French)", 
              "Korean", "Latvian", "Hebrew", "Norwegian", "French (Quebecois)",
              "Slovak", "Spanish (European)", "Russian", "Turkish") 

#models = list()
#coefs = list()
load("data/multiling_2pl_WG_comp_fits.Rdata")

# Norwegian did not converge after 2000 cycles
# do comprehension first
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/",lang,"_WG_data.Rdata", sep=''))
    print(paste("Fitting",nrow(d_comp),"subjects and",ncol(d_comp),"words in",lang))
    mod_string = paste0('G = 1-',ncol(d_comp),',
                LBOUND = (1-',ncol(d_comp),', a1, 0),
                PRIOR = (1-',ncol(d_comp),', d, norm, 0, 3)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_comp, model = mod, itemtype="2PL", 
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=3000, removeEmptyRows=TRUE)) 
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
  }
}

save(models, coefs, file="data/multiling_2pl_WG_comp_fits.Rdata")


# fit WG production

#models = list()
#coefs = list()
load("data/multiling_2pl_WG_prod_fits.Rdata")
# production
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/",lang,"_WG_data.Rdata", sep=''))
    print(paste("Fitting",nrow(d_prod),"subjects and",ncol(d_prod),"words in",lang))
    mod_string = paste0('G = 1-',ncol(d_prod),',
                LBOUND = (1-',ncol(d_prod),', a1, 0),
                PRIOR = (1-',ncol(d_prod),', d, norm, 0, 3)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_prod, model = mod, itemtype="2PL", 
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=3000, removeEmptyRows=TRUE)) 
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
  }
}

save(models, coefs, file="data/multiling_2pl_WG_prod_fits.Rdata")


# combine comprehension (1) and production (2) data and
# fit generalized partial credit model
models = list()
coefs = list()
# https://rstudio-pubs-static.s3.amazonaws.com/357155_6674780326ef4afba5f009d17a85d4ae.html
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/",lang,"_WG_data.Rdata", sep=''))
    d_cp = d_prod * 2 + d_comp
    print(paste("Fitting",nrow(d_cp),"subjects and",ncol(d_cp),"words in",lang))
    
    mod_string = paste0('G = 1-',ncol(d_cp),',
                LBOUND = (1-',ncol(d_cp),', a, 0)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_cp, model = mod, itemtype="gpcm", 
                          method="QMCEM", verbose=TRUE, 
                          technical=list(NCYCLES=3000, removeEmptyRows=TRUE)) 
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
  }
}

save(models, coefs, file="data/multiling_2pl_WG_comp_prod_gpcm_fits.Rdata")

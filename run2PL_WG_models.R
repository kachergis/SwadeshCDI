# generic functions to 
# 1) grab 

library(wordbankr)
library(tidyverse)
library(mirt)

wordbankr::get_instruments() %>% 
  filter(form=="WG") %>%
  arrange(desc(unilemma_coverage))
  

# MB@home: wide age range, diff items for different age ranges?

get_wg_data <- function(language, save=T) {
  d_demo <- 
    get_administration_data(language = language, form = "WG") 
  
  items <- get_item_data(language = language, form="WG") %>%
    filter(type=="word")
  
  d_long_wg <- get_instrument_data(language = language, form = "WG") %>% # 418 items
    left_join(items %>% select(-complexity_category), by="num_item_id") %>%
    filter(type=="word")
  
  if(!"" %in% unique(d_long_wg$value)) print(paste("No blank responses in",language,"-- replace NAs with ''?"))
  
  if(language=="Danish" | language=="Norwegian") {
    d_long_wg <- d_long_wg %>% mutate(value = replace_na(value, ""))
  }
  
  d_long_wg <- d_long_wg %>%
    mutate(produces = as.numeric(value == "produces"),
           comprehends = as.numeric(value == "understands"))
  
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
  if(length(bad_words_comp)!=0) d_comp = d_comp[,-bad_words_comp]
  if(length(bad_words_prod)!=0) d_prod = d_prod[,-bad_words_prod]
  
  if(save) save(d_demo, items, d_long_wg, d_prod, d_comp,
                file=paste("data/",language,"_WG_data.Rdata", sep=''))
}

get_wg_data("Turkish")
get_wg_data("Mandarin (Taiwanese)")
get_wg_data("English (American)")
get_wg_data("Croatian") 
get_wg_data("Danish")
get_wg_data("Italian")
get_wg_data("Russian")
get_wg_data("Spanish (Mexican)")

#get_wg_data("Spanish (European)") # "26 words with all 0 responses removed from Spanish (European) production"
# manually adding uni-lemmas GK coded
do_once <- function(language) {
  load(paste("data/",language,"_WG_data.Rdata", sep=''))
  sp_eur_it <- read_csv("[Spanish_European_WG].csv")
  items <- items %>% select(-uni_lemma) %>% 
    left_join(sp_eur_it %>% select(definition, uni_lemma))
  d_long_wg <- d_long_wg %>% select(-uni_lemma) %>%
    left_join(sp_eur_it %>% select(definition, uni_lemma))
  save(d_demo, items, d_long_wg, d_prod, d_comp,
     file=paste("data/",language,"_WG_data.Rdata", sep=''))
}

get_wg_data("Swedish") # "44 words with all 0 responses removed from Swedish production"
get_wg_data("Norwegian") 
get_wg_data("French (Quebecois)") # "83 words with all 0 responses removed from French (Quebecois) production"
get_wg_data("Slovak")
# "2 words with all 0 responses removed from Slovak comprehension"
# "3 words with all 0 responses removed from Slovak production"
get_wg_data("Latvian") # need uni-lemmas

# a lot removed..
get_wg_data("French (French)")
# "79 words with all 0 responses removed from French (French) comprehension"
# "486 words with all 0 responses removed from French (French) production"

get_wg_data("Korean") 
get_wg_data("Hebrew") # "4 words with all 0 responses removed from Hebrew production"


languages = c("Croatian","Danish","English (American)","Korean","Spanish (Mexican)",
              "Italian","Mandarin (Taiwanese)","French (French)", 
              "Korean", "Latvian", "Hebrew", "Norwegian", "French (Quebecois)",
              "Slovak", "Spanish (European)", "Russian", "Turkish") 
# "Swedish", <- a bunch of words have only one response category, but there are lots of NAs..

#models = list()
#coefs = list()
load("data/multiling_2pl_WG_comp_fits.Rdata")

languages = c("Danish","Norwegian","French (French)","Croatian")

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
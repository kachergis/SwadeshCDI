# generic functions to 
# 1) grab 

library(wordbankr)
library(tidyverse)
library(mirt)

# MB@home: wide age range, diff items for different age ranges?

get_wg_data <- function(language, save=T) {
  d_demo <- 
    get_administration_data(language = language, form = "WG") 

  items <- get_item_data(language = language, form="WG") 
  
  d_long_wg <- get_instrument_data(language = language, form = "WG") %>% # 418 items
    left_join(items %>% select(-complexity_category), by="num_item_id") %>%
    mutate(produces = as.numeric(value == "produces"),
           comprehends = as.numeric(value == "understands"))
  
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

# need to run IRT models for these:
get_wg_data("Spanish (Mexican)") # 'list' object cannot be coerced to type 'double'
get_wg_data("Korean") # 'list' object cannot be coerced to type 'double'
get_wg_data("Hebrew") # duplicate definitions: should we 1) use numeric item_id,

languages = c("Croatian","Danish","English (American)","Korean","Spanish (Mexican)",
              "Italian","Mandarin (Taiwanese)","Russian","Turkish")

models = list()
coefs = list()


# do comprehension first
for(lang in languages) {
  load(paste("data/",lang,"_WG_data.Rdata", sep=''))
  models[[lang]] = mirt(data = d_comp, model = 1, itemtype="2PL", 
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=2000, removeEmptyRows=TRUE)) 
  coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
    mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
}

save(models, coefs, file="data/multiling_2pl_WG_comp_fits.Rdata")


models = list()
coefs = list()

# production
for(lang in languages) {
  load(paste("data/",lang,"_WG_data.Rdata", sep=''))
  models[[lang]] = mirt(data = d_prod, model = 1, itemtype="2PL", 
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=2000, removeEmptyRows=TRUE)) 
  coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
    mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
}

save(models, coefs, file="data/multiling_2pl_WG_prod_fits.Rdata")
# generic functions to 
# 1) grab 

library(wordbankr)
library(tidyverse)
library(mirt)

languages = instr <- wordbankr::get_instruments() %>% 
  filter(form=="WS") %>% # form_type=="WS" gets us "English (British)" and "American Sign Language"
  arrange(desc(unilemma_coverage))


# fit WS production
mirtCluster()
#models = list()
#coefs = list()
load("data/multiling_2pl_WS_prod_fits.Rdata")
# production
for(lang in languages$language) {
  load(paste("data/WS/",lang,"_WS_data.Rdata", sep=''))
  if(is.element(lang, names(models))) {
    nsubj_model = length(models[[lang]]@Data$rowID)
    if(nsubj_model != nrow(d_prod)) print(paste(lang,"model has",nsubj_model,"subjects; d_prod has",nrow(d_prod),"; d_demo has",nrow(d_demo)))
  }
  # do number of subjects in fitted model match d_prod? (may not have updated all fits with WB2 data?)
  if(!is.element(lang, names(models))) { # skip if already fitted
    
    bad_words_prod = c(which(colSums(d_prod, na.rm=T) == 0), which(colSums(d_prod, na.rm=T) == nrow(d_prod)))
    print(paste(length(bad_words_prod),"words with all 0 responses removed from",lang,"production"))
    if(length(bad_words_prod)!=0) d_prod = d_prod[,-bad_words_prod]
    
    print(paste("Fitting",nrow(d_prod),"subjects and",ncol(d_prod),"words in",lang))
    mod_string = paste0('G = 1-',ncol(d_prod),',
                LBOUND = (1-',ncol(d_prod),', a1, 0),
                PRIOR = (1-',ncol(d_prod),', d, norm, 0, 3)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_prod, model = mod, itemtype="2PL", 
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=3000)) # , removeEmptyRows=TRUE
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
    save(models, coefs, file="data/multiling_2pl_WS_prod_fits.Rdata")
  }
}

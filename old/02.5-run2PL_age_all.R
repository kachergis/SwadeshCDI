# generic functions to 
# 1) grab 

library(wordbankr)
library(tidyverse)
library(mirt)

languages = instr <- wordbankr::get_instruments() %>% 
  filter(form=="WS") %>%
  arrange(desc(unilemma_coverage))

low_data_langs <- c("Kiswahili", "Kigiriama", "Irish", "Persian", "Finnish",
                    "English (Irish)", "Spanish (Peruvian)", "Greek (Cypriot)")

languages = setdiff(languages$language, low_data_langs)

# fit WS production
mirtCluster()
#models = list()
#coefs = list()
load("data/multiling_2pl_age_WS_prod_fits.Rdata")
# production
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/WS/",lang,"_WS_data.Rdata", sep=''))
    d_demo$age_sc = (d_demo$age - 23)  # center to target age range (16-30)..scale? (/23)
    bad_words_prod = c(which(colSums(d_prod, na.rm=T) == 0), which(colSums(d_prod, na.rm=T) == nrow(d_prod)))
    print(paste(length(bad_words_prod),"words with all 0 responses removed from",lang,"production"))
    if(length(bad_words_prod)!=0) d_prod = d_prod[,-bad_words_prod]
    
    print(paste("Fitting",nrow(d_prod),"subjects and",ncol(d_prod),"words in",lang))
    mod_string = paste0('G = 1-',ncol(d_prod),',
                COV = F1
                LBOUND = (1-',ncol(d_prod),', a1, 0),
                PRIOR = (1-',ncol(d_prod),', d, norm, 0, 3)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_prod, model = mod, itemtype="2PL", 
                        covdata = d_demo$age_sc,
                        method="QMCEM", verbose=TRUE, 
                        technical=list(NCYCLES=3000)) # , removeEmptyRows=TRUE
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
    save(models, coefs, file="data/multiling_2pl_age_WS_prod_fits.Rdata")
  }
}

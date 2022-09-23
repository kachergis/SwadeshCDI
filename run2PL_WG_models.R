# generic functions to 
# 1) grab 

#library(wordbankr)
library(tidyverse)
library(mirt)

# ToDo: rewrite to read whatever language data files are in data/WG
languages = c("Kigiriama", "Kiswahili", "British Sign Language",
              "Croatian","Danish","English (American)",
              "Italian","Mandarin (Taiwanese)","French (French)", 
              "Korean", "Latvian", "Hebrew", "Norwegian", "French (Quebecois)",
              "Slovak", "Spanish (European)", "Spanish (Mexican)", "Swedish",
              "Russian", "Turkish", "Portuguese (European)", 
              "Dutch", "Spanish (Chilean)", "Persian")

#models = list()
#coefs = list()
load("data/multiling_2pl_WG_comp_fits.Rdata")

# Norwegian did not converge after 2000 cycles
# do comprehension first
mirtCluster()
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/WG/",lang,"_WG_data.Rdata", sep=''))
    
    bad_words_comp = c(which(colSums(d_comp, na.rm=T) == 0), which(colSums(d_comp, na.rm=T) == nrow(d_comp)))
    print(paste(length(bad_words_comp),"words with all 0 or 1 responses removed from",lang,"comprehension"))
    if(length(bad_words_comp)!=0) d_comp = d_comp[,-bad_words_comp]
    
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
    save(models, coefs, file="data/multiling_2pl_WG_comp_fits.Rdata")
  }
}

# problem with Swedish..
#"Fitting 474 subjects and 385 words in Swedish"
#Error: The following items have only one response 
#category and cannot be estimated: item_1 item_2 item_3 item_4 item_5 item_6 item_7
#... item_113

# "79 words with all 0 or 1 responses removed from French (French) comprehension" (a lot! look at these?)

# fit WG production
mirtCluster()
#models = list()
#coefs = list()
load("data/multiling_2pl_WG_prod_fits.Rdata")
# production
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/WG/",lang,"_WG_data.Rdata", sep=''))
    
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
                        technical=list(NCYCLES=3000, removeEmptyRows=TRUE)) 
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
    save(models, coefs, file="data/multiling_2pl_WG_prod_fits.Rdata")
  }
}
# WB2 fits for 24 languages completed, but some models not fully converged after 3000 iterations

# combine comprehension (1) and production (2) data and
# fit generalized partial credit model
#models = list()
#coefs = list()
load("data/multiling_2pl_WG_comp_prod_gpcm_fits.Rdata")
# https://rstudio-pubs-static.s3.amazonaws.com/357155_6674780326ef4afba5f009d17a85d4ae.html
for(lang in languages) {
  if(!is.element(lang, names(models))) { # skip if already fitted
    load(paste("data/WG/",lang,"_WG_data.Rdata", sep=''))
    d_cp = d_prod + d_comp
    
    bad_words = c(which(colSums(d_cp, na.rm=T) == 0), which(colSums(d_cp, na.rm=T) == nrow(d_cp)))
    print(paste(length(bad_words),"words with all 0 or 1 responses removed from",lang,"production"))
    if(length(bad_words)!=0) d_cp = d_cp[,-bad_words]
    
    print(paste("Fitting",nrow(d_cp),"subjects and",ncol(d_cp),"words in",lang))
    
    mod_string = paste0('G = 1-',ncol(d_cp),',
                LBOUND = (1-',ncol(d_cp),', a, 0)')
    mod <- mirt.model(mod_string)
    models[[lang]] = mirt(data = d_cp, model = mod, itemtype="gpcm", 
                          method="QMCEM", verbose=TRUE, 
                          technical=list(NCYCLES=3000, removeEmptyRows=TRUE)) 
    coefs[[lang]] <- as_tibble(coef(models[[lang]], simplify = TRUE)$items) %>%
      mutate(definition = rownames(coef(models[[lang]], simplify = TRUE)$items))
    save(models, coefs, file="data/multiling_2pl_WG_comp_prod_gpcm_fits.Rdata")
  }
}


# what is this??
# "3 words with all 0 or 1 responses removed from British Sign Language production"
# "Fitting 161 subjects and 545 words in British Sign Language"
# "item_72" re-mapped to ensure all categories have a distance of 1 ...

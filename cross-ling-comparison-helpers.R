# helper functions for cross-linguistic wordbank/IRT analyses

# given long dataframe of uni_lemmas by language,
# return # of pairwise overlapping uni-lemmas
get_uni_lemma_overlap <- function(xldf) {
  nlang = length(unique(xldf$language))
  langs = sort(unique(xldf$language))
  olap <- matrix(NA, nrow=nlang, ncol=nlang)
  row.names(olap) = langs
  colnames(olap) = langs
  for(l1 in langs) {
    for(l2 in langs) {
      l1v = subset(xldf, language==l1)$uni_lemma
      l2v = subset(xldf, language==l2)$uni_lemma
      olap[l1,l2] = length(intersect(l1v, l2v))
    }
  }
  return(olap)
}



get_cross_ling_difficulty_cors <- function(xldf) {
  languages = unique(xldf$language)
  
  prod_cors <- matrix(0, nrow=length(languages), ncol=length(languages))
  prod_sims <- tibble()
  colnames(prod_cors) = languages
  rownames(prod_cors) = languages
  
  for(l1 in languages) {
    for(l2 in languages) {
      tmp <- xldf %>% filter(language==l1 | language==l2, !is.na(d)) %>%
        select(uni_lemma, category, lexical_category, language, d) %>%
        group_by(uni_lemma, language) %>%
        slice(1) %>% 
        pivot_wider(names_from = language, values_from = d) %>%
        drop_na()
      prod_cors[l1,l2] <- cor(tmp[,l1], tmp[,l2], method="spearman")
      prod_sims <- bind_rows(prod_sims, tibble("Lang1" = l1, "Lang2" = l2, 
                                               "r" = cor(tmp[,l1], tmp[,l2], method="spearman")[[1]], 
                                               "N" = nrow(tmp)))
    }
  }
  return(list(prod_cors=prod_cors, prod_sims=prod_sims))
}


run_swadesh_comparisons <- function(xldf, languages, form='WS') {
  xx <- tibble()
  for(lang in languages) {
    load(here(paste("data/",form,"/",lang,"_",form,"_data.Rdata", sep='')))
    swad_l <- subset(xldf, language==lang & is.element(uni_lemma, good_prod$uni_lemma)) # 
    swad_cor = cor(rowSums(d_prod, na.rm=T), rowSums(d_prod[,swad_l$item_id], na.rm=T))
    rand_inds = sample(1:ncol(d_prod), nrow(swad_l)) # N random words 
    rand_cor = cor(rowSums(d_prod, na.rm=T), rowSums(d_prod[,rand_inds], na.rm=T))
    xx <- xx %>% bind_rows(tibble(language = lang, `Swadesh r` = swad_cor, 
                                  `Rand r` = rand_cor, N = nrow(swad_l)))
    # d_demo, d_long, d_prod
  }
  return(xx)
}
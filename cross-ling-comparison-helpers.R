# helper functions for cross-linguistic wordbank/IRT analyses

get_item_n_subject_counts <- function(models) {
  tab <- tibble()
  for(lang in names(coefs)) {
    nitems = models[[lang]]@Data$nitems
    N = models[[lang]]@Data$N
    tab = bind_rows(tab, tibble(Language = lang, items = nitems, N = N))
  }
  return(tab)
}

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

# for each language, look for uni-lemmas in swad_list in xldf (may not find all of them!)
# and test correlation between swad_list subsample and full CDI.
# also test correlation of full CDI against many random samples of size length(swad_list)
run_swadesh_comparisons <- function(xldf, languages, swad_list, form='WS', rand_comparisons=100) {
  xx <- tibble()
  for(lang in languages) {
    load(here(paste("data/",form,"/",lang,"_",form,"_data.Rdata", sep='')))
    swad_l <- subset(xldf, language==lang & is.element(uni_lemma, swad_list)) 
    swad_cor = cor(rowSums(d_prod, na.rm=T), rowSums(d_prod[,swad_l$item_id], na.rm=T))
    
    xx <- xx %>% bind_rows(tibble(language = lang, sublist = "Swadesh", 
                                  run = NA, cor = swad_cor, N = nrow(swad_l)))
    
    if(rand_comparisons!=0) {
      rand_cors <- sapply(1:rand_comparisons, \(x) {
        rand_inds = sample(1:ncol(d_prod), nrow(swad_l)) # N random words 
        rand_cor = cor(rowSums(d_prod, na.rm=T), rowSums(d_prod[,rand_inds], na.rm=T))
        rand_cor
      })
      xx <- xx %>% bind_rows(tibble(language = lang, sublist = "random", 
                                    run = 1:rand_comparisons, cor = rand_cors, N = nrow(swad_l)))
    }
    
  }
  
  if(rand_comparisons!=0) {
    xx_sum <- xx %>% 
      group_by(language, sublist, N) %>%
      summarise(r = mean(cor)) %>%
      pivot_wider(names_from = sublist, values_from = r) %>%
      rename(`Swadesh r` = Swadesh, `Rand r` = random)
  } else { # no random comparison
    xx_sum <- xx %>% 
      group_by(language, sublist, N) %>%
      summarise(r = mean(cor)) %>%
      pivot_wider(names_from = sublist, values_from = r) %>%
      rename(`Swadesh r` = Swadesh)
  }
  return(xx_sum)
}

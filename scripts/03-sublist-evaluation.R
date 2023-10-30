make_swadesh_sublist <- function(prod_sum, list_size, k) {
  prod_sum |> 
    filter(num_langs >= k) |> 
    arrange(sd_d) |> 
    slice(1:list_size)
}

make_random_sublist <- function(prod_sum, list_size, k) {
  subk <- prod_sum |> 
    filter(num_langs >= k)
  idx <- sample(1:nrow(subk), min(list_size, nrow(subk)))
  subk |> slice(idx)
}

get_difficulty_cor <- function(sublist, test_items) {
  prod_res <- sublist |>
    left_join(test_items, by = "uni_lemma",
              multiple = "first")
  c((!is.na(prod_res$d)) |> sum(),
    cor(prod_res$mean_d, prod_res$d, use = "complete.obs"))
}

get_sumscore_cor <- function(sublist, xldf, all_prod, lang) {
  xldf_sub <- sublist |> 
    left_join(xldf |> filter(language == lang),
              by = "uni_lemma",
              multiple = "first") |> 
    filter(!is.na(uid))
  if (nrow(xldf_sub) == 0) {
    return(NA) 
  } else if (nrow(xldf_sub) == 1) {
    prod_sub <- all_prod[[lang]][,xldf_sub$uid]
  } else {
    prod_sub <- rowMeans(all_prod[[lang]][,xldf_sub$uid], na.rm = TRUE)
  }
  c(nrow(xldf_sub),
    cor(rowMeans(all_prod[[lang]], na.rm = TRUE), prod_sub,
      use = "na.or.complete"))
}

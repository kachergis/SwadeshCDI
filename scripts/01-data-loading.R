get_new_items <- function(language, pronouns) {
  lang <- language
  items <- read_csv(glue("data/new_items/{language}.csv"))
  forms <- setdiff(colnames(items), 
                   c("category", "definition", "gloss", "uni_lemma", "sho"))
  prons <- pronouns |> 
    filter(language == lang) |> 
    select(uid, new_uni_lemma)
  
  new_items <- items |> 
    pivot_longer(cols = all_of(forms),
                 names_to = "form",
                 values_to = "item_id") |> 
    mutate(uid = ifelse(is.na(item_id), NA, glue("{form}_{item_id}"))) |> 
    select(-item_id) |> 
    pivot_wider(names_from = form, values_from = uid)
  new_items <- new_items |> 
    mutate(uid = do.call(coalesce, select(new_items, all_of(forms)))) |> 
    select(-all_of(forms)) |> 
    cbind(items |> select(all_of(forms))) |> 
    left_join(prons, by = "uid") |> 
    mutate(uni_lemma = coalesce(new_uni_lemma, uni_lemma)) |> 
    select(-new_uni_lemma)
  
  list(forms = forms,
       new_items = new_items)
}

make_matrix <- function(df) {
  d_vals <- df |> 
    select(data_id, uid, value) |> 
    pivot_wider(id_cols = data_id, 
                names_from = uid, 
                values_from = value) |> 
    data.frame()
  
  d_vals <- d_vals |> 
    `rownames<-`(value = d_vals$data_id) |> 
    select(-data_id) |> 
    data.matrix()
  
  d_vals
}

make_data <- function(language, forms, new_items) {
  all_demo <- c()
  all_long <- c()
  
  for (form in forms) {
    d_demo <- get_administration_data(language = language, form = form) 
    
    items <- get_item_data(language = language, form = form) |> 
      filter(item_kind == "word") 
    
    d_long <- get_instrument_data(language = language, form = form) |> 
      left_join(items |> select(-complexity_category), by = "item_id") |> 
      filter(item_kind == "word")
    
    d_long <- d_long |> 
      mutate(produces = as.numeric(produces),
             understands = as.numeric(understands)) |> 
      left_join(new_items |> select(uid, all_of(form)), by = c("item_id" = form))
    
    if (language == "French (French)") {
      d_demo <- d_demo |> filter(dataset_name != "Kern")
      d_long <- d_long |> filter(data_id %in% d_demo$data_id)
      
      f <- form
      new_d_demo <- readRDS("data/frfr_new_demo.rds") |> 
        filter(form == f) |> 
        mutate(dataset_name = "Tsuji_CAT",
               dataset_source_name = "Tsuji",
               language = "French (French)") |> 
        select(-source_name)
      new_d_wide <- readRDS("data/frfr_new_wide.rds") |> 
        filter(data_id %in% new_d_demo$data_id)
      new_d_long <- new_d_wide |> 
        pivot_longer(cols = -data_id,
                     names_to = "sho",
                     values_to = "produces") |> 
        left_join(new_items |> select(-"WS", -"WG"), by = "sho") |> 
        select(-"sho", "item_definition" = "definition", "english_gloss" = "gloss") |> 
        mutate(language = "French (French)",
               form = f)
      
      d_demo <- bind_rows(d_demo, new_d_demo)
      d_long <- bind_rows(d_long, new_d_long)
    }
    
    print(paste("Retrieved data for", length(unique(d_long$data_id)), 
                language, form, "participants"))
    
    all_demo <- c(all_demo, list(d_demo))
    all_long <- c(all_long, list(d_long))
  }
  
  all_demo <- bind_rows(all_demo)
  all_long <- bind_rows(all_long)
  all_prod <- all_long |> 
    mutate(value = produces) |> 
    make_matrix()
  
  list(all_demo = all_demo,
       all_long = all_long,
       all_prod = all_prod)
}
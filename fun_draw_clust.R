hc2split <- function(x)
{
  A <- x$merge # (n-1,n) matrix
  n <- nrow(A) + 1
  B <- list()
  
  for(i in 1:(n-1)){
    ai <- A[i,1]
    
    if(ai < 0)
      B[[i]] <- -ai
    else
      B[[i]] <- B[[ai]]        
    
    ai <- A[i,2]
    
    if(ai < 0)
      B[[i]] <- sort(c(B[[i]],-ai))
    else
      B[[i]] <- sort(c(B[[i]],B[[ai]]))
  }
  
  CC <- matrix(rep(0,n*(n-1)),nrow=(n-1),ncol=n)
  
  for(i in 1:(n-1)){
    bi <- B[[i]]
    m <- length(bi)
    for(j in 1:m)
      CC[i,bi[j]] <- 1
  }
  
  split <- list(pattern=apply(CC,1,paste,collapse=""), member=B)
  
  return(split)
}

gen_rect <- function(m = NULL,
                     border = NULL,
                     alpha = 0.95,
                     type = "geq",
                     pv = "bp",
                     max.only = F){
  len <- nrow(m$edges)
  member <- hc2split(m$hclust)$member
  order <- m$hclust$order
  usr <- par("usr")
  xwd <- usr[2] - usr[1]
  ywd <- usr[4] - usr[3]
  cin <- par()$cin
  if (is.null(border)) {
    border <- c(si = 4, au = 2, bp = 3)[pv]
  }
  ht <- c()
  j <- 1
  if (is.na(pm <- pmatch(type, c("geq", "leq", "gt", "lt")))) 
    stop("Invalid type argument: see help(pvrect)")
  
  rect_info <- list()
  
  for (i in (len - 1):1) {
    
    if (pm == 1) {
      wh <- (m$edges[i, pv] >= alpha)
    } else if (pm == 2) {
      wh <- (m$edges[i, pv] <= alpha)
    } else if (pm == 3) {
      wh <- (m$edges[i, pv] > alpha)
    } else if (pm == 4) {
      wh <- (m$edges[i, pv] > alpha)
    }
    
    if (wh) {
      mi <- member[[i]]
      ma <- match(mi, order)
      if (max.only == FALSE || (max.only && sum(match(ma, 
                                                      ht, nomatch = 0)) == 0)) {
        xl <- min(ma)
        xr <- max(ma)
        yt <- m$hclust$height[i]
        yb <- usr[3]
        mx <- xwd/length(member)/3
        my <- ywd/200
        rect_info[[i]] <- c(xl - mx, yb + my, xr + mx, yt + my)
        j <- j + 1
      }
      ht <- c(ht, ma)
    }
  }
  
  rect_info %<>% 
    bind_cols() %>% 
    t %>% 
    data.frame %>% 
    setNames(c("left",
               "bottom",
               "right",
               "top"))
  
  
  return(rect_info)
  
}

find_branch <- function(m = NULL,
                        prune_threshold = 50,
                        max.only = F){
  
  len <- nrow(m$edges)
  member <- hc2split(m$hclust)$member
  order <- m$hclust$order
  usr <- par("usr")
  xwd <- usr[2] - usr[1]
  ywd <- usr[4] - usr[3]
  cin <- par()$cin
  ht <- c()
  j <- 1
  
  all_rect <- list()
  
  for (i in (len - 1):1) {
    
      mi <- member[[i]]
      ma <- match(mi, order)
      if (max.only == FALSE || (max.only && sum(match(ma, 
                                                      ht, nomatch = 0)) == 0)) {
        xl <- min(ma)
        xr <- max(ma)
        yt <- m$hclust$height[i]
        yb <- usr[3]
        mx <- xwd/length(member)/3
        my <- ywd/200
        all_rect[[i]] <- c(xl - mx, yb + my, xr + mx, yt + my)
        j <- j + 1
      # }
      ht <- c(ht, ma)
    }
  }
  
  all_rect %<>% 
    bind_cols() %>% 
    t %>% 
    data.frame %>% 
    setNames(c("left",
               "bottom",
               "right",
               "top")) 
  
  # attach statistical significance
  sig <- m$edges$bp >= 0.95; sig <- sig[1:(length(sig)-1)]
  all_rect %<>% 
    mutate(sig = sig,
           remain = top <= prune_threshold) 
  
  # detect subset
  index_subset <- which(all_rect$sig + all_rect$remain == 2)
  
  lapply(1:length(index_subset), function(x){
    all_rect[index_subset,] %>% 
      filter(left <= all_rect[index_subset,]$left[x],
             right >= all_rect[index_subset,]$right[x],
             top > all_rect[index_subset,]$top[x]) 
  }) %>% map(nrow) %>% 
    unlist() %>% as.logical() %>% !. -> index_max

  member[index_subset[index_max]] %>% 
    map(~m$hclust$labels[.]) -> sig_clusters
  
  return(sig_clusters)

}

clust_bs %>% map(find_branch) -> all_branches

all_branches %>% 
  map(enframe) %>% 
  map(unnest) %>% 
  map(setNames, c("cluster_index","policy_index")) %>% 
  bind_rows(.id = "scen") %>% 
  pivot_wider(names_from = policy_index, values_from = cluster_index) %>% 
  right_join(data.frame(scen = names(joined)[1:36]), by = "scen") %>% 
  replace(., is.na(.),0) %>% 
  pivot_longer(cols = policy_raw_discrete,
               names_to = "policy_index",
               values_to = "cluster_index") -> all_branches_cleaned

all_branches_cleaned %>% 
  mutate(optim_lag = "1 day") %>% 
  bind_rows(all_branches_cleaned %>% 
              mutate(optim_lag = "14 days")) %>% 
  bind_rows(all_branches_cleaned %>% 
              mutate(optim_lag = "28 days")) %>% 
  mutate(in_cluster = if_else(cluster_index == 0, F, T),
         optim_lag = factor(optim_lag)) %>% 
  full_join(effect_data %>% 
              filter(criterion == "AIC") %>% 
              mutate(var_nchar = nchar(var),
                     var = if_else(var_nchar == 3,
                                   substr(var,1,2),
                                   var)) %>% 
              dplyr::select(var, scen, optim_lag, estimate, "p.value") %>% 
              rename(policy_index = var),
            by = c("scen", "policy_index","optim_lag")) %>% 
  filter(policy_index != "V_all_adj") -> all_branches_combined

all_branches_combined %>% group_by( in_cluster) %>% group_split() -> tmp

tmp[[1]] %>% 
  mutate(self_pos  = `p.value` <= 0.05 & estimate > 0,
         self_neg  = `p.value` <= 0.05 & estimate < 0,
         any_pos = as.logical(NA)) %>% 
  bind_rows(tmp[[2]] %>% 
              group_by(scen, cluster_index, optim_lag) %>% 
              group_split() %>% 
              map(mutate, self_pos = `p.value` <= 0.05 & estimate > 0) %>% 
              map(mutate, self_neg = `p.value` <= 0.05 & estimate < 0) %>% 
              map(mutate, any_pos = any(self_pos == T, na.rm = T)) %>% 
              map(mutate, n_in_cluster = n()) %>% 
              bind_rows()) -> tmp

# condition 1 = are they included in the variable selection process?
joined[1:36] %>% 
  map(colnames) -> names_all

lapply(1:36, function(x) names_all[[x]][which(names_all[[x]] %in% policy_raw_discrete)]) %>% 
  map(enframe) %>% 
  setNames(names(joined[1:36])) %>% 
  bind_rows(.id = "scenario") %>% 
  dplyr::select(-name) %>% 
  mutate(VS_include = T) %>% 
  pivot_wider(names_from = value, values_from = VS_include) %>% 
  replace(., is.na(.), F) %>% 
  pivot_longer(cols = policy_raw_discrete,
               names_to = "policy_code",
               values_to = "VS_include") %>% 
  rename(scen = scenario) -> condition_1

# condition 2 = if they are in the variable selection process, were they selected?
effect_data %>% 
  filter(criterion == "AIC",
         var != "V_all_adj") %>% 
  mutate(var_new2 = if_else(var_nchar == 3, substr(var,1,2), var)) %>% 
  dplyr::select(var_new2, scen, optim_lag) %>% 
  mutate(value = T) %>% 
  pivot_wider(names_from = var_new2, values_from = value) %>% 
  replace(., is.na(.), F) %>% 
  pivot_longer(cols = policy_raw_discrete,
               names_to = "policy_code",
               values_to = "VS_select") -> condition_2

condition_1 %>% 
  right_join(condition_2, by = c("scen", "policy_code")) %>% 
  rename(policy_index = policy_code) %>% 
  .[,c(1,2,4,3,5)] -> conditions

conditions %>% 
  left_join(tmp[,c("scen","policy_index","optim_lag","in_cluster", "self_neg","any_pos","n_in_cluster")],
            by = c("scen", "policy_index","optim_lag")) %>% 
  mutate(n_in_cluster = if_else(is.na(n_in_cluster),
                                as.integer(1),
                                n_in_cluster),
         n_in_cluster_score = ((n_in_cluster)*(-1) + 11)/10) %>% 
  separate(scen, into = c("metric","phase_def","phase"), remove = F) -> conditions_all


conditions_all %>% 
  filter(metric == "con") %>% 
  filter(VS_include == T,
         VS_select == T,
         self_neg == T,
         any_pos == F) 

# write_rds(conditions_all, "data/conditions_all.rds")

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

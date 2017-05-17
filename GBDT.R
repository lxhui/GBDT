library(readxl)
library(magrittr)
library(stringr)
letter_all = read_excel(path = "letter.xlsx",col_names = FALSE,n_max = 15000) 
letter_split <- function(x) {
  letter_unlist = unlist(x)
  y = letter_unlist[1] %>% 
    as.numeric()
  x = letter_unlist[2:17] %>%
    str_split(pattern = ":") %>%
    unlist() %>%
    extract(seq(2,32,by = 2)) %>%
    as.numeric()
  return(c(y,x))
}
letter_all_df = letter_all %>%
  apply(1,FUN = str_split,pattern = " ") %>%
  lapply(FUN = letter_split) %>% as.data.frame() %>%
  t %>% 
  as.data.frame() %>%
  set_rownames(NULL)


# 统计学习方法数据
# CART --------------------------------------------------------------------
cart_df = data.frame(A1 = rep(c(1,2,3),each = 5),A2 = c(2,2,1,1,2,2,2,1,2,2,2,2,1,1,2),A3 = c(2,2,2,1,2,2,2,1,1,1,1,1,2,2,2),A4 = c(3,2,2,3,3,3,2,2,1,1,1,2,2,3,1),label = c(2,2,1,1,2,2,2,1,1,1,1,1,1,1,2))






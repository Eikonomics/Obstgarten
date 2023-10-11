a = c(4:1)
b = c(4:1)
c = c(4:1)
d = c(4:1)

# proof that the product of frutis is maximized when 
# fruits are picked from the richest tree

i = 0
sum <- c()
math <- c()
for (a_1 in a) {
  for (b_1 in b) {
    for (c_1 in c) {
      for (d_1 in d) {
        i = i + 1
        sum[i] <- a_1 * b_1 * c_1 * d_1
        math[i] <- paste0("_",a_1, b_1, c_1,d_1)
      }
    }
  }
}
cbind(sum, math) %>% tibble() %>% print(n = 300)

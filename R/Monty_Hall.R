monty.hall <- function() {
  key <- sample(1:3, size = 1)
  goat <- setdiff(1:3, key)
  contestant <- sample(1:3, size = 1)
  monty <- ifelse(contestant == key, sample(goat, size = 1), setdiff(goat, contestant))
  switch <- setdiff(1:3, c(contestant, monty))
  result <- ifelse(switch == key, 1, 0)
  # result
  c("Key" = key, "Contestant" = contestant, "Monty" = monty, "Switch" = switch, "Result" = result)
}
monty.hall()
N <- 10000
monty.result <- replicate(N, monty.hall())
# monty.result
sum(monty.result[5, ])/N
cumsum(monty.result[5, ])
y <- cumsum(monty.result[5, ])
y_win <- ifelse(monty.result[5, ] == 1, y, NA)
y_win
y_lose <- ifelse(monty.result[5, ] == 1, NA, y)
y_lose
monty.plot <- function(N) {
monty.result <- replicate(N, monty.hall())
y <- cumsum(monty.result[5, ])
y_win <- ifelse(monty.result[5, ] == 1, y, NA)
y_win
y_lose <- ifelse(monty.result[5, ] == 1, NA, y)
y_lose
plot(x = 1:N, 
     y = y_win / N, 
     pch = 24, 
     col = "blue", 
     bg = "blue", 
     ylim = c(0, 4/5),
     xlab = "Number of Trials",
     ylab = "Proportion of Wins",
     yaxt = "n",
     cex = 0.7)
axis(side = 2, 
     at = c(0, 1/3, 2/3, 3/4), 
     labels = c("0", "1/3", "2/3", "3/4"))
points(x = 1:N, 
       y = y_lose / N, 
       pch = 25, 
       col = "red", 
       bg = "red", 
       cex = 0.7)
abline(h = 2/3, lty = 3)
legend("topleft", 
       inset = 0.05, 
       legend = c("Win", "Lose"), 
       pch = c(24, 25), 
       col = c("blue", "red"), 
       pt.bg = c("blue", "red"))
sum(monty.result[5, ])/N
}
monty.plot(30)
monty.plot(100)
table(unlist(monty.result[5, ]))


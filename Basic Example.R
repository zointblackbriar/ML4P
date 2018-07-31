library(stats)
library(limSolve)


# F1Score <- function(fact, pred){
#   "%ni%" <- Negate("%in%")
#   fact <- strsplit(fact, " ")
#   fact <- fact[nzchar(fact)][[1]]
#   
#   pred <- strsplit(pred, " ")
#   pred <- pred[nzchar(pred)][[1]]
#   
#   TP = sum(pred %in% fact)
#   
#   if(TP == 0)
#   {
#     return (0)
#   }
#   
#   precision = TP/length(pred)
#   recall = TP/length(fact)
#   
#   2 * precision * recall / (precision + recall)
# }
# 
# applyF1Score <- function (df) {
#   apply(df, 1, function(x) F1Score(x["fact"], x["predicted"]))
# }
# 
# #Test function
# df <- data.frame(order_id = c(1,2,3),
#                  fact = c("100 200", "300", "300 400"),
#                  predicted = c("10 20", "300", "15 25"))
# 
# df$f1 <- applyF1Score(df)
# print(df)
# print(mean(df$f1))


theta_grad <- matrix(c(0, 0), nrow = 9, ncol=9)  # Initialize the parameters
print(theta_grad)
# print(y)
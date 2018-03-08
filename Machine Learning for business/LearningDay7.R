library(arules)
library(arulesViz)

data("Groceries")
inspect(Groceries[1:3])
itemLabels(Groceries)
itemFrequencyPlot(Groceries, topN = 20, type = "absolute")
itemFrequencyPlot(Groceries, topN = 20, type = "relative")

# create item matrix
# form list
a_list <- list(c("a","b","c"), c("a","b"),
               c("a","b","d"), c("c","e"),
               c("a","b","d","e"))
names(a_list) <- paste0("Tr", c(1:5))
trans1 <- as(a_list, "transactions")
summary(trans1)
inspect(trans1)

# from matrix
a_matrix <- matrix(c(1,1,1,0,1,
                     1,1,0,0,0,
                     1,1,0,1,0,
                     0,0,1,0,1,
                     1,1,0,1,1), ncol = 5)
dimnames(a_matrix) <- list(paste0("Tr", c(1:5)),
                           c("a","b","c","d","e"))

trans2 <- as(a_matrix, "transactions")

inspect(trans2)

# create rules
rules <- apriori(Groceries, parameter = list(supp = 0.001,
                                           conf = 0.8))
inspect(rules[1:5])

# sort rules by lift
rules_sorted <- sort(rules, by = "lift", decreasing = T)
inspect(rules_sorted[1:5])

# sort rules by support
rules_sorted <- sort(rules, by = "support", decreasing = T)
inspect(rules_sorted[1:5])

# tuning rules maxlen
rules <- apriori(Groceries, parameter = list(supp = 0.0001,
                                             conf = 0.8,
                                             maxlen = 3))

# targeting items, find rhs
rules <- apriori(Groceries,
                 parameter = list(supp = 0.001,
                                  conf = 0.8),
                 appearance = list(default = "lhs",
                                   rhs = "whole milk"))
rules_sorted <- sort(rules, by = "confidence",
                     decreasing = T)
inspect(rules_sorted[1:5])

# target item, if lhs -> what rhs
rules <- apriori(Groceries,
                 parameter = list(supp = 0.001,
                                  conf = 0.15,
                                  minlen = 2),
                 appearance = list(default = "rhs",
                                   lhs = "whole milk"))
rules_sorted <- sort(rules, by = "confidence",
                     decreasing = T)
inspect(rules_sorted[1:5])

# Question

rules <- apriori(Groceries,
                 parameter = list(supp = 0.001,
                                  conf = 0.3,
                                  minlen = 1),
                 appearance = list(default = "lhs",
                                   rhs = c("bottled beer", "canned beer")))
rules_sorted <- sort(rules, by = "confidence",
                     decreasing = T)
inspect(rules_sorted[1:5])

# Visualized rules
library(arulesViz)
plot(rules_sorted[1:5], method = "graph",
     interactive = T, shading = NA)
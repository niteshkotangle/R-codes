install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)


# get the data in the library to see how certain functions work
data("Groceries")
summary(Groceries)

# take a look at the data format
inspect(head(Groceries))

# Plot the top 50 items in terms of relative frequency (the same as support)
itemFrequencyPlot(Groceries, topN=50, type='relative')

# Generate the association rule
rules <- apriori(Groceries, 
                 parameter=list(supp=0.01, conf=0.3))

# Sort the rules according to confidence
rules <- sort(rules, by="confidence", decreasing = TRUE)
summary(rules)


# Inspect top 30 rules
inspect(rules[1:30])

# Inspect rules which has lift >3
inspect(subset(rules, lift> 3))

# By default, plot() can generate a heatmap based on arulesViz
plot(rules)

#Visualize top 30 rules as network of items
topRules <- rules[1:30]
plot(topRules, method = "graph")

#You can have interactive graph by specifying the engine parameter
plot(topRules, method="graph", engine = "interactive")


require(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg))


# graph1
graph1file=file.path(getwd(), "graph1.Rds")
saveRDS(p, graph1file)

# graph2
graph2file=file.path(getwd(), "graph2.Rds")
saveRDS(p, graph2file)

# graph3
normalizePath(".")
graph3file=file.path(getwd(), "graph3.Rds")
saveRDS(p, graph3file)

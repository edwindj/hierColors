sbi <- read.csv("data/sbi_all.csv")

parentcode <- character(nrow(sbi))

codes <- levels(sbi$SBI.level)
parents <- head(codes, -1)
children <- tail(codes, -1)

for (i in seq_along(parents)){
  child <- children[i]
  parent <- parents[i]
  parent.codes <- sbi[[parent]]
  
  w.p <- which(sbi$SBI.level == parent)
  w.c <- which(sbi$SBI.level == child)
  
  parentcode[w.c] <- as.character(sbi$code[w.p[match(parent.codes[w.c], parent.codes[w.p])]])
}
parentcode

sbi$parentcode <- parentcode

head(sbi)

require(igraph)
g <- graph.data.frame(sbi[c("code", "parentcode", 'labels')])

plot(g, layout=layout.kamada.kawai, vertex.color="green")




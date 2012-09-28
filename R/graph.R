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

sbi2 <- sbi[ordered(sbi$SBI.level) <= "SBI2",]
sbi2.g <- sbi2[c("code", "parentcode", 'labels')]
sbi2.g <- rbind(sbi2.g, data.frame(code="", parentcode=NA, labels="Total"))
rd <- recursiveDivide(sbi2[c("SBI1", "SBI2")])
require(igraph)
g <- graph.data.frame(sbi2.g)
g <- g - 109

labels <- c(as.character(sbi2$code))
chroma = 25*nchar(labels)

cols <- c(hcl(360*rd, c=chroma), "white")
labels <- c(labels, "")

plot(g, layout=layout.fruchterman.reingold, vertex.color=cols, vertex.label=labels)
tkplot(g, layout=layout.fruchterman.reingold, vertex.color=cols, vertex.label=labels)




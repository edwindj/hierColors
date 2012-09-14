sbi <- read.csv("data/sbi2008versie2012.csv", stringsAsFactors=FALSE, header=FALSE)

names(sbi) <- c("code", "labels")
sbi$code <- gsub(" ", "", sbi$code)

empty <- sbi$code == ""
sbi  <- sbi[!empty, ]

SBI1_idx <- which(sbi$code %in% LETTERS)
SBI1 <- rep(sbi$code[SBI1_idx], times=diff(c(SBI1_idx, 1 + nrow(sbi))))

sbi$SBI1 <- SBI1

sp <- strsplit(sbi$code, ".", fixed=TRUE)

SBI2 <- sapply(sp, function(s){ 
  n <- 1
  if (length(s) >= n) s[[n]]
  else NA
}
)

SBI2 <- sprintf("%02d", as.integer(SBI2))
is.na(SBI2) <- SBI2 == "NA"
SBI2 <- as.factor(SBI2)

sndpart <- sapply(sp, function(s){ 
                        n <- 2
                        if (length(s) >= n) s[[n]]
                        else NA
                        }
                      )

SBI3 <- as.factor(as.integer(substr(sndpart, 1, 1)))

SBI4 <- sapply(sp, function(s){ 
  n <- 2
  if (length(s) >= n) s[[n]]
  else NA
}
)

SBI4 <- as.factor(as.integer(substr(sndpart, 2, 2)))

sbi$SBI2 <- SBI2
sbi$SBI3 <- SBI3
sbi$SBI4 <- SBI4

levs <- paste0("SBI", 1:4)
SBI.level <- rowSums(!sapply(sbi[levs], is.na))
SBI.level <- factor(SBI.level, labels=levs)

sbi$SBI.level <- SBI.level
write.csv(sbi, "data/sbi_all.csv", row.names=FALSE, na="")
sbi$SBI.level <- NULL

s <- split(sbi, SBI.level)
for (n in names(s)){
  write.csv(s[[n]], paste0("data/",n,".csv"), row.names=FALSE, na="")
}

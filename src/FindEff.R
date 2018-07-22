filenames = c("SolData2/310815.txt",
              "SolData2/010915.txt",
              "SolData2/020915.txt",
              "SolData2/030915.txt",
              "SolData2/040915.txt",
              "SolData2/050915.txt",
              "SolData2/060915.txt")

soldata = lapply(filenames, function(i) {
  read.csv(i, sep="\t")
})

soldata = do.call(rbind.data.frame, soldata)

library(dplyr)
library(rafalib)

Sol = select(soldata, c(2)) %>% unlist

Pmax1 = select(soldata, c(30)) %>% unlist
Pmax2 = select(soldata, c(35)) %>% unlist
Pmax3 = select(soldata, c(40)) %>% unlist
Pmax4 = select(soldata, c(45)) %>% unlist
Pmax5 = select(soldata, c(20)) %>% unlist
Pmax6 = select(soldata, c(50)) %>% unlist
Pmax7 = select(soldata, c(25)) %>% unlist
Pmax8 = select(soldata, c(15)) %>% unlist
Pmax9 = select(soldata, c(55)) %>% unlist
Pmax10 = select(soldata, c(10)) %>% unlist

Areal1 = 1.94
Areal2 = 1.94
Areal3 = 1.71
Areal4 = 1.71
Areal5 = 1.07
Areal6 = 1.39
Areal7 = 0.54
Areal8 = 0.63
Areal9 = 0.44
Areal10 = 1.01

Effektivitet1 = (Pmax1 / Areal1) / Sol
Effektivitet2 = (Pmax2 / Areal2) / Sol
Effektivitet3 = (Pmax3 / Areal3) / Sol
Effektivitet4 = (Pmax4 / Areal4) / Sol
Effektivitet5 = (Pmax5 / Areal5) / Sol
Effektivitet6 = (Pmax6 / Areal6) / Sol
Effektivitet7 = (Pmax7 / Areal7) / Sol
Effektivitet8 = (Pmax8 / Areal8) / Sol
Effektivitet9 = (Pmax9 / Areal9) / Sol
Effektivitet10 = (Pmax10 / Areal10) / Sol
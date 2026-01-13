source("funs.R")
source("funs2.R")
source("inline.R")
source("get2.R")
print(nrow(abs))
source("dups.R")
print(nrow(abs))
source("html.R")


inline("byDept.html", "abstractsByDept.html")


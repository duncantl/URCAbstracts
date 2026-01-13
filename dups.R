idx2 = paste(abs$year, abs$title, abs$studentName, abs$abstract, abs$sponsor, abs$text, sep = "--")
table(duplicated(idx2))
# Only one of idx2 is duplicated.
# We can remove this.
abs = abs[!duplicated(idx2), ]

if(FALSE) {

    
dupByVar =
function(var, x = abs)
{
    a = x[var]
    a$sep = "--"
    idx = do.call(paste, a)
    g = split(x, idx)
    g[sapply(g, nrow) > 1]
}


z = dupByVar(c("title", "studentName", "abstract", "sponsor"))
table(sapply(z, function(x) length(unique(x$year))) > 1)
# All are duplicates across years.


z = dupByVar(c("title", "abstract", "sponsor"))
table(sapply(z, function(x) length(unique(x$studentName))) > 1)
table(sapply(z, function(x) length(unique(x$year))) > 1)


z = dupByVar(c("title"))
table(sapply(z, function(x) length(unique(x$year))) > 1)


w1 = duplicated(abs$title)
w2 = duplicated(abs$abstract)

idx = paste(abs$year, abs$title, abs$abstract, sep = "--")
table(duplicated(idx))
# 590 duplicates




g = split(abs, abs$title)
g = g[sapply(g, nrow) > 1]


# same title and abstract, different student
# same title and abstract, different year but same student

}

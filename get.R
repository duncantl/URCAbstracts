source("funs.R")
library(PRM)

xml = list.files(pattern = "\\.xml$")
# drop 2019 as gives error about excessive number of XML nodes in getNodeSet
z = lapply(xml[-1], getSponsors)
df = do.call(rbind, z)
years = c(2020, 2021, 2023, 2024, 2025)
df$year = rep(years, sapply(z, nrow))
df$year = ordered(as.character(df$year))

tmp = split(df, df$name)
tmp2 = lapply(tmp, function(x)  {
    ans = structure(rep(0, length(years)), names = years)
    tt = table(x$year)
    ans[names(tt)] = tt
    ans
})

tmp3 = as.data.frame(do.call(rbind, tmp2))
fac = cbind(do.call(rbind, lapply(tmp, function(x) x[1,])), tmp3)



#who = unique(df$name)
who = trimws(fac$name)

#
fix = readxl("FacNamesToFix.xlsx")
ww = !is.na(fix$email) & grepl("@$", fix$email)
fix$email[ww] = paste0(fix$email[ww], "ucdavis.edu")


m = match(who, fix$orig)
w = !is.na(m)
fac$fix[w] = who[w] = fix$fix[ m[w] ]


# 
info = lapply(who, function(w) try(prmQuery(w, curl = prm, asDf = TRUE)))
names(info) = who
err = sapply(info, inherits, 'try-error')

m2 = match(who[err], fix$fix)

table(is.na(m2))

fix2 = fix[m2,]

# Use any login
ww = !is.na(fix2$login)
tmp0 = lapply(fix2$login[ww], getPRMInfo, curl = prm, asDf = TRUE)
info[ fix2$fix[ww] ] = tmp0

# Now use the emails w/o a login
ww = !is.na(fix2$email) & is.na(fix2$login)
tmp = lapply(fix2$email[ww], function(e) try(prmQuery(e, curl = prm, asDf = TRUE)))
names(tmp) = fix2$fix[ww]
err2 = sapply(tmp, inherits, 'try-error')

# 2 errors - ccthomase and maycho.
info[ names(tmp) ] = tmp

# 6 people have no email and no login. Ignore. Couldn't find them as faculty.
ww = is.na(fix2$email) & is.na(fix2$login)


err = sapply(info, inherits, 'try-error')


###
# add loginid to fac
m = match(fac$name, names(info))


#########################################
# Not used

############


w = is.na(fix$email) & is.na(fix$login)
match(
tmp = lapply(fix$fix[w], function(w) try(prmQuery(w, curl = prm, asDf = TRUE)))

###

info2 = lapply(who, function(w) try(prmQuery(w, curl = prm, asDf = TRUE)))
names(info2) = who

err = sapply(info2, inherits, 'try-error')
# 51 with errors.




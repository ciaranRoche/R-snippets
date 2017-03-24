tmp <- as.data.frame(UCBAdmissions)               # convert tables to data frame
tmp <- lapply(tmp, function(x) rep(x,tmp$Freq))   # repeat rows based on given frequencies 
tmp <- as.data.frame(tmp)                         # convert to data frame
admit <- tmp[,c("Admit","Gender","Dept")]         # drop unneeded columns


a <− admit
freq <− table(a$Admit, a$Gender)
freq
rel_freq <− prop.table(freq ,2)
rel_freq
png("admit_all .png")
barplot ( rel_freq , main="All Departments")
dev.off()

# individual departments
for (d in c("A","B","C","D","E","F")) {
# select (subset of rows) based on department (and all columns)
a <− admit[admit$Dept==d, ]
freq <− table(a$Admit, a$Gender) print ( sprintf ("Department %s" , d)) print(freq)
rel_freq <− prop.table(freq ,2)
print ( sprintf ("Department %s" , d)) print(rel_freq)
png(sprintf("admit_%s.png", d))
barplot ( rel_freq , main=sprintf ("Department %s" , d)) dev.off()
}
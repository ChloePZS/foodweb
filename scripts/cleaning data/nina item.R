sum <- dplyr::summarise(group_by(datatest, site_code, fish_sp), tot = length(item_cor))

test <- dplyr::left_join(datatest, sum)
length(which(is.na(test$item_fam_cor) & test$tot > 1))

sum(is.na(test$item_fam_cor))

test2 <- test[is.na(test$item_fam_cor) & test$tot > 1,]
nrow(unique(dplyr::select(test, site_code, fish_sp)))

sel <- which(is.na(test$item_fam_cor) & test$tot > 1) #rows for which fam = NA and more than one observations/sp

test_sel <- test[-sel,]

#Great but pb is it would remove all fam=NA if nobs = 0, so loss of many obs ...

##Modif Chloe
sum <- dplyr::summarise(group_by(datatest, site_code, item_class,fish_sp ), tot = length(item_cor))

test <- dplyr::left_join(datatest, sum)
length(which(is.na(test$item_ord_cor) & test$tot > 1))

sum(is.na(test$item_ord_cor))

class(test$item_cor)
class(test$item_ord_cor)
test$item_cor <- as.character(test$item_cor)
test$item_ord_cor <- as.character(test$item_ord_cor)

test2 <- test[is.na(test$item_ord_cor) & test$tot > 1,]
test2 <- test %>% filter(is.na(test$item_fam_cor) & test$tot > 1 & test$item_cor!=test$item_ord_cor)

nrow(unique(dplyr::select(test, site_code, fish_sp)))

sel <- which(is.na(test$item_ord_cor) & test$tot > 1) #rows for which fam = NA and more than one observations/sp

test_sel <- test[-sel,] #still remove way too much infos, especially for Harmelin Vivien and
#it doent removes duplicates


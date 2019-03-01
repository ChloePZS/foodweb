sum <- dplyr::summarise(group_by(datatest, site_code, fish_sp), tot = length(item_cor))

test <- dplyr::left_join(datatest, sum)
length(which(is.na(test$item_fam_cor) & test$tot > 1))

sum(is.na(test$item_fam_cor))

test2 <- test[is.na(test$item_fam_cor) & test$tot > 1,]
nrow(unique(dplyr::select(test, site_code, fish_sp)))

sel <- which(is.na(test$item_fam_cor) & test$tot > 1)

test_sel <- test[-sel,]

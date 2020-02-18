load("sem_fit2.Rdata")

summary(semtest2)

inspect(semtest2)

cov_mat <- fitted(semtest2)

cov_mat <- cov_mat$cov

cor_mat <- cov2cor(cov_mat)

corrplot(cor_mat) # not interpretable

grep("C22A", colnames(cor_mat))

corrplot(cor_mat[1:64, 1:64] %>% select("B3", "B1", "B5", "B19", "B21B",
                                        "C1", "C2", "C7", "C10", "C11", "C12", "C13",
                                        "C17A", "C20B", "C22B"))

grep("B3", colnames(cor_mat))
grep("B1", colnames(cor_mat))
grep("C2", colnames(cor_mat))
grep("C17A", colnames(cor_mat))
grep("C20B", colnames(cor_mat))
grep("C22B", colnames(cor_mat))

corrplot(cor_mat[c(1, 2, 3, 4, 5, 14, 15, 16, 17, 18, 19, 20, 21, 33, 45), c(1, 2, 3, 4, 5, 14, 15, 16, 17, 18, 19, 20, 21, 33, 45)])

# locally run and force push to preview branch

source("script/update-testdir.R")

library(gert)
git_branch_create("preview", checkout = TRUE)
git_add('data/testdir.csv')
git_commit("[local user] update from salesforce")
git_push(force = TRUE)


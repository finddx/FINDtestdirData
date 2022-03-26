# locally run and force push to preview branch

source("script/update-testdir.R")

library(gert)

# start with fresh branch 'preview', based on 'main'

git_branch_create("preview")

# add file and force-push, so no history is left by frequent updates
git_add('data/testdir.csv')
git_commit("[local user] update from salesforce")
git_push(force = TRUE)

git_branch_checkout("main")
git_branch_delete("preview")

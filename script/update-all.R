# locally run and force push to preview branch


library(glue)
library(gert)


process_dataset <- function(dataset = "testdir") {

  # start with fresh branch 'preview', based on 'main'

  branch <- paste0("preview-", dataset)

  # start with a fresh preview branch
  git_branch_checkout("main")
  if (git_branch_exists(branch)) {
    git_branch_delete(branch)
  }
  git_branch_create(branch)

  # update data
  source(paste0("script/update-", dataset, ".R"))

  # add file and force-push, so no history is left by frequent updates
  git_add(paste0("data/", dataset, ".csv"))
  if (any(git_status()$staged)) {
    git_commit(paste0("[local user] update '", dataset, "' from salesforce"))
    git_push(force = TRUE)
  } else {
    message(dataset, ": no changes, doing nothing.")
  }

}


process_dataset("testdir")
process_dataset("selftests")

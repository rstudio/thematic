#!/bin/bash -e

# Mimic shinycoreci's approach to auto-generating GH branches
# for easy checkout and shinytest::view_test_diff()
SHORT_SHA_="$GITHUB_HEAD_REF$GITHUB_SHA"
SHORT_SHA=${SHORT_SHA_:0:7}
# Using `[[` to work well with windows as it doesn't like `""` in the middle of the expression
R_VERSION=$(Rscript -e "cat(paste0(R.version[['major']], '.', R.version[['minor']]))")

FAIL_TIME=$(date +%Y_%m_%d_%H_%M)
FAILED_BRANCH="gha-$FAIL_TIME-$R_VERSION-$RUNNER_OS"

# need to unset the extra header to allow for regular https push
# git checkout -B -- Creates the branch <new_branch> and start it at <start_point>; if it already exists, then reset it to <start_point>.
# git push --force -- JUST in case the prior branch existed
git config --local user.email "${GITHUB_ACTOR}@users.noreply.github.com" && \
  git config --local user.name "${GITHUB_ACTOR}" && \
  git config --local --unset http.https://github.com/.extraheader && \
  git config --local --list
  git fetch --unshallow

git checkout -B "$FAILED_BRANCH"

git commit tests/ -m "Add test files - rstudio/thematic@$SHORT_SHA"

git log -n 4 --pretty=oneline --simplify-by-decoration

git push --force "https://$GITHUB_ACTOR:$GITHUB_PAT@github.com/rstudio/thematic.git" "HEAD:$FAILED_BRANCH"

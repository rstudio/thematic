# Contributing

When contributing to this repository, please first discuss the change you wish to make via a [new issue](https://github.com/rstudio/thematic/issues/new), email, or any other method with the owners of this repository before [making a change](https://github.com/rstudio/thematic/blob/main/.github/PULL_REQUEST_TEMPLATE.md).

Please note we have a [code of conduct](CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.


## Managing visual tests

After the [R-CMD-check workflow](https://github.com/rstudio/thematic/actions?query=workflow%3AR-CMD-check) (i.e., automated testting) runs on GitHub Actions, any differences in visual (**shinytest** and/or **vdiffr**) baselines are pushed to a new GitHub branch. This makes it easier to approve and incorporate "false positive" or otherwise acceptable changes in those visual tests across platforms and R versions. To fetch and view the changes in the visual tests for a particular `$GHA_BRANCH`, run the following terminal commands:

```shell
git fetch
git checkout $GHA_BRANCH
R -e "thematic:::view_shinytest_diffs()"
```

Then, after approving relevant differences, to incorporate the changes into `$MY_BRANCH`, do:

```shell
git add -u tests
git commit -m "Approve new baselines"
git checkout $MY_BRANCH
git merge $GHA_BRANCH
git push origin $MY_BRANCH
```

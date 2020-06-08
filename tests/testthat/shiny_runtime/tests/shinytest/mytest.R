app <- ShinyDriver$new("../../runtime.Rmd", seed = 46815)
app$snapshotInit("mytest")

app$snapshot()

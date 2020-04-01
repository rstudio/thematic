app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(foo = "svg")
app$snapshot()
app$setInputs(foo = "jpeg")
app$snapshot()

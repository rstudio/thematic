app <- ShinyDriver$new("../../")
Sys.sleep(10)

app$snapshotInit("mytest")

app$snapshot()

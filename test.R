

test <- c("this", "that", "", "some more")
test
test != ""
x <- test != ""
test[x]

test[test != ""]

test <- test[test != ""]

test


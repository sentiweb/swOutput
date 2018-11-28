out("toto")

x = 1:10

out(x, title="titre")

data(women)

out(women, title="women", last.row=TRUE)

tt = as.table(c(10,10,22,12))

out(tt, title="Mon table")

out(chisq.test(tt))

out(women, title="women", name="women")

xbloc("Titre 1")
  xbloc("titre 2")
    out("testx")
  xbloc(end=TRUE)
xbloc()


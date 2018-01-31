library(swOutput)

init_output(type="html")

out("toto")


data(women)

out(women, title="women")

tt = as.table(c(10,10,22,12))

out(chisq.test(tt))

out(women, title="women", name="women")

nick <- iterQijmat
bob = 0
for(i in 1:nrow(nick)) {
  bob <- bob + nick$Qij[[i]][1]
}

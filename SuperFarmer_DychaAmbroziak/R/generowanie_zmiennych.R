rows = c("rabbit","small_dog","sheep","pig","big_dog","cow","horse")
columns = c("value", "max.count", "win.condition", "count")

value         = c( 1, 6,  6, 12, 36, 36, 72)
max.count     = c(60, 4, 24, 20,  2,  6,  6)
win.condition = c( 1, 0,  1,  1,  0,  1,  1)
count         = c( 0, 0,  0,  0,  0,  0,  0)

game = cbind(value, max.count, win.condition, count)
colnames(game) = columns
rownames(game) = rows
game

die1 = c("wolf",   "cow", "pig", "sheep", "sheep", "sheep", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit")
die2 = c( "fox", "horse", "pig",   "pig", "sheep", "sheep", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit")

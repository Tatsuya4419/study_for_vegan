library(vegan)
library(ggplot2)


data("BCI")


# レアファクションカーブの傾きリスト
slopes <- apply(BCI, 1, function(.x) rareslope(.x, 1:(sum(.x))-1))


# 下記二つは同じ結果を返すことから、
# 傾きを正しく計算できているとわかる
# slopes[[1]][1:3]
# rareslope(BCI[1,], 1:3)


# 最終サンプルのカバレッジを返す
maxslopes <- sapply(slopes, function(.x) .x[length(.x)])


# カバレッジパーセントを表示
covpct <- (1 - maxslopes) * 100


# カバレッジ90%をレアファクションの基準とし、
# これに満たないサンプルは取り除いてみる
minslope <- 0.1


# サンプリングするSpeciesの数を求める
sample.sizes <- sapply(slopes, function(.x) {
  sample.size <- min(which(.x <= minslope)) + 1
  if(is.infinite(sample.size)) {
    return(NA)
  } else {
    return(sample.size)
  }
})


for(i in 1:length(slopes)) {
  if(is.na(sample.sizes[i])) {
    return()
  } else {
    print(c(as.integer(i), slopes[[i]][sample.sizes[i]]))
  }
}


# 各フィールドにおけるサンプリングパーセントを表示
sample.sizes / rowSums(BCI) * 100


# レアファクション
# 
# 下2行を実行すると、BCIからsample.sizesがNAであるフィールドを
# 取り除いていることを確認できる
# na.omit(sample.sizes)
# rownames(BCI[!is.na(sample.sizes),])
set.seed(123)
BCI.rarefied <- rrarefy(BCI[!is.na(sample.sizes),], na.omit(sample.sizes))


# レアファクションカーブを描画するための
# 数値を取得
tidy.df <- rarecurve(BCI.rarefied, tidy = T)


# プロット
ggplot(data = tidy.df, aes(x = Sample, y = Species, group = Site)) + 
  geom_line()





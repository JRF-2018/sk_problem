

    「志村けん問題」を JAGS で解く その１ 結論編
    (Created: 2020-05-08, Time-stamp: <2020-05-08T20:22:45Z>)


** 概要

新型コロナウィルス(COVID-19)により稀代のコメディアン・志村けん氏が亡く
なった。その志村けん氏がまだ闘病中だったころ、新型コロナについて一つの
確率的問題が提起された。

志村けん氏ほどの有名人が新型コロナに罹ったということは、感染者数は言わ
れているよりもっと多いに違いない。…これは本当か。

…というのが、その問題のだいたいである。

「それは本当だ」というのを R 言語上のベイズ統計モデリングソフト JAGS
を用いて示せたというのが本稿の主張である。

** はじめに

詳しい経緯は↓にある。これを「元記事」として本稿では参照する。

《志村けんのパラドックス - アスペ日記》  
https://takeda25.hatenablog.jp/entry/2020/03/28/083257

まず最初に問題となるツイートがあった。

《Twitter:森岡正博:2020年3月25日》
https://twitter.com/Sukuitohananika/status/1242698846032953345
> みんな冷静に計算してほしいけど、東京都の新コロナ感染者数は現在１７１
> 人。東京から無作為に２００人をピックアップしたときに、その中に超有名
> 人の志村けん氏が入ってる確率ってどのくらいだと思う？　現在の感染拡大
> ペースは我々の想像をはるかに超えてるよ。桁違いの感染者数になってるよ。

これが間違っているか、あっているかで議論となった。

「元記事」では次のように結論付けている。

> 例の哲学者の人が
> 
>   * この病気は誰でも平等にかかるものだ
> 
>   * 政府は感染者数を隠蔽しているかもしれない
> 
>   * 政府は有名人の感染については隠蔽できないだろう
> 
> という前提を持っているのであれば、志村けんさんの感染によって彼が「政
> 府は感染者数を隠蔽しているんだろう」という確信を深めるのは、確率の考
> え方としては、完全に理にかなったことです。
> 
> 有効な反論は、それらの前提に対する
> 
>   * 志村けんは特別に感染確率が高かったんだろう
> 
>   * 政府が感染者数を隠蔽するなんて無理だろう
> 
>   * その気になれば有名人の感染も隠蔽できるだろう
>
> といったものです。

ここでは、その問題を次のようになおす。

「100人に1人の有名人の感染がわかった集団と、そういう者がいない集団の間
で感染者数の推定に差があるか。」

そして、

「その有名人は感染確率の高い集団に属していてその高さが異なる場合、また、
政府の信頼性が異なる場合、また、隠蔽の巧拙が異なる場合について、推定さ
れる差が変化するか。」

…という問題を検討することにする。

元のツイートに比べて「200人を無作為に抽出して1人…」というのが抜けるが、
それに近いことについては、「その２ 試行錯誤編」のほうで扱うことにする。
結果的にいうと、それを考えに入れるモデルは、私の実力では ESS
(Effective Sample Size, 実効サンプル数) が足りなくなり、うまく扱えなかっ
た。

** 実装

問題の実装は、R 言語 3.6.3、JAGS 4.3.0、rjags 4.10 を使った。

試行錯誤の末に辿り着いたのが次のコードになる。

<source>
data {
  # N <- 100000000
  # n.fam.sk <- 100
  # filter.fam <- 0.01
  # n.risk <- 1000000
  # mag.risk <- 2
  # mag.all <- 10
  # alpha.pos <- 1
  # beta.pos <- 2
  one1 <- 1
}

model {
  p.pos.sk.temp ~ dbeta(alpha.pos, beta.pos)
  p.pos.nsk.temp ~ dbeta(alpha.pos, beta.pos)
  p.pos.sk.risk <- p.pos.sk.temp * mag.risk / mag.all
  p.pos.nsk.risk <- p.pos.nsk.temp * mag.risk / mag.all
  p.pos.sk.nrisk <- p.pos.sk.temp / mag.all
  p.pos.nsk.nrisk <- p.pos.nsk.temp / mag.all

  n.pos.sk.risk ~ dbinom(p.pos.sk.risk , n.risk - n.fam.sk)
  n.pos.nsk.risk ~ dbinom(p.pos.nsk.risk , n.risk)
  n.pos.sk.nrisk ~ dbinom(p.pos.sk.nrisk, N - n.risk)
  n.pos.nsk.nrisk ~ dbinom(p.pos.nsk.nrisk, N - n.risk)

  for (i in 1:n.fam.sk) {
    mf.fam[i] ~ dbern(p.pos.sk.risk)
    mf.f[i] ~ dbern(filter.fam)
    mf.fam.f[i] <- mf.fam[i] * mf.f[i]
  }
  one1 ~ sum(step(sum(mf.fam.f) - 1) * 1, 0)

  n.pos.sk <- n.pos.sk.risk + n.pos.sk.nrisk + sum(mf.fam)
  n.pos.nsk <- n.pos.nsk.risk + n.pos.nsk.nrisk

  diff <- n.pos.sk - n.pos.nsk
  gr <- n.pos.sk > n.pos.nsk
}
</source>

sk は志村けん氏のいる群、nsk は志村けん氏のいない群を現わす。pos は感
染者で、risk はリスク群、nrisk は非リスク群、fam はリスク群をかねた有
名人群を示す。n は人数で、p は割合、mf は famous man。filter は政府や
マスコミによりフィルターアウトされない確率を表し、.f はフィルターアウ
トされなかった者を表す。

リスク群を 2 倍から 10 倍のリスクに設定するため、逆にそうでないリスク
はあらかじめ 1/10 している。それが p.pos.sk.temp や p.pos.nsk.temp の
テクニックである。

one1 ~ sum(step(sum(mf.fam.f) - 1) * 1, 0) は、TRUE ~ (sum(mf.fam.f)
>= 1) を表し、有名人の数(n.fam.sk)の中で、感染した上でフィルターアウト
されなかった者(mf.fam.f[i] == 1)の合計(sum)が 1 以上であることを示す。
これが制約条件のようになっている。

知りたいのは diff で、志村けん氏のいる群の感染者数 n.pos.sk と志村けん
氏のいない群の感染者数 n.pos.nsk の差を比較する。

わかりやすいだろうと思って、sk_problem18.jag を先に見せたが、ベルヌー
イ分布 dbern を使っている部分は、二項分布 dbinom を使って書き直すこと
ができる。そうしたのが、sk_problem19.jag で次のようになる。

<source>
data {
  one1 <- 1
}

model {
  p.pos.sk.temp ~ dbeta(alpha.pos, beta.pos)
  p.pos.nsk.temp ~ dbeta(alpha.pos, beta.pos)
  p.pos.sk.risk <- p.pos.sk.temp * mag.risk / mag.all
  p.pos.nsk.risk <- p.pos.nsk.temp * mag.risk / mag.all
  p.pos.sk.nrisk <- p.pos.sk.temp / mag.all
  p.pos.nsk.nrisk <- p.pos.nsk.temp / mag.all

  n.pos.sk.risk ~ dbinom(p.pos.sk.risk , n.risk - n.fam.sk)
  n.pos.nsk.risk ~ dbinom(p.pos.nsk.risk , n.risk)
  n.pos.sk.nrisk ~ dbinom(p.pos.sk.nrisk, N - n.risk)
  n.pos.nsk.nrisk ~ dbinom(p.pos.nsk.nrisk, N - n.risk)

  n.fam.f ~ dbinom(filter.fam, n.fam.sk)
  n.pos.fam.f ~ dbinom(p.pos.sk.risk, n.fam.f)
  n.pos.fam.nf ~ dbinom(p.pos.sk.risk, n.fam.sk - n.fam.f)
  one1 ~ sum(step(n.pos.fam.f - 1) * 1, 0)

  n.pos.sk <- n.pos.sk.risk + n.pos.sk.nrisk + n.pos.fam.f + n.pos.fam.nf
  n.pos.nsk <- n.pos.nsk.risk + n.pos.nsk.nrisk

  diff <- n.pos.sk - n.pos.nsk
  gr <- n.pos.sk > n.pos.nsk
}
</source>

one1 ~ sum(step(n.pos.fam.f - 1) * 1, 0) は、TRUE ~ (n.pos.fam.f >= 1)
ということがやりたいのである。

これを実行するには R で次のように行う。

<source>
library(rjags)
library(coda)

m <- jags.model('sk_problem19.jag',
                data = list(
                    N = 100000000,
                    n.fam.sk = 100,
                    n.risk = 1000000,
                    filter.fam = 0.01,
                    mag.risk = 2, mag.all = 10,
                    alpha.pos = 1, beta.pos = 2
                ),
                inits = list(
                    p.pos.sk.temp = 0.001,
                    p.pos.nsk.temp = 0.001,
                    n.pos.sk.risk = 5,
                    n.pos.nsk.risk = 5,
                    n.pos.fam.f = 1
                ),
                n.chains=4)
update(m, 10000)
post.list <- coda.samples(m, c('n.pos.sk', 'n.pos.nsk', 'diff', 'gr'),
                          n.iter=10000)
print(summary(post.list))
print(effectiveSize(post.list))
</source>

結果は次のようになる。

<source>
> print(summary(post.list))

Iterations = 11001:21000
Thinning interval = 1 
Number of chains = 4 
Sample size per chain = 10000 

1. Empirical mean and standard deviation for each variable,
   plus standard error of the mean:

               Mean        SD  Naive SE Time-series SE
diff      1.627e+06 3.277e+06 1.639e+04      1.831e+04
gr        6.940e-01 4.608e-01 2.304e-03      2.445e-03
n.pos.nsk 3.365e+06 2.385e+06 1.192e+04      1.192e+04
n.pos.sk  4.992e+06 2.270e+06 1.135e+04      1.429e+04

2. Quantiles for each variable:

              2.5%     25%     50%     75%   97.5%
diff      -5075426 -592374 1718641 4006771 7591012
gr               0       0       1       1       1
n.pos.nsk   123516 1363403 2944500 5052312 8526503
n.pos.sk    938800 3197919 4986752 6747554 9139378

> print(effectiveSize(post.list))
     diff        gr n.pos.nsk  n.pos.sk 
 32095.21  35553.71  40039.25  25275.07 
</source>

Mean を見ると、n.pos.sk のほうが、n.pos.nsk よりも大きいのがわかる。志
村けん氏が感染したことで、感染者数の見積りを大きくすることは間違ってな
さそうだ。しかし、diff は 0 をはさんでかなり散らばっており、「確信を持っ
て大きい」とまでは言えない。

いちおう、グラフに書くと次のようになる。n.pos.sk のほうが大きいほうに
シフトしているのがわかる。

{{fig-n-pos-sk-nsk.png}}

次に、リスクの高い集団のリスクの高さ mag.risk を 2 から 10 に上げてみ
よう。すると、結果の一部の Mean に関する部分は次のようになる。

<source>
diff      1.606e+06 3.533e+06 1.767e+04      2.108e+04
n.pos.nsk 3.609e+06 2.561e+06 1.280e+04      1.285e+04
n.pos.sk  5.215e+06 2.432e+06 1.216e+04      1.701e+04
</source>

n.pos.sk は少し大きくなったが、それ以上に n.pos.nsk が大きくなり、結果、
diff はほんの少し小さくなっている。リスク集団のリスクが上がるとほんの
少し差がなくなる傾向があると言えるかもしれない。

次に mag.risk を 2 に戻した上で、政府の信頼性が異なる場合を試すことを
考える。これは事前確率の形が違うことに相当しよう。特にベータ分布の集中
度が上がることが、政府がより信頼されることに相当すると思われる。そこで、
alpha.pos = 1、beta.pos = 2 を alpha.pos = 2、beta.pos = 4 に変えてみ
る。その結果の一部の Mean に関する部分は次のようになる。

<source>
diff      9.021e+05 2.527e+06 1.263e+04      1.421e+04
n.pos.nsk 3.380e+06 1.803e+06 9.015e+03      9.015e+03
n.pos.sk  4.282e+06 1.767e+06 8.834e+03      1.108e+04
</source>

diff は少し小さくなる。n.pos.sk も少し小さくなっているようだ。政府の信
頼度が上がると、差がなくなる傾向にあると言えるようだ。

次に、alpha.pos = 1、beta.pos = 2 に戻した上で、隠蔽の巧拙が異なる場合
を試すことにする。filter.fam を 0.01 から 0.9 に上げてみる。その結果の
一部の Mean に関する部分は次のようになる。

<source>
diff      3.179e+05 3.295e+06 1.648e+04      3.875e+04
n.pos.nsk 3.355e+06 2.378e+06 1.189e+04      1.192e+04
n.pos.sk  3.673e+06 2.291e+06 1.145e+04      3.692e+04
</source>

n.pos.sk も diff もそこそこ小さくなった。隠蔽がなくなれば、差はなくな
る傾向にあると言えそうだ。

最後に、全部をいっしょに試す。mag.risk = 10, alpha.pos = 2, beta.pos =
4, filter.fam = 0.9 で試す。その結果の一部の Mean に関する部分は次のよ
うになる。

<source>
diff      7.064e+03 2725747.4 1.363e+04      6.021e+04
n.pos.nsk 3.641e+06 1943851.8 9.719e+03      9.676e+03
n.pos.sk  3.648e+06 1924074.9 9.620e+03      6.132e+04
</source>

n.pos.sk は n.pos.nsk とほとんど変わらず、diff はほぼ誤差と言えるくら
いかなり小さくなった。元記事の反論は有効であったと言えよう。


** 結論

100人に1人の有名人の感染がわかった集団と、そういう者がいない集団の間
で感染者数の推定に差があることがわかった。

さらに、その有名人が感染確率の高い集団に属していてその確率がより高く、
政府の信頼性が高く、また、隠蔽がなせれていない場合は、推定に差がなくな
ることがわかった。

いずれの推論も確信できるほどではないが、そのような推論をしても論理的に
間違いと言えないことは示せたはず。


** 参考

  * 《志村けんのパラドックス - アスペ日記》。元記事。

    https://takeda25.hatenablog.jp/entry/2020/03/28/083257

  * 《COVID-19 日本国内の潜在的な陽性者数を推定する試み - StatModeling
    Memorandum》。JAGS に興味を持ったのはこの記事を見て。

    http://statmodeling.hatenablog.com/entry/covid19-estimate-total-number-of-positives-in-japan

  * 『ベイズ統計モデリング - R, JAGS, Stan によるチュートリアル 原著第
    2版』(John K. Kruschke 著, 前田 和寛 ＆ 小杉 考司 監訳, 共立出版,
    2017年)。この本を見て JAGS に関して勉強した。本稿はその勉強の成果
    という側面もある。

    https://www.amazon.co.jp/dp/4320113160


** ライセンス

パブリックドメイン。 (数式のような小さなプログラムなので。)

自由に改変・公開してください。



    「志村けん問題」を JAGS で解く その２ 試行錯誤編
    (Created: 2020-05-08)


** 概要

JAGS に慣れていないこともあり、「その１ 結論編」に達するまでにはかなり
試行錯誤した。その「苦労」を記録しておく。

JAGS は確率に関する Prolog みたいなものという認識で、それがおもしろい
と思っていたのだが、「苦労」した結果、Prolog みたいに使うのは間違いか
と思うようになった。

** 初期モデル

2020年4月16日ごろに最初に作ったのは次のようなモデルである。
これを sk_problem01.jag とする。

<source>
data {
  # N <- 100
  # n.fam.sk <- 5
  SK <- 1
  one1 <- 1
  one2 <- 1
}

model {
  n.pos.sk ~ dbinom(p.pos.sk, N)
  n.pos.nsk ~ dbinom(p.pos.nsk, N)
  p.pos.sk ~ dbeta(0.5, 1)
  p.pos.nsk ~ dbeta(0.5, 1)
  diff <- n.pos.sk - n.pos.nsk
  gr <- n.pos.sk > n.pos.nsk

  for (i in 1:N) {
    pi.fam[i] <- 1
  }
  for (i in 1:N) {
    pi.pos[i] <- 1
  }
  for (i in 1:n.fam.sk) {
    m.fam[i] ~ dcat(pi.fam)
    eq.fam[i] <- equals(m.fam[i], SK)
  }
  for (i in 1:N) {
    m.pos[i] ~ dcat(pi.pos)
    eq.pos[i] <- ifelse(i <= n.pos.sk, equals(m.pos[i], SK), 0)
  }
  one1 ~ sum(step(sum(eq.fam) - 1) * 1, 0)
  one2 ~ sum(step(sum(eq.pos) - 1) * 1, 0)
}
</source>

まず、感染者数 n.pos.sk を決めてしまう。その上で、有名人 100人の中に志
村けん氏すなわち SK がいると同時に、n.pos.sk の中にも SK がいる…とい
うふうに考えた。(あとで述べるが、この想定は間違っている。)

番号 SK は 1 に固定して考える。SK は重複して選ばれうるが、1番であれば、
あまり問題はないはず。

ただ、このモデルは、N = 100 までが限界で、N = 10000 とかにするとほとん
ど止まってしまうのだった。

ここから、志村けん氏は特別なリスク群に属するとした場合、そのリスク群と
それ以外のリスクの差(倍率)はどれぐらいになるかを知ろうとしたのが
sk_problem02.jag である。かなり大きな倍率が必要となっていまい、「おか
しい」となる。

リスク群倍率 mag と n.pos.sk と n.pos.nsk の diff を同時に見ようとした
のが sk_problem03.jag になる。

「おかしい」ので、逆に志村けんとは逆の人物、非リスク群の人物も感染して
いるとしたらどうなるのかを調べたのが sk_problem04.jag になる。こうして
も、mag は変わらないはず…と考えていたのだが、結果は予想に反し、mag が
かなり抑えられることになった。

もしかして、有名人である必要はないのではないかと、sk_problem01.jag の
one1 の行をコメントアウトして作ったのが、sk_problem05.jag である。…こ
れにより有名人であることはほぼ意味のないことになっていることが判明した。

そこでロジックを変えたのが sk_problem06.jag である。まず、感染者数
n.pos.sk を決める。その上で、感染者の名前を上げていくと、有名人 100人
の中に含まれる者がいる。…というふうなモデルにした。どうもこのほうが正
しそうだ。sk_problem01.jag は間違っていた。

sk_problem06.jag に対する sk_problem02.jag に対応するのが
sk_problem07.jag、sk_problem06.jag に対する sk_problem04 に対応するの
が sk_problem08.jag である。

sk_problem2.jag の方向ではなく、逆に何倍リスクが高ければ、n.pos.sk と
n.pos.nsk に差がなくなるのだろうか？ それを探ろうと sk_problem09.jag
を作った。ここでは芸能人はすべてリスク群になるという仮定を置いている。
リスクは 1000倍ぐらいにしないと効果がない。そこまで倍率を大きくすると
リスク群が小さいほど差がなくなる。ただ、差のなくなり方はそれほど大きく
ない。

逆にリスクが少ないと思われている芸能人であれば結果は異なるのではないか。
それを試すのが sk_problem10.jag である。n.risk が 50 ぐらいであれば差
を大きくし、n.risk が 10 ぐらいであれば差は変わらない。


** 中期モデル

2020年4月30日、Kruschke『ベイズ統計モデリング』を6章まで読んで、エクサ
サイズ等を見ることでアイデアが浮かんだ。

dcat を使わない方法がわかった。その 1 が dbern を使うもの。その 2 が
dbinom を使うもの。その 1 を sk_problem11.jag として、その 2 を
sk_problem12.jag として実装した。まずまずうまくいったようだ。何より速
い。

「有名人も隠蔽」のフィルタを設定してみる。確率 0.9 で隠蔽に失敗すると
する。sk_problem06.jag sk_problem11.jag sk_problem12.jag に相当するも
のを sk_problem13.jag sk_problem14.jag sk_problem15.jag として作ってみ
る。

二項分布の結果が別の二項分布の結果に含まれるというのを指示するのに、不
等式を使えばいいのではないかと思って作ったのが sk_problem16.jag。

しかし、不等式が多いせいか、結果が安定しない。そこで、不等式を減らした
のが、sk_problem17.jag。それよりさらに不等式を減らしたのが
sk_problem18.jag でこれが「その１ 結論編」の最初のモデルである。

当初は、sk_problem18.jag は遅く、sk_problem17.jag のほうで「完璧か」と
思ったのだが、filter.fam を 0.5 から 0.9 に上げたときなぜか、差が開く
方向に出てしまう。sk_problem18.jag だとそのようなことはない。

これは、包含の不等式がまずいのだろうと考え直した。その上で、dbern を
dbinom に置き換える方法を思い付き、それをコード化したのが、
sk_problem19.jag になり、これが今回の完成形である。

このころになると、ESS (Effective Sample Size, 実効サンプル数) も気にな
り出す。幸いsk_problem19.jag はそれなりに ESS があるようだ。しかし、
ESS が小さくなっているのは不等式があるせいではないかと疑い、作ったのが
sk_problem20.jag で、これは志村けん問題とはほぼ関係がない。

sk_problem20.jag はそんなに ESS が小さくなるような感じはしないのだが、
n.iter = 10000 × n.chains = 4 に対し、n1 の ESS はたったの 10.81684
しかないという惨憺たるありさま。

sk_problem19.jag において不等式をコメントアウトしたのが
sk_problem21.jag。そこで mcmc.list を得たあとに、one1 の条件を満たす要
素数を数えてみた。sk_problem19.jag の ESS は、n.pos.sk について、
4482.228 ぐらい。sk_problem21.jag の one1 の条件を満たす要素数は、
28765 とかなり多い。不等式を使うと ESS はかなり小さくなるとは言えそう
だ。


** 終期モデル

2020年5月7日に Kruschke『ベイズ統計モデリング』を読み終わる。そこから
再度考える。

元のツイートでは「200人を無作為に抽出して1人…」という部分があった。そ
れを考慮したほうがいいかもしれない。

有名人が「1人以上」ではなく「1人だけ」感染しているとわかったというモデ
ルにする。200人ではなく2万人とかになれば、逆に有名人から1人しか出てい
ないことが効いてくるかも…などと考えていた。

感染者のうち、マスコミ等でフィルターされた人数 n.pos.pub (= 200)がわかっ
ているとするモデルにしたのが、sk_problem22.jag である。しかし、これの
ESS はとんでもなく小さくなり、まともな比較ができている様子はない。

リスク群をはじめから考えて、それについて sum を固定しようとしているの
がダメなのか…とリスク群を考えないようにしたのが、 sk_problem23.jag。
しかし、これも ESS がとんでもなく小さくなる。

その ESS を thinning などをしながら無理矢理大きくして結果をみると、ど
うやっても n.pos.sk と n.pos.nsk に差が出てこない。これは、フィルター
filter.pub が固定されているからだろう。ならば、filter.pub を動かそうと
したのが、sk_problem24.jag である。それは次のようになる。

<source>
data {
  # N <- 100000000
  # n.fam.sk <- 100
  # alpha.pos <- 0.5
  # beta.pos <- 1
  # n.pos.pub <- 200
  # filter.fam.mag <- 10
  # filter.pub <- 0.001
  # n.pos.pub.fam <- 1
  n.pos.pub.rest <- n.pos.pub - n.pos.pub.fam
}

model {
  p.pos.sk ~ dbeta(alpha.pos, beta.pos)
  p.pos.nsk ~ dbeta(alpha.pos, beta.pos)
  filter.pub.sk ~ dbeta(alpha.filter, beta.filter)
  filter.pub.nsk ~ dbeta(alpha.filter, beta.filter)
  filter.pub.fam <- min(1, filter.pub.sk * filter.fam.mag)

  n.pos.sk.nfam ~ dbinom(p.pos.sk, N - n.fam.sk)
  n.pos.nsk ~ dbinom(p.pos.nsk, N)

  n.pos.fam ~ dbinom(p.pos.sk, n.fam.sk)

  n.pos.pub ~ dbinom(filter.pub.nsk, n.pos.nsk)
  n.pos.pub.rest ~ dbinom(filter.pub.sk, n.pos.sk.nfam)
  n.pos.pub.fam ~ dbinom(filter.pub.fam, n.pos.fam)

  n.pos.sk <- n.pos.sk.nfam + n.pos.fam

  diff <- n.pos.sk - n.pos.nsk
  gr <- n.pos.sk > n.pos.nsk
}
</source>

しかし、この ESS は絶望的に小さく、結果私はいまだにちゃんと評価できて
いない。


** 結論

いろいろ苦労して、とりあえず「その１ 結論編」を書けたのは良かったが、
よりまともなモデルを作ろうとして、うまくいかなかった。

JAGS は確率に関する Prolog みたいなものという認識で、それがおもしろい
と思っていた。しかし、そのような使い方だと ESS が小さくなり、実質的に
評価ができないことがわかった。不等式を使う場合だけでなく、
sk_problem24.jag のような不等式のトリックを使わないようなものでも、ESS
がとても小さくなるのは意外だった。その「苦労」の結果、Prolog みたいに
使うのは間違いだと結論せざるを得ない。(そもそも Prolog 自体、万能推論
機でないので、逆にそこが似てるのかもしれないが。)



(This document is mainly written in Japanese/UTF8.)

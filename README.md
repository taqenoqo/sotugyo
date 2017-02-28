卒業判定機
==========

修得した単位をあらゆる科目区分に割り当てて
卒業できる方法がないか探す。

## 使い方

TWINS の単位修得状況照会でダウンロードできる CSV ファイルと、
科目区分を書いた yaml ファイルが必要。

```
$ stack exec sotugyo sample_data/seiseki.csv sample_data/kubun.yaml
留年確定です ^p^

科目区分が未定義の単位:
```

実行には SMT ソルバ Z3 <https://z3.codeplex.com/> のインストールが必要。



# dziwqrim-dict
## 依存

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
- LuaLaTeX
- EB Garamond
- Junicode
- Gentium Plus
- Source Han or Noto CJK

Haskellにより書かれているため、実行には[Haskell Stack](https://docs.haskellstack.org/en/stable/README/)が要る。

[How to install](https://docs.haskellstack.org/en/stable/README/#how-to-install)に従ってインストールすればUnix系でもWindowsでも動くはずである。

LuaLaTeXはTeXLiveを使うのがよい。

## 生成方法
スプレッドシートをプロジェクトディレクトリに `隋語常用漢字音表.csv` `諧聲.csv` の名で保存して、次のコマンドを実行する。

```
$ stack build && stack exec -- dziwqrim-dict-conv-exe 隋語常用漢字音表.csv 諧聲.csv data
$ matexmk -lualatex main.tex
```

# eijiro-util

## 概要

英辞郎 `https://www.eijiro.jp/` のテキストデータに関する次の機能を提供します。
なお、このアプリケーションはバージョン136に対応しています。

- P-Study System `https://www.takke.jp/` 用の問題集データを作成します。
- Shift JISフォーマットをUTF-8フォーマットに変換します。
- 英辞郎データに含まれる文字のコードポイントを分析します。

## 用語の定義

- アプリケーション
  - `eijiro-util` コマンドをいいます。
- アプリケーションオプション
  - `eijiro-util` に指定するオプションをいいます。
  - アプリケーションオプションは通常形および短縮形があります。
  - 例）`./eijiro-util --verbose` or `eijiro-util -v`
- アプリケーションコマンド
  - P-Study System用の問題集データ作成のためのコマンド `pstudy` や、英辞郎テキストデータのエンコーディングをUTF-8に変換するためのコマンド `utf8` など、個別の機能を指定するためのコマンドをいいます。
  - 例）`./eijiro-util pstudy`、 `./eijiro-util utf8`
- コマンドオプション
  - アプリケーションコマンドに対するオプションをいいます。
  - アプリケーションオプションと同様にコマンドオプションは通常形および短縮形があります。
  - 例） `./eiiro-util pstudy --help`、 `./eijiro-util pstudy -h`

## 使い方

### 英辞郎テキストデータの準備

- 市販の英辞郎またはダウンロード販売の英辞郎データを購入してください。
- 商品の指示にしたがってデータを解凍し、任意の場所に解凍されたテキストデータを保存してください。このアプリケーションを実行する際に、この解凍されたテキストデータを指定することになります。

### アプリケーションのヘルプ

- `./eijiro-util --help` コマンドでアプリケーションオプションおよびコマンドの一覧を確認できます。
- 各アプリケーションコマンドについては、`./eijiro-util アプリケーションコマンド名 --help` コマンドで各アプリケーションコマンドのヘルプを確認できます。

### `pstudy`: P-Study System用問題集を作成するアプリケーションコマンド

すべてのSVLを対象に指定したファイルにShift JISで結果を出力する場合:
`./eijiro-util pstudy 【英辞郎テキストデータのパス】 --file ./my_pstudy_data.csv`

SVL 1のみを対象に標準出力にUTF-8で結果を出力する場合で、さらに、実行過程および実行結果の情報を表示する場合:
`./eijiro-util -v pstudy 【英辞郎テキストデータのパス】 --level 1 --dest_encoding UTF-8`

### `utf8`: 英辞郎データをUTF-8に変換するアプリケーションコマンド

utf8コマンドのヘルプを参照してください。

### `analyse`: 英辞郎データに使用されている文字を分析するアプリケーションコマンド

#### targetオプションに指定する値
- `target`の値が`header`ならば、見出し語に含まれる文字とそのコードポイントを分析します。英辞郎のデータは「■みだし語 : 〜」というフォーマットになっており、このうち"■"と" : "に挟まれた部分の文字を抽出して、コードポイントを表示します。
- `target`の値が`label`ならば、ヘッダーに含まれる{}で囲まれたラベルを分析します。
- `target`の値が`attr`ならば、【】でかこまれた属性名を分析します。

#### 英辞郎データについて
- 英辞郎データの内容については `https://www.eijiro.jp/version.htm` を参照してください。
- 英辞郎データの仕様概要については `https://www.eijiro.jp/spec.htm` を参照してください。

## 開発情報

- Ubuntu 22.04.1 LTS
- Stack 2.9.3

## Pre Build

ビルド前に次の依存を解決してください。
- ユニコードライブラリ
- 圧縮ライブラリ

```sh
sudo apt-get install libicu-dev zlib1g-dev
```

## Build

- `stack build`

## Install

` stack install`

## Run

* `stack run -- [コマンド] [コマンドオプション]`

## Run tests

`stack test`

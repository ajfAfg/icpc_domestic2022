#!/bin/sh

FILE='judgedata.zip'

curl -o $FILE https://icpc.iisf.or.jp/past-icpc/domestic2022/judgedata.zip
unzip $FILE
rm $FILE

#!/bin/bash

path=$1

mkdir -p /home/taosdump/_0c4d322a4d

#导出:
taosdump -o /home/taosdump/_0c4d322a4d -u root _0c4d322a4d _0c4d322a4d -T 8
#导入:
taosdump -i /home/taosdump/_8778425df1 -u root _8778425df1 _a98222e5f4  -T 8

#!/bin/bash

cat RECORDS | while read line
	do 
		rdann -r $line -a atr -e* -v>csv/atr$line.txt
		ann2rr -r $line -a qrs -i s -v -V>csv/qrs$line.txt

done

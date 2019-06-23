#!/bin/bash

cat RECORDS | while read line
	do 	
		wqrs -r $line
		ann2rr -r  $line -a wqrs -i s -v -V>csv/wqrs$line.csv
done

		wqrs -r A04805
		ann2rr -r  A04805 -a wqrs -i s -v -V>csv/wqrsA04805.csv
#!/usr/bin/env sh
FILE=$1
ID=`basename $FILE .json`
curl -X PUT http://127.0.0.1:5984/otot/$ID --data-binary @FILE

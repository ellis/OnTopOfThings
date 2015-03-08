`curl http://127.0.0.1:5984/otot/_all_docs | grep id | grep -v _design | sed 's#{"id":"\([^"]*\).*rev":"\([^"]*\).*#curl -X DELETE http://127.0.0.1:5984/otot/\1?rev=\2#'`

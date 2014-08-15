var http = require('http'),
	request = require('request'),
	JSONStream = require('JSONStream');

var stream = JSONStream.parse(['results', true, 'id']) //rows, ANYTHING, doc

stream.on('data', function(data) {
  console.log(data);
});

stream.on('root', function(root, count) {
  if (!count) {
    console.log('no matches found:', root);
  }
});

//request({url: 'http://127.0.0.1:5984/otot/_all_docs'})
request({url: 'http://127.0.0.1:5984/otot/_changes?since=1039'})
  .pipe(stream);

var http = require('http'),
	request = require('request'),
	JSONStream = require('JSONStream');

var idSeen_l = {};

var stream = JSONStream.parse(['results', true, 'id']) //rows, ANYTHING, doc
stream.on('data', function(data) {
	var l = data.split("|");
	if (l.length == 3) {
		var id = l[0];
		if (!(id in idSeen_l)) {
			idSeen_l[id] = true;
			console.log("ID: "+id);

			var stream2 = JSONStream.parse(['rows', true, 'value', 'data']);
			stream2.on('data', function(data) {
				console.log("ITEM: "+JSON.stringify(data));
			});
			request({url: 'http://127.0.0.1:5984/otot/_design/main/_view/items?startkey=["'+id+'"]&endkey=["'+id+'",{}]'})
				.pipe(stream2);
		}
	}
});
stream.on('root', function(root, count) {
  if (!count) {
    console.log('no matches found:', root);
  }
});

request({url: 'http://127.0.0.1:5984/otot/_changes?since=1039'})
  .pipe(stream);

var http = require('http'),
	request = require('request'),
	JSONStream = require('JSONStream');

var idSeen_l = {};
var item_l = [];

function loadFromView(url) {
	var stream2 = JSONStream.parse(['rows', true, 'value', 'data']);
	stream2.on('data', function(data) {
		var id = data.id;
		data['_id'] = id;
		delete data['id'];
		console.log("ITEM: "+JSON.stringify(data));

		request.put(
			{
				url: 'http://127.0.0.1:5984/otot/'+id,
				headers: {
					'Content-Type': 'application/json'
				},
				json: data
			},
			function (e, r, body) {
				console.log("BODY: "+JSON.stringify(body));
			}
		);
	});
	request({url: url})
		.pipe(stream2);
}

function loadFromChanges(since) {
	var stream = JSONStream.parse(['results', true, 'id']) //rows, ANYTHING, doc
	stream.on('data', function(data) {
		var l = data.split("|");
		if (l.length == 3) {
			var id = l[0];
			if (!(id in idSeen_l)) {
				idSeen_l[id] = true;
				console.log("ID: "+id);
				loadFromView('http://127.0.0.1:5984/otot/_design/main/_view/patches?startkey=["'+id+'"]&endkey=["'+id+'",{}]');
			}
		}
	});
	stream.on('root', function(root, count) {
	  if (!count) {
		console.log('no matches found:', root);
	  }
	});
	/*stream.on('end', function() {
		console.log("ALL ITEMS:");
		for (var i = 0; i < item_l.length; i++) {
			console.log(JSON.stringify(item_l[i]));
		}
	});*/

	request({url: 'http://127.0.0.1:5984/otot/_changes?since='+since})
	  .pipe(stream);
}

loadFromView('http://127.0.0.1:5984/otot/_design/main/_view/patches?group_level=1');
//loadFromChanges(1099);

var http = require('http'),
	request = require('request'),
	JSONStream = require('JSONStream');

var idSeen_l = {};
var item_l = [];

function aggregate(keys, values) {
  //var n = values.length;
  var x = { rectype: "none", data: {} };
  var rev = 0;
  for (var i in values) {
    var value = values[i];
    if (value.rectype === "patch") {
      if (x['rectype'] === "snapshot") {
        for (var j in value.data.diffs) {
          var diff = value.data.diffs[j];
          if (diff[0] === "=") {
            var name = diff[1];
            x[name] = diff[2];
          }
        }
      }
      else if (x['rectype'] === "patch") {
        x['data'].diffs = x['data'].diffs.concat(value.data.diffs);
      }
      else if (x['rectype'] === "none") {
        x['rectype'] = "patch";
        x['data'] = value.data;
      }
      x.data.mtime = value.mtime;
    }
    else if (value.rectype === "snapshot") {
      x['rectype'] = "snapshot";
      x['data'] = value.data;
      x.data.mtime = value.mtime;
    }
    else if (value.rectype === "rev") {
      rev = value.rev;
    }
  }

  if (keys) {
    var mtime = "";
    for (var i = 0; i < keys.length; i++) {
      if (keys[i][0].length == 2) {
        var mtime2 = keys[i][0][1];
        if (mtime2 > mtime)
          mtime = mtime2;
      }
    }
    if (mtime) {
      x.data.mtime = mtime;
    }
  }

  if (rev) {
    x.data._rev = rev;
  }

  return x;
}

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

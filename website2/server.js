var crypto = require('crypto');
var express = require('express');
var fs = require('fs');
var http = require('http');
var jf = require('jsonfile');
var moment = require('moment');
var path = require('path');
var url = require('url');
var util = require('util');
var _ = require('underscore');

var port = process.argv[2];

var app = express();

function calcHash(str) {
    return crypto
        .createHash('md5')
        .update(str, 'utf8')
        .digest('hex')
}

// Server static files from the ui/ directory
app.use(express.static('ui'));

app.get('/items', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	var item_m = getItemMap();
	var item_l = _.toArray(item_m);
	if (urlData.query.hasOwnProperty('wrapper')) {
		data = {};
		data[urlData.query.wrapper] = item_l;
	}
	else {
		data = item_l;
	}
	response.write(JSON.stringify(data));
	response.end();
});

app.get('/items/:id', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	var item_m = getItemMap();
	var id = request.params.id;
	if (item_m.hasOwnProperty(id)) {
		var item = item_m[id];
		if (urlData.query.hasOwnProperty('wrapper')) {
			data = {};
			data[urlData.query.wrapper] = item;
		}
		else {
			data = item;
		}
		response.write(JSON.stringify(data));
	}
	response.end();
});

app.post('/items/:id/close', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	var id = request.params.id;
	var item_m = getItemMap();
	var date = moment().utc();
	var closed = date.format();
	if (item_m.hasOwnProperty(id)) {
		var item = item_m[id];
		var patch = {
			type: "patch1",
			version: 1,
			time: closed,
			user: "default",
			id: id,
			diffs: [["=", "closed", closed]]
		};
		var content = JSON.stringify(patch)
		var hash = calcHash(content);
		var filename = "./data/"+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
		fs.writeFileSync(filename, content);
	}
	response.end();
});

// patchN example
// {"time":"2014-08-03T15:11:21.086302Z","hunks":[{"diffs":["=status closed","=closed 2014-08-03T15:11:21.086302Z"],"uuids":["3258a7fa-e3b4-4d76-ac4a-c3bd18eb979d"]}],"user":"default","version":1,"type":"patch1"}


function getItemMap() {
	// Load all json files into a map to a list of 
	var filename_l = fs.readdirSync("./data/");
	// The files should be named in order of processing,
	// so sort the array so that we can directly update the item list
	var filename_l = _.filter(filename_l, function(filename) { return path.extname(filename) === ".json" });
	filename_l.sort();

	var item_m = {};
	_.each(filename_l, function(filename) {
		var contents = JSON.parse(fs.readFileSync("./data/"+filename, "utf8"));
		if (contents.type === "snapshot") {
			_.each(contents.items, function(item) {
				item_m[item.id] = item;
			});
		}
		// Patch with multiple ids, each receiving the same diffs
		else if (contents.type === "patch1") {
			var id = contents.id;
			_.each(contents.diffs, function(diff) {
				if (diff[0] === "=") {
					var name = diff[1];
					item_m[id][name] = diff[2];
				}
			});
		}
		else if (contents.type === "patchN") {
			_.each(contents.hunks, function(hunk) {
				_.each(hunk.ids, function(id) {
					_.each(hunk.diffs, function(diff) {
						if (diff[0] === "=") {
							var name = diff[1];
							item_m[id][name] = diff[2];
						}
					});
				});
			});
		}
	});

	return item_m;
}

var server = app.listen(port, function() {
	var host = server.address().address
	var port = server.address().port

	console.log('Example app listening at http://%s:%s', host, port)
});


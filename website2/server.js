var express = require('express');
var fs = require('fs');
var http = require('http');
var jf = require('jsonfile');
var path = require('path');
var url = require('url');
var util = require('util');
var _ = require('underscore');

var port = process.argv[2];

var app = express();

app.use(express.static('ui'));
app.get('/items', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	var item_l = getAllItems();
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

function getAllItems() {
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
			_.each(contents.ids, function(id) {
				_.each(contents.diffs, function(diff) {
					if (diff[0] === "=") {
						var name = diff[1];
						item_m[id][name] = diff[2];
					}
				});
			});
		}
	});

	return _.toArray(item_m);
}

var server = app.listen(port, function() {
	var host = server.address().address
	var port = server.address().port

	console.log('Example app listening at http://%s:%s', host, port)
});


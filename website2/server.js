var fs = require('fs');
var http = require('http');
var jf = require('jsonfile');
var path = require('path');
var url = require('url');
var util = require('util');

var port = process.argv[2];

var item_m = {};

var server = http.createServer(function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	//var date = new Date(urlData.query.iso);
	
	// Load all json files into a map to a list of 
	var filename_l = fs.readdirSync("./data/");
	// The files should be named in order of processing,
	// so sort the array so that we can directly update the item list
	filename_l.sort();
	for (var i in filename_l) {
		var filename = filename_l[i];
		if (path.extname(filename) === ".json") {
			var contents = JSON.parse(fs.readFileSync("./data/"+filename, "utf8"));
			if (contents.type === "snapshot") {
				for (var j in contents.items) {
					var item = contents.items[j];
					item_m[item.id] = item;
				}
			}
			// Patch with multiple ids, each receiving the same diffs
			else if (contents.type === "patch1") {
				for (var j in contents.ids) {
					var id = contents.ids[j];
					for (var k in contents.diffs) {
						var diff = contents.diffs[k];
						if (diff[0] === "=") {
							var name = diff[1];
							item_m[id][name] = diff[2];
						}
					}
				}
			}
		}
	}

	for (var i in item_m) {
		var item = item_m[i];
		response.write(item.id+": "+JSON.stringify(item)+"\n");
	}

	//response.write(filename+": "+JSON.stringify(file)+"\n");

	/*if (urlData.pathname == "/api/parsetime") {
		var data = { hour: date.getHours(), minute: date.getMinutes(), second: date.getSeconds() };
		response.write(JSON.stringify(data));
	}
	else if (urlData.pathname == "/api/unixtime") {
		var data = { unixtime: date.getTime() };
		response.write(JSON.stringify(data));
	}
	*/
	response.end();
});

server.listen(port);

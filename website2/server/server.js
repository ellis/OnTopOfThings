var bodyParser = require('body-parser');
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

var dataDir = "../../testdata/data02/";

var port = (process.argv[2]) ? process.argv[2] : 10000;

var app = express();

function calcHash(str) {
    return crypto
        .createHash('md5')
        .update(str, 'utf8')
        .digest('hex')
}

// Serve static files from the ui/ directory
//app.use(express.static('../ui'));
app.use(express.static('../react'));

// For parsing application/json body contents
app.use(bodyParser.json());

app.get('/items', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	var item_m = getItemMap();
	var item_l = _.toArray(item_m);
	// If there is an 'archived' filter:
	switch (urlData.query.archived) {
		// If we only want the archived items, filter out everything else
		case "only":
			item_l = item_l.filter(function(item) { return item.archived; });
			break;
		// Send both archived and non-archived items
		case "true":
			break;
		// Otherwise, by default, don't send archived items
		default:
			item_l = item_l.filter(function(item) { return !item.archived; });
	}
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

app.post('/items/:id', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	var id = request.params.id;
	var item_m = getItemMap();
	var time = moment().utc();
	if (item_m.hasOwnProperty(id) && request.body && !_.isEmpty(request.body.diffs)) {
		var patch = {
			type: "patch1",
			version: 1,
			time: time.format(),
			user: "default",
			id: id,
			diffs: request.body.diffs
		};
		var content = JSON.stringify(patch)
		var hash = calcHash(content);
		var filename = dataDir+time.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
		fs.writeFileSync(filename, content);
		applyPatch(item_m, patch);
		var item = item_m[id];
		response.write(JSON.stringify({result: "OK", item: item}));
	}
	response.end();
});

function generateUUID() {
	var d = new Date().getTime();
	var uuid = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
		var r = (d + Math.random()*16)%16 | 0;
		d = Math.floor(d/16);
		return (c=='x' ? r : (r&0x7|0x8)).toString(16);
	});
	return uuid;
}

app.put('/items', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	
	var date = moment().utc();

	var item = request.body;
	item.id = generateUUID();
	item.created = date.format();
	item.creator = "default";

	var patch = {
		type: "snapshot",
		version: 1,
		time: date.format(),
		user: "default",
		items: [item]
	};

	var content = JSON.stringify(patch)
	var hash = calcHash(content);
	var filename = dataDir+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
	fs.writeFileSync(filename, content);

	response.write(JSON.stringify({item: item}));
	response.end();
});

app.post('/archive', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	
	var item_m = getItemMap();
	var date = moment().utc();
	var timeString = date.format();

	var patch = {
		type: "patchN",
		version: 1,
		time: timeString,
		user: "default",
		hunks: [{ids: [], diffs: [["=", "archived", true]]}]
	};

	var ids = [];
	_.each(request.body.ids, function(id) {
		if (item_m.hasOwnProperty(id)) {
			ids.push(id.toString());
		}
	});
	patch.hunks[0].ids = ids;

	if (ids.length > 0) {
		var content = JSON.stringify(patch);
		var hash = calcHash(content);
		var filename = dataDir+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
		fs.writeFileSync(filename, content);
	}

	response.write(JSON.stringify({result: "OK", deleted: timeString}));
	response.end();
});


app.post('/close', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	
	var item_m = getItemMap();
	var date = moment().utc();
	var timeString = date.format();

	var patch = {
		type: "patchN",
		version: 1,
		time: timeString,
		user: "default",
		hunks: [{ids: [], diffs: [["=", "closed", timeString]]}]
	};

	var ids = [];
	_.each(request.body.ids, function(id) {
		if (item_m.hasOwnProperty(id)) {
			ids.push(id.toString());
		}
	});
	patch.hunks[0].ids = ids;
0
	if (ids.length > 0) {
		var content = JSON.stringify(patch);
		var hash = calcHash(content);
		var filename = dataDir+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
		fs.writeFileSync(filename, content);
	}

	response.write(JSON.stringify({result: "OK", closed: timeString}));
	response.end();
});

app.post('/delete', function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });
	
	var item_m = getItemMap();
	var date = moment().utc();
	var timeString = date.format();

	var patch = {
		type: "patchN",
		version: 1,
		time: timeString,
		user: "default",
		hunks: [{ids: [], diffs: [["=", "deleted", timeString]]}]
	};

	var ids = [];
	_.each(request.body.ids, function(id) {
		if (item_m.hasOwnProperty(id)) {
			ids.push(id.toString());
		}
	});
	patch.hunks[0].ids = ids;
0
	if (ids.length > 0) {
		var content = JSON.stringify(patch);
		var hash = calcHash(content);
		var filename = dataDir+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
		fs.writeFileSync(filename, content);
	}

	response.write(JSON.stringify({result: "OK", deleted: timeString}));
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
		if (!item.closed) {
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
			var filename = dataDir+date.format("YYYYMMDD_HHmmssSSS")+"-"+hash+".json";
			fs.writeFileSync(filename, content);
		}
		response.write(JSON.stringify({result: "OK"}));
	}
	response.end();
});

// patchN example
// {"time":"2014-08-03T15:11:21.086302Z","hunks":[{"diffs":["=status closed","=closed 2014-08-03T15:11:21.086302Z"],"uuids":["3258a7fa-e3b4-4d76-ac4a-c3bd18eb979d"]}],"user":"default","version":1,"type":"patchN"}


function getItemMap() {
	// Load all json files into a map to a list of 
	var filename_l = fs.readdirSync(dataDir);
	// The files should be named in order of processing,
	// so sort the array so that we can directly update the item list
	var filename_l = _.filter(filename_l, function(filename) { return path.extname(filename) === ".json" });
	filename_l.sort();

	var item_m = {};
	_.each(filename_l, function(filename) {
		var contents = JSON.parse(fs.readFileSync(dataDir+filename, "utf8"));
		applyPatch(item_m, contents);
	});

	return item_m;
}

function applyPatch(item_m, contents) {
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
}

var server = app.listen(port, function() {
	var host = server.address().address
	var port = server.address().port

	console.log('Example app listening at http://%s:%s', host, port)
});


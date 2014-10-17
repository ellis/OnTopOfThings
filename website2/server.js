var fs = require('fs');
var http = require('http');
var jf = require('jsonfile');
var path = require('path');
var url = require('url');
var util = require('util');

var port = process.argv[2];

var server = http.createServer(function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	//var date = new Date(urlData.query.iso);
	
	var filename_l = fs.readdirSync("./data/");
	for (var i in filename_l) {
		var filename = filename_l[i];
		if (path.extname(filename) === ".json") {
			response.write(filename);
		}
	}

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

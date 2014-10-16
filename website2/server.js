var http = require('http');
var url = require('url');

var port = process.argv[2];

var server = http.createServer(function(request, response) {
	response.writeHead(200, { 'content-type': 'application/json' });

	var urlData = url.parse(request.url, true);
	var date = new Date(urlData.query.iso);

	if (urlData.pathname == "/api/parsetime") {
		var data = { hour: date.getHours(), minute: date.getMinutes(), second: date.getSeconds() };
		response.write(JSON.stringify(data));
	}
	else if (urlData.pathname == "/api/unixtime") {
		var data = { unixtime: date.getTime() };
		response.write(JSON.stringify(data));
	}
	response.end();
});

server.listen(port);

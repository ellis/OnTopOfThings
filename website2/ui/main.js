$("#main").append("Hello");

$.ajaxSetup({beforeSend: function(xhr){
  if (xhr.overrideMimeType)
  {
    xhr.overrideMimeType("application/json");
  }
}
});

/*
function compare(a, b) {
	if (a.id < b.id)
		return -1;
	else if (a.id > b.id)
		return 1;
	else if (a.

  if (a.last_nom > b.last_nom)
    return 1;
  return 0;
}
*/

/*
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
*/

var horizon_l = ["new", "today", "next", "week", "month", "quarter", "year"];

function createComparor(criterion_l) {
	return function(a, b) {
		//console.log("criterion_l: "+criterion_l);
		for (var i = 0; i < criterion_l.length; i++) {
			var criterion = criterion_l[i];
			//console.log("criterion: "+criterion);
			if (criterion == "created") {
				var n = a.created.localeCompare(b.created);
				if (n != 0) {
					return n;
				}
			}
			else if (criterion == "folder") {
				if (!_.isEqual(a.folder, b.folder)) {
					return (a.folder < b.folder) ? -1 : 1;
				}
			}
			else if (criterion == "horizon") {
				var ia = horizon_l.indexOf(a.horizon);
				var ib = horizon_l.indexOf(b.horizon);
				//console.log(a.horizon+"="+ia+", "+b.horizon+"="+ib);
				if (ia != ib) {
					return (ia < ib) ? -1 : 1;
				}
			}
		}
		return 0;
	}
/*
	for (var i = criterion_l.length - 1; i >= 0; i--) {
		var criterion = criterion_l[i];
		if (criterion == "created") {
			fn = function(a, b) {
				var n = a.created.localeCompare(b.created);
				return (n != 0) ? n : fn(a, b);
			}
		}
		else if (criterion == "folder") {
			fn = function(a, b) {
				return (a.folder == b.folder) ? fn(a, b) : (a.folder < b.folder) ? -1 : 1;
			}
		}
		else if (criterion == "horizon") {
			fn = function(a, b) {
				var ia = horizon_l.indexOf(a.horizon);
				var ib = horizon_l.indexOf(b.horizon);
				return (ia == ib) ? fn(a, b) : (ia < ib) ? -1 : 1;
			}
		}
	}
	return fn;
*/
}

/*
function createFilter() {
	var fn0 = function(item) { return !item.closed; }
	//var fn = function(item) { return true; }
	
	var folder_l = $("#folders").val().split(",").filter(function(x) x);
	var tag_l = $("#tags").val().split(",").filter(function(x) x);

	var fn1 = fn0;
	if (folder_l.length > 0) {
		// TODO: filtering by folder doesn't work yet
		fn1 = function(item) {
			return fn0 && _.some(folder_l, function(f) {
				var f_l = f.split("/")
				return f_l == item.folder.slice(0, f_l.length);
			});
		}
	}

	var fn2 = fn1;
	if (tag_l.length > 0) {
		fn2 = function(item) {
			if (!item.tag)
				return false;
			//console.log("item.tag: "+item.tag);
			//console.log("tag_l: "+tag_l);
			for (var i = 0; i < tag_l.length; i++) {
				if (item.tag.indexOf(tag_l[i]) < 0)
					return false;
			}
			//console.log("...");
			return fn1(item);
		}
	}

	return fn2;
}
*/

function createFilterFromQuery(query) {
	var ast = (query) ? JSON.parse(query) : [];
	return function(item) {
		if (item.closed) return false;
		return filterFromAst(ast, item);
	}
}

function filterFromAst(ast, item) {
	for (var i in ast) {
		var elem = ast[i];
		var op = elem[0];

		if (op === "AND") {
		}
		else if (op === "OR") {
		}
		// Non-recursive folder query
		else if (op === "folder=") {
			var queryValue = elem[1];
			var itemValue = item.folder.join("/");
			if (!_.isEqual(itemValue, queryValue))
				return false;
		}
		// Folder query (item is in folder or one of it's children)
		else if (op === "folder") {
			var queryValue = elem[1];
			var itemValue = item.folder.join("/");
			//console.log("itemValue: "+itemValue+", "+_.isEqual(itemValue, queryValue)+", "+queryValue.startsWith(itemValue + "/"));
			if (!(_.isEqual(itemValue, queryValue) || itemValue.startsWith(queryValue + "/")))
				return false;
		}
		else {
			var queryName = elem[1];
			if (!item.hasOwnProperty(queryName)) return false;
			var itemValue = item[queryName];

			if (op === "=") {
				var queryValue = elem[2];
				if (!_.isEqual(itemValue, queryValue))
					return false;
			}
			// Contains
			else if (op === "->") {
				var value = elem[2];
				if (!(item.hasOwnProperty(queryName) && item[queryName].indexOf(value) >= 0))
					return false;
			}
		}
	}
	return true;
}

/*function filterItemIsOpen(item) {
	return !item.closed;
}

function filterItemIsOpenMustdo(item) {
	return !item.closed && item.tag && (item.tag.indexOf("mustdo") > -1);
}*/

function doList() {
	$("#list").empty();
	$.getJSON("/items?wrapper=items", function(snapshot) {
		var item_l = snapshot.items;

		// TODO: need to validate the header and order fields
		var header_l = $("#headers").val().split(",").filter(function(s) s);
		var criterion_l = header_l.concat($("#order").val().split(",")).filter(function(s) s);
		var comparor = createComparor(criterion_l);
		//var filter = createFilter();
		var filter = createFilterFromQuery($("#query").val());
		item_l = item_l.filter(filter);
		item_l.sort(comparor);
		var listElem = $("#list");
		var header0 = {};
		for (i in item_l) {
			var item = item_l[i];
			var n = parseInt(i) + 1;
			var tags = item.tag ? " (" + item.tag.join(",") + ")" : "";
			for (var j = 0; j < header_l.length; j++) {
				var fieldName = header_l[j];
				var fieldValue = item[fieldName];
				if (fieldName === "folder") {
					fieldValue = "/"+fieldValue.join("/");
				}
				//console.log("j = "+j+", fieldName = "+fieldName+", header[fieldName] = "+header[fieldName]+", header0[fieldName] = "+header0[fieldName]);
				if (!_.isEqual(fieldValue, header0[fieldName])) {
					listElem.append("<h"+(j+1)+" class='field-"+fieldName+"'>"+encodeURI(fieldValue)+"</h"+(j+1)+">");
					header0[fieldName] = fieldValue;
				}
			}

			var textHorizon = (header_l.indexOf("horizon") >= 0) ? "" : "<span class='field-horizon'>?" + item.horizon + "</span> ";
			var textFolder = (header_l.indexOf("folder") >= 0) ? "" : item.folder.join("/")+": ";

			listElem.append("<li>"+n+" "+textFolder+textHorizon+item.title+tags+"</li>");
		}
	});
}


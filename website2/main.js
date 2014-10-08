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

function createComparor(criterion_l) {
	var fn = function(a, b) { return 0; }

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
				var n = a.created.localeCompare(b.created);
				return (a.folder == b.folder) ? fn(a, b) : (a.folder < b.folder) ? -1 : 1;
			}
		}
	}
	return fn;
}

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

/*function filterItemIsOpen(item) {
	return !item.closed;
}

function filterItemIsOpenMustdo(item) {
	return !item.closed && item.tag && (item.tag.indexOf("mustdo") > -1);
}*/

function doList() {
	$("#list").empty();
	$.getJSON("snapshot--20140813.json", function(snapshot) {
		var item_l = snapshot.items;
		var criterion_l = $("#order").val().split(",");
		var comparor = createComparor(criterion_l);
		var filter = createFilter();
		item_l = item_l.filter(filter);
		item_l.sort(comparor);
		var listElem = $("#list");
		for (i in item_l) {
			var item = item_l[i];
			var n = parseInt(i) + 1;
			var tags = item.tag ? " (" + item.tag.join(",") + ")" : "";
			listElem.append("<li>"+n+" "+item.folder.join("/")+": "+item.title+tags+"</li>");
		}
	});
}


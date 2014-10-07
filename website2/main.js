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

function compareItemsByCreate(a, b) {
	return a.created.localeCompare(b.created);
}

function filterItemIsOpen(item) {
	return !item.closed;
}

function filterItemIsOpenMustdo(item) {
	return !item.closed && item.tag && (item.tag.indexOf("mustdo") > -1);
}

$.getJSON("snapshot--20140813.json", function(snapshot) {
	var item_l = snapshot.items;
	item_l = item_l.filter(filterItemIsOpenMustdo);
	item_l.sort(compareItemsByCreate);
	var listElem = $("#list");
	for (i in item_l) {
		var item = item_l[i];
		var n = parseInt(i) + 1;
		var tags = item.tag ? " (" + item.tag.join(",") + ")" : "";
		listElem.append("<li>"+n+" "+item.folder.join("/")+": "+item.title+tags+"</li>");
	}
});

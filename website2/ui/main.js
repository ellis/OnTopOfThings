$.ajaxSetup({beforeSend: function(xhr){
  if (xhr.overrideMimeType)
  {
    xhr.overrideMimeType("application/json");
  }
}
});

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
}

function createFilterFromQuery(query) {
	var ast = (query) ? JSON.parse(query) : [];
	return function(item) {
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

var item_m = {};
var header_l = [];

function getItemInnerHtml(item, index) {
	var textHorizon = (header_l.indexOf("horizon") >= 0) ? "" : "<span class='field-horizon'>?" + item.horizon + "</span> ";
	var textFolder = (header_l.indexOf("folder") >= 0) ? "" : item.folder.join("/")+": ";
	var checkbox = (item.closed) ? "<input type='checkbox' class='checkbox-closed' checked> " : "<input type='checkbox' class='checkbox-closed'> "
	var tags = item.tag ? " (" + item.tag.join(",") + ")" : "";
	var text = index+" "+checkbox+textFolder+textHorizon+item.title+tags;
	if (item.deleted) {
		text = "<span style='text-decoration: line-through; color: #808080'>"+text+"</span>";
	}
	return text;
}

function updateItemAtIndex(index) {
	if (item_m.hasOwnProperty(index)) {
		var item = item_m[index];
		var text = getItemInnerHtml(item, index);
		var li = $("#item"+item.id);
		li.empty();
		li.append(text);
	}
}

function doList() {
	$("#list").empty();
	var archived = "";
	if ($("#rdoArchived").prop("checked"))
		archived = "&archived=true";
	else if ($("#rdoArchivedOnly").prop("checked"))
		archived = "&archived=only";
	$.getJSON("/items?wrapper=items"+archived, function(snapshot) {
		var item_l = snapshot.items;

		// TODO: need to validate the header and order fields
		header_l = $("#headers").val().split(",").filter(function(s) { return s; });
		var criterion_l = header_l.concat($("#order").val().split(",")).filter(function(s) { return s; });
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
			var text = getItemInnerHtml(item, n);
			listElem.append("<li id='item"+item.id+"'>"+text+"</li>");
			item_m[n] = item;
		}
	});
}

function doCreateNew() {
	var data = {
		type: "task"
	};
	// Title
	if ($("#newItemTitle").val()) {
		data.title = $("#newItemTitle").val();
	}
	// Folder
	if ($("#newItemFolder").val()) {
		data.folder = $("#newItemFolder").val().split("/").filter(function(s) { return s; });
	}
	// Horizon
	if ($("#newItemHorizon").val()) {
		data.horizon = $("#newItemHorizon").val();
	}
	// Tags
	if ($("#newItemTags").val()) {
		data.tag = $("#newItemTags").val().split(",").filter(function(s) { return s; });
	}
	
	$.ajax({
		type: "PUT",
		url: "/items",
		data: JSON.stringify(data),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			doList();
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}

function doCloseN() {
	var l = $("#closeList").val().split(" ");
	var indexes = _.map(l, function(s) { return parseInt(s); })
	var ids = _.map(indexes, function(index) {
		return (item_m.hasOwnProperty(index)) ? item_m[index].id : null;
	}).filter(function(id) { return id; })

	$("#closeList").val("");
	
	$.ajax({
		type: "POST",
		url: "/close",
		data: JSON.stringify({ ids: ids }),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			// Iterate through the ids we closed, and make sure the closed checkbox is ticked.
			_.each(indexes, function(index) {
				item_m[index].closed = data.closed;
				updateItemAtIndex(index);
			});
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}

function doDeleteN() {
	var l = $("#deleteList").val().split(" ");
	var indexes = _.map(l, function(s) { return parseInt(s); })
	var ids = _.map(indexes, function(index) {
		return (item_m.hasOwnProperty(index)) ? item_m[index].id : null;
	}).filter(function(id) { return id; })

	$("#deleteList").val("");
	
	$.ajax({
		type: "POST",
		url: "/delete",
		data: JSON.stringify({ ids: ids }),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			// Iterate through the ids we deleted, and make sure the item's font is crossed through.
			_.each(indexes, function(index) {
				item_m[index].deleted = data.deleted;
				updateItemAtIndex(index);
			});
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}

function indexToId(index) {
	return (item_m.hasOwnProperty(index)) ? item_m[index].id : null;
}

function doEdit() {
	var index = parseInt($("#editItemIndex").val());
	var id = indexToId(index);

	var diffs = [];
	// Title
	if (editItem.title !== $("#editItemTitle").val()) {
		diffs.push(["=", "title", $("#editItemTitle").val()]);
	}
	// Folder
	var folder = $("#editItemFolder").val().split("/").filter(function(s) { return s; });
	if (!_.isEqual(editItem.folder, folder)) {
		var folder = $("#editItemFolder").val().split("/").filter(function(s) { return s; });
		diffs.push(["=", "folder", folder]);
	}
	// Horizon
	if (editItem.horizon !== $("#editItemHorizon").val()) {
		diffs.push(["=", "horizon", $("#editItemHorizon").val()]);
	}
	// Tags
	var tags = $("#editItemTags").val().split(",").filter(function(s) { return s; });
	if (!_.isEqual(editItem.tag, tags) && (!_.isEmpty(editItem.tag) || !_.isEmpty(tags))) {
		diffs.push(["=", "tag", tags]);
	}
	
	var data = { diffs: diffs };
	$.ajax({
		type: "POST",
		url: "/items/"+id,
		data: JSON.stringify(data),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			alert(JSON.stringify(data));
			if (data.result === "OK" && data.item) {
				item_m[index] = data.item;
				updateItemAtIndex(index);
			}
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}

function doArchive() {
	// REFACTOR: replace this with underscore call to filter on item_m values
	var ids = [];
	for (var key in item_m) {
		var item = item_m[key];
		if (item.closed || item.deleted)
			ids.push(item.id);
	}

	$.ajax({
		type: "POST",
		url: "/archive",
		data: JSON.stringify({ ids: ids }),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			doList();
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}

function doListSchedule() {
	$("#list").empty();
	var archived = "";
	if ($("#rdoArchived").prop("checked"))
		archived = "&archived=true";
	else if ($("#rdoArchivedOnly").prop("checked"))
		archived = "&archived=only";
	$.getJSON("/items?wrapper=items"+archived, function(snapshot) {
		var item_l = snapshot.items;

		// Populate map from id to item
		var idToItem_m = {};
		_.each(item_l, function(item) { idToItem_m[item.id] = item; });

		schedule_l = item_l.filter(function(item) { return item.type == "schedule" });
		var listElem = $("#list");
		var parentIdToChildren_m = {};
		_.each(schedule_l, function(schedule) {
			listElem.append("<h2>"+schedule.date+"</h2>");
			listElem.append(JSON.stringify(schedule, '\t'));
			_(schedule.nodes).each(function(node) {
				if (!parentIdToChildren_m.hasOwnProperty(node.parentId))
					parentIdToChildren_m[node.parentId] = [];
				parentIdToChildren_m[node.parentId].push(node);
			});

			listElem.append("<pre>"+JSON.stringify(parentIdToChildren_m, '  ')+"</pre>");
			handleScheduleItem(parentIdToChildren_m, parentIdToChildren_m[0], listElem, 0);
		});
	});
}

function handleScheduleItem(parentIdToChildren_m, node_l, elem, indent) {
	_(node_l).sortBy(function(node) { return node.index });
	_(node_l).each(function(node) {
		var text;
		switch (node.type) {
			case "section":
				text = node.text;
				break;
			case "reference":
				text = node.refId;
				break;
		}
		elem.append("<div style='margin-left: "+(indent*2)+"em'>"+text+"</div>");
		if (parentIdToChildren_m.hasOwnProperty(node.id)) {
			var child_l = parentIdToChildren_m[node.id];
			handleScheduleItem(parentIdToChildren_m, child_l, elem, indent+1);
		}
	});
}

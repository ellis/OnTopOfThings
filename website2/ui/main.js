$("#main").append("Hello");

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
		if (item.archived) return false;
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

function doList() {
	$("#list").empty();
	$.getJSON("/items?wrapper=items", function(snapshot) {
		var item_l = snapshot.items;

		// TODO: need to validate the header and order fields
		var header_l = $("#headers").val().split(",").filter(function(s) { return s; });
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
			var checkbox = (item.closed) ? "<input type='checkbox' class='checkbox-closed' checked> " : "<input type='checkbox' class='checkbox-closed'> "

			listElem.append("<li id='item"+item.id+"'>"+n+" "+checkbox+textFolder+textHorizon+item.title+tags+"</li>");
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
		data.folder = $("#newItemFolder").val().split("/").filter(function(s) s);
	}
	// Horizon
	if ($("#newItemHorizon").val()) {
		data.horizon = $("#newItemHorizon").val();
	}
	// Tags
	if ($("#newItemTags").val()) {
		data["tags"] = $("#newItemTags").val().split(",").filter(function(s) s);
	}
	
	$.ajax({
		type: "PUT",
		url: "/items",
		data: JSON.stringify(data),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			alert(data);
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
			_.each(ids, function(id) {
				$("#item"+id).find('.checkbox-closed').prop("checked", true);
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
	var id = indexToItem(index);

	var data = {
		type: "task"
	};
	// Title
	if ($("#editItemTitle").val()) {
		data.title = $("#editItemTitle").val();
	}
	// Folder
	if ($("#editItemFolder").val()) {
		data.folder = $("#editItemFolder").val().split("/").filter(function(s) s);
	}
	// Horizon
	if ($("#editItemHorizon").val()) {
		data.horizon = $("#editItemHorizon").val();
	}
	// Tags
	if ($("#editItemTags").val()) {
		data["tags"] = $("#editItemTags").val().split(",").filter(function(s) s);
	}
	
	$.ajax({
		type: "POST",
		url: "/items/"+id,
		data: JSON.stringify(data),
		contentType: "application/json; charset=utf-8",
		dataType: "json",
		success: function(data) {
			alert(data);
		},
		failure: function(errMsg) {
			alert(errMsg);
		}
	});
}


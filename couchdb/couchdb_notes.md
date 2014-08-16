# Todo

- [x] Use ``curl -X GET http://127.0.0.1:5984/otot/_changes\?feed\=continuous\&since\=978\&style\=all_docs`` to get a list of recent patches
- [x] the ID for a patch should be: itemId|utc|randomNumber
- [x] Parse the patch id and request a list of the item IDs from the appropriate view
- [x] client.js: generates update request for creating documents with the itemIds. (The view will need to include revision number if a document already exists)
- [x] _view/items: need to output the rev if present
- [x] Create a script to run all of the above
- [ ] couchdb: Rename 'item' view to 'patch' view
- [ ] couchdb: Create views based on item documents
- [ ] website: adapt for new views
- [ ] website: view by horizon
- [ ] website: view of projects
- [ ] website: view by project
- [ ] website: let user mark items as done
- [ ] website: let user mark change item's horizon
- [ ] client.js: move aggregation of snapshots and patches from couchdb view to client.js
- [ ] modularize client.js to create a script that updates everything and one that updates just based on _changes
- [ ] Create a program that subscribes to the changelog and updates the item documents when patches come in

# Notes

Want to figure out way to handle Schedule.wiki-like interaction
Maybe have a list, and separate list items so that if different clients update items, they can be more easily merged
How to mark an item as done?
How to mark time spent on an item?
How to assign a projected start time to an item?
How to allocate pomodoros to an item?

A list for each day

Difficult features:
- block together shorter tasks for a single pomodoro
- recurring todos

Schedule item types:
- todo items
- activity/goal items
- simple text items

Schedule list item:
    user
    date
    context
    parentId (an ID, allows for sublists)
    index (index in list/sublist)
    hour
    minute
    text (optional text)
    itemId (an optional ID to another item)
    pomodoros (list of estimates, usually only one)
    pomodoroStatus (list of completed, interrupted, whatever)
    timeStart
    timeStop

        22 Sunday
        - [x] 06:00 planning [x]
        - [x] 06:25 edu: research
          - [x] load peoples summaries of logic
          - [x] edu: research: copy my logic notes into an text file
        - [x] 07:15 edu: android: evaluate [x]
        @Train
        - [x] edu: algo: programming 6 due [x] [x]
        - [ ] OnTopOfThings [ ]

    {"id": "01", "mtime": 1, "type": "activity", "user": "ellis", "folder": ["eth"], "name": "planning", "status": "open"} 
    {"id": "02", "mtime": 1, "type": "project", "user": "ellis", "folder": ["eth", "lrn"], "name": "research", "status": "open"} 
    {"id": "03", "mtime": 1, "type": "task", "user": "ellis", "folder": ["eth", "lrn", "research"], "title": "load peoples summaries of logic", "status": "closed"}
    {"id": "04", "mtime": 1, "type": "task", "user": "ellis", "folder": ["eth", "lrn", "research"], "title": "copy my logic notes into an text file", "status": "closed"}
    {"id": "05", "mtime": 1, "type": "project", "user": "ellis", "folder": ["per", "lrn"], "name": "android", "status": "open"} 
    {"id": "06", "mtime": 1, "type": "task", "user": "ellis", "folder": ["per", "lrn", "android"], "title": "evaluate week 4", "status": "closed"}
    {"id": "07", "mtime": 1, "type": "project", "user": "ellis", "folder": ["eth", "lrn"], "name": "algo", "status": "open"} 
    {"id": "08", "mtime": 1, "type": "task", "user": "ellis", "folder": ["eth", "lrn", "algo"], "title": "programming 6 due", "status": "closed"}
    {"id": "09", "mtime": 1, "type": "project", "user": "ellis", "folder": ["per"], "name": "otot", "status": "open"} 

    {"id": "10", "mtime": 1, "type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 1, "time": [6, 0], "itemId": "01", "pomodoros": [1], "pomodoroStatus": ["x"]}
    {"id": "11", "mtime": 1, "type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 2, "time": [6, 25], "itemId": "02", "pomodoros": [1], "pomodoroStatus": ["x"]}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "parentId": "11", "index": 1, "itemId": "03"}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "parentId": "11", "index": 2, "itemId": "04"}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 3, "time": [7, 15], "itemId": "06", "pomodoros": [1], "pomodoroStatus": ["x"]}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 4, "text": "@Train"}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 5, "itemId": "08", "pomodoros": [2], "pomodoroStatus": ["x", "x"]}
    {"type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 6, "itemId": "09", "pomodoros": [1]}


Figures I haven't thought about yet:
- storing large binary files
- git versioned files
- contacts

    "diffs": [
      ["=", "horizon", "week"]
      ["+", "tag", "mustdo"]
    ]

# Snapshots

    {
      "type": "snapshot",
      "ctime": "XXXTZ",
      "items:" [
        ...
      ]
    }

# Views

```javascript
function(doc) {
  if (doc.type === "patch") {
    emit([doc.id, doc.mtime], {rectype: "patch", data: doc});
  }
  else if (doc.type === "snapshot") {
    for (var j in doc.items) {
      var item = doc.items[j];
      emit([item.id, doc.ctime], {rectype: "snapshot", data: item});
    }
  }
}
function(keys, values) {
  var x = { rectype: "none", data: {} };
  for (var i in values) {
    var value = values[i];
    if (value.rectype === "patch") {
      if (x.rectype === "snapshot") {
        for (var j in value.data.diffs) {
          var diff = value.data.diffs[j];
          if (diff[0] === "=") {
            var name = diff[1];
            x.data[name] = diff[2];
          }
        }
      }
      else if (x.rectype === "patch") {
        x.data.diffs = x.data.diffs.concat(value.data.diffs);
      }
      else if (x['rectype'] === "none") {
        x.rectype = "patch";
        x.data = value.data;
      }
    }
    else if (value.rectype === "snapshot") {
      x.rectype = "snapshot";
      x.data = value.data;
    }
  }
  return x;
}
```

# Queries

    curl http://127.0.0.1:5984/otot/_design/main/_view/schedule
    curl http://127.0.0.1:5984/otot/_design/main/_view/items\?group_level\=1\&limit\=10
    curl -X PUT http://127.0.0.1:5984/otot/snapshot--20140813 --data-binary @snapshot--20140813.json
    curl http://127.0.0.1:5984/otot/_design/main/_list/openTasks/items\?group_level\=1\&limit\=10


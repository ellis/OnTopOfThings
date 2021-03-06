# Todos

- [ ] create index objects that somehow track the order of items in a given context (e.g. project)
- [ ] make it easy to see tasks that have been completed
- [ ] read article about todo app using ReactJS and Flux: https://www.codementor.io/reactjs/tutorial/react-js-flux-architecture-tutorial
- [ ] let user choose order of item display elements (via the 'S' flag for view config)
- [ ] add an 'o' view flag, that uses the element for ordering, but doesn't display it automatically
- [ ] figure out view specification for how to sort on 'created', but either not show it at all, or show it at the end
- [ ] TaskView: for each item, drop-down checkbox/menu for selecting open/done/deleted/archive
- [ ] TaskView: add selection checkbox for selecting multiple items
- [ ] consider a json query language:
    - https://github.com/deitch/searchjs
    - http://www.jsoniq.org/
    - http://objectpath.org/
- [ ] App: add menu for operating on selected items (close, delete, archive, maybe dialog box for changing properties, like moving all items to a specific folder)
- [ ] create dialog box for detailed view settings?
- [ ] put search field into the header again, next to the field for new tasks
- [ ] when user starts typing in a new task, expand the view to show more information, like tags, horizon, etc
- [ ] let user set `order` of task in its parent?  Or store ordering constraints in parent?  Or create separate item that stores ordering constraints per parent/context?
- [ ] manage task hierarchy, where tasks can point to parent tasks
    - [ ] work on idToChilden in updateItems() around line 609
- [ ] TaskListSettings: make Headers a drop-down checkbox list (see http://davidstutz.github.io/bootstrap-multiselect/)
    - make sortable: http://stackoverflow.com/questions/20424477/how-can-i-implement-a-touch-sensitive-responsive-sortable-list-supporting-drag
    - make sortable: http://jqueryui.com/sortable/
- [ ] start working on Schedule interface
- [ ] App: search: automatically OR together values for field when multiple were specificed for folders, horizions, tags (e.g. "/per /fam" should show both /per and /fam folders)
- [ ] App: search: OR together title words
- [ ] App: search: OR together folders, horizons, title words, tags
- [ ] App: search: figure out way to AND the tags
- [ ] App: search: let user put quotations around title words to specify sequence
- [ ] TaskListSettings: select whether showArchived = yes|no|both
- [ ] TaskListSettings: make Order a sortable listbox
- [ ] allow for inline editing of task elements (i.e., double-click on title opens a title input box)
- [ ] start creating console interface?
- [ ] check out https://github.com/tastejs/todomvc/tree/gh-pages/examples/react
- [ ] figure out why Escape doesn't close the dialog
- [ ] when closed checkbox is clicked, request to close item
- [ ] let user uncheck closed button on an item
- [ ] in close dialog, parse input to allow for number ranges such as "1-5"
- [ ] update displayed item after edit, but how to handle changes that impact the headers and filter?
- [ ] TaskEditor: folder: make it auto-complete
- [ ] TaskEditor: more compact styling
- [ ] TaskListSettings: why doesn't search="", headers="", order="created" work?
- [ ] TaskListSettings: would like to list the tasks in order of newest to oldest, and from oldest to newest
- [ ] rename directory `website2` to `website`
- [ ] rename directory `website2/react` to `website/ui`
- [ ] create charts of number of tasks in various projects/tags (also over time)
- [ ] create charts of amount of time spent on various projects/tags (also over time)
- [ ] create a convenient way to add notes to tasks and folders; multiple notes or all in one file?  If all in one file, there would likely be merging challenges.
- [ ] rename directory `website/react` to `website/ui`
- [ ] Add field for reason for closing an item -- this is particularly desired for when deleting the item; look at how github does it
- [ ] Move the "Views" list to an external data file, so that it's not hard-coded to my personal view list

# Indexes

Ideas:

- create folder items that have a list of task UUIDs; this might be useful as a starting point for other kinds of lists?  It may be challenging to merge re-orderings from multiple computers.
- add an "index" field to tasks; this is the simplest solution, but then we need to change multiple tasks whenever one of the indexes is reordered.

Let's think about the diff for ordering tasks.
Consider using a list:
```
indexedItems: [A, B, C]

set.remove uuid
set.append uuid
set.insert index uuid
set.move uuid index

when items are archive, they need to also be removed from the indexedItems list
```

Consider using a map:
```
indexedItems:
  A: 1
  B: 2

map.append key value
map.remove key

to change order, submit multiple fields on the map.
should be easy to merge, but still have difficulty of duplicate index values.
when items are archive, they need to also be removed from the indexedItems map
```

# Daily schedule setup:

Consists of a tree of "entries".
Each entry is either a section name or reference to an item.
Each entry has a parent entry and a sub-index within that entry.
Each entry can optionally have a time associated with it.
Each entry can optionally have pomodoros associated with it.
Each pomodoro can optionally have a time associated with it, as well as some other flags.

How can this be updated and synchronized between multiple computers?

schedule: {
  nodes: {
    "91823450": {type: "section", text: "Home", parentId: 0, index: 0},
    "01592384": {type: "reference", refId: "sdfl2j30asl", parentId: 91823450, index: 0, time: ?, pomodoros: [{status: ""}, {}]}
  }
}

[".", "nodes", "key", "91823450", "=", "text", "@Home"]
[".", "nodes", "put", "91823450", {type: ..}]

# Urls

/schedule
/tasks
/tasks/list/:listname
/tasks/list/:root/:headers/:order/:query

# Completed Todos

- [x] save edits to server
- [x] create new item
- [x] try using 'director' for selecting page, ordering, and filters
- [x] improve TaskListSettings so that it updates based on the current route
- [x] turn itemsCached into a map from id to item
- [x] close items
- [x] TaskEditor: expand edit capabilities to include closed, deleted, archive
- [x] TaskEditor: fix enabled status on archived textbox, so that it gets enabled when user selected 'closed'
- [x] TaskEditor: handle 'closed' and 'deleted' fields on item send back up to App
- [x] TaskEditor: handle status and archived
- [x] BUG: why isn't item removed from list after archiving?
- [x] menu item to archive closed and deleted items
- [x] search: also search when user presses ENTER on an empty search box (to clear the search criteria)
- [x] App: search: search on title words
- [x] from URL parameters, fill in search field
- [x] URL-encode the search field
- [x] from URL parameters, fill in header and order fields
- [x] TaskListSettings: consider merging Search and Filter field, where '[]' is used to indicate advanced filter instead of simple search
- [x] BUG: set "search=/ppl", edit task title: why does horizon get reset to inbox?
- [x] server: change point so it's no longer 8080
- [x] BUG: enter "+mustdo" in search, press ENTER: gets changed to "%2Bmustdo" (but Ctrl-R fixes it again)
- [x] let user list tasks by age (without header)
- [x] try using query-string.js for query parsing
- [x] change view settings to accept query as follows:
    v=folder,H;horizon,O;created,O;tag,S
    where H=header, O=order, S=show, s=hide
- [x] BUG: during item listing, when a header changes, reset the lower headers too
- [x] display creation date
- [x] rename directory `website2` to `website`

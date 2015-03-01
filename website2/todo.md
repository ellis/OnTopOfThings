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
- [ ] let user choose to display creation date
- [ ] don't create new task unless there are task words
- [ ] manage task hierarchy, where tasks can point to parent tasks
    - [ ] work on idToChilden in updateItems() around line 609
- [ ] TaskListSettings: make Headers a drop-down checkbox list (see http://davidstutz.github.io/bootstrap-multiselect/)
    - make sortable: http://stackoverflow.com/questions/20424477/how-can-i-implement-a-touch-sensitive-responsive-sortable-list-supporting-drag
    - make sortable: http://jqueryui.com/sortable/
- [ ] start working on Schedule interface
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

- [x] use 'sed' to make all closed items archived
- [x] don't show archived items by default
- [x] visual indication of closed
- [x] move data directory to testdata/ folder, and re-sed to make closed items archived
- [x] figure out how to have server accept list of item ids to close
- [x] figure out how to have client send list of item ids to close
- [x] update item on webpage when closed
- [x] figure out how to access checkbox without giving it its own id
- [x] for LI item id field, use item's id instead of index
- [x] create close form/dialog
- [x] create dialog for new item
- [x] create dialog for editing an item
- [x] server: API for receiving item modifications
- [x] ui: find what has changed when user edits and item, and send those changes to the server
- [x] add checkboxes to top of page for filtering on open/archived status
- [x] display items by open/archived status
- [x] ui: dialog for deleting items
- [x] ui: doDelete()
- [x] server: API for /delete
- [x] update displayed item after delete
- [x] setup with bower
- [x] BUG: Why does 'mustdo' tag not appear for item "?next register for exams in Zurich (pay 50CHF)"?
- [x] get bower to work on mac
- [x] ui: create button to archive closed and deleted items
- [ ] start working on Schedule interface
- [ ] start creating console interface?
- [ ] check out https://github.com/tastejs/todomvc/tree/gh-pages/examples/react
- [ ] get css and other resources locally so that website works in train
- [ ] style the dialog box better, especially the size
- [ ] expand edit capabilities to include closed, deleted, archive
- [ ] figure out why Escape doesn't close the dialog
- [ ] when closed checkbox is clicked, request to close item
- [ ] let user uncheck closed button on an item
- [ ] in close dialog, parse input to allow for number ranges such as "1-5"
- [ ] update displayed item after edit, but how to handle changes that impact the headers and filter?

Daily schedule setup:

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


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
- [ ] style the dialog box better, especially the size
- [ ] figure out why Escape doesn't close the dialog
- [ ] add checkboxes to top of page for filtering on open/closed/archived status
- [ ] get css and other resources locally so that website works in train
- [ ] setup with bower
- [ ] display items by open/closed/archived status
- [ ] when closed checkbox is clicked, request to close item
- [ ] let user uncheck closed button on an item
- [ ] in close dialog, parse input to allow for number ranges such as 1-5
- [ ] after adding an new item, insert it into current display?
- [ ] get bower to work on mac
- [ ] update displayed item after edit, but how to handle changes that impact the headers and filter?

Daily schedule setup:

Consists of a tree of "entries".
Each entry is either a section name or reference to an item.
Each entry has a parent entry and a sub-index within that entry.
Each entry can optionally have a time associated with it.
Each entry can optionally have pomodoros associated with it.
Each pomodoro can optionally have a time associated with it, as well as some other flags.

How can this be updated and synchronized between multiple computers?

Day

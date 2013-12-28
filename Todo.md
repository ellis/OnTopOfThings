- [x] Command: instead of storing data changes, store commands
- [x] Database: load command records into database
- [x] Add: update database based on a Command
- [ ] Add: create CommandRecord from CLI parameters
- [ ] Add: append new CommandRecord to database
- [ ] Add: save CommandRecord to disk
- [ ] DatabaseTables: Switch to using UTCTime
- [ ] Main: list tasks
- [ ] Main: list tasks which have a field matching a string
- [ ] Main: list tasks with specific fields, i.e. --stage=inbox
- [ ] Main: find parent by title
- [ ] Main: when setting parent, prevent cyclic loops -- might want to check this in loadDB too.

- [ ] Change license to GPL3
- [ ] Put git repository on github

## Old
- [x] Change: switch to new JSON format
- [x] Change: Give clearer names to the Change* types
- [x] Create new change record files
- [x] Change: loadDB: load multiple files
- [x] Change: loadDB: load all json files in direcory
- [x] Change: Switch to using UTCTime in Change.hs
- [x] Main: Get current time for change records
- [x] Change: Convert Change* types to JSON
- [x] Main: Generate UUID for new change records
- [x] Change: remove unnecessary elements from JSON output
- [x] Main: save change record to disk

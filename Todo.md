# Todos

- [x] Command: instead of storing data changes, store commands
- [x] Database: load command records into database
- [x] Add: update database based on a Command
- [x] Add: create CommandRecord from CLI parameters
- [x] Add: append new CommandRecord to database
- [x] Main: save CommandRecord to disk
- [x] Main: list tasks
- [x] List: prefix title with parent list name
- [x] Add: store creation time for item
- [x] Database: index the tasks which are pending
- [x] Main: 'mod' command
- [x] Mod: modify item properties
- [x] List: check off task if it's completed
- [x] List: use esqueleto
- [x] Database: create an item table for wide-format data
- [x] List: list of lists and their components
- [x] Database: only set 'index' property on 'Item'
- [x] Add: only use Property table for list properties and non-standard properties
- [x] Add: find parent by uuid, index, label
- [x] Change license to GPL3
- [x] Put git repository on github
- [x] Mod: move function to Add.hs and refactor them
- [x] Mod: only use Property table for list properties and non-standard properties
- [x] Import: import from task warrior json and save a single file containing all the commands
- [x] Database: add fields for start time, end time, close time, due time
- [x] Import: handle close time
- [x] List: parameter 'from=TIME' hides any items which were closed before the given TIME
- [x] Set type and title in optsProcess2_add
- [ ] Rename Args.hs to Modes.hs
- [ ] Use CmdArgs for 'add', 'close', 'mod'
- [ ] Simplify creation of CommandRecord so that only index-references are turned into uuids, and use same format as on the command line
- [ ] Main: 'close' command
- [ ] Main: 'stage' command
- [ ] List: display by stage
- [ ] Main: use the 'format' parameter of CommandRecord to choose which function handles the command record
- [ ] Index all displayed items instead of open tasks?
- [ ] Add: maybe refToUuid shouldn't be called while constructing the CommandRecord in order to leave labels as they are
- [ ] Add: autogenerate a label for lists if none is given
- [ ] Utils: create a Validation monad and monoid
- [ ] Add: think about putting list paths into 'Property' tables for finding lists by path and detecting duplicates
- [ ] List: get parent path by using 'Item' instead of 'Property' table, unless path is stored into Property
- [ ] Database: add fields for priority
- [ ] List: prefix with start time
- [ ] List: prefix with end time
- [ ] List: list tasks for a given list
- [ ] List: show tags
- [ ] List: show contexts
- [ ] List: show age
- [ ] List: show comments
- [ ] Add: find parent by title too (case insensitive)
- [ ] Main: list tasks which have a field matching a string
- [ ] Main: list tasks with specific fields, i.e. --stage=inbox
- [ ] DatabaseTables: Switch to using UTCTime
- [ ] Main: when setting parent, prevent cyclic loops -- might want to check this in loadDB too.
- [ ] Lookup tradeoffs for String, Text, ByteString, as well as the lazy and strict versions.
- [ ] Command: save UTCTime with more precision?

When creating a new item from the commandline:

- [x] Options read by CmdArgs
- [x] Options are validated and processed
- [x] Options convert to CommandRecord
- [ ] CommandRecord saved to a temporary file
- [ ] CommandRecord read in from file
- [ ] Verify that CommandRecords are equal
- [x] CommandRecord converted to Command
- [x] Command saved to DB
- [ ] Command loaded from DB
- [ ] Verify that Commands are equal
- [ ] Command converted to a CommandRecord
- [ ] Verify that CommandRecords are equal
- [ ] CommandRecord converted to Options
- [ ] Verify that Options are equal
- [x] Options are validated and processed for modification of DB 'item' and 'property' tables
- [ ] items and properties are updated
- [ ] if anything went wrong, remove the Command entry and rebuild
- [ ] if everything went well, move the CommandRecord file to the appropriate path

Important lists:
- [ ] everything
- [ ] just tasks for a given list/tag/context
- [ ] all items for a given list/tag/context
- [ ] pomodoro day report
- [ ] kanban report
- [ ] calendar report
- [ ] goals/beeminder report
- [ ] urgent/imporant report

## Incubator tasks

- [ ] 'command undo' command deleting the last local command from the history
- [ ] 'command history' command to list all commands given so far (perhaps filtering out old closed items)
- [ ] 'command ignore' command for ignoring a command when rebuilding the database
- [ ] repl
- [ ] interactive mode to handle ambiguities
- [ ] interactive mode possibly for adding new item
- [ ] web UI

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

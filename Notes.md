# OnTopOfThings -- Developer Notes

## Requirements and Ideas

* things to handle:
    * one-time tasks
    * recurring tasks
    * calendar events
    * time tracking for time spent on tasks
    * quantified self goal tracking, like beeminder, connect to beeminder
    * project management
    * ideas lists, maybe even mind mapping
    * exchange between instances in order to have private and shared (or encrypted and unencrypted)
    * pomodoro statistics?
    * scrum stuff?
    * kanban stuff?
* items are either tasks, outcomes, or perhaps external references
* outcomes have 0 or more tasks assigned
* an item can have a parent
* any item with children is automatically an outcome
* items can be assigned to roles, contexts
* realms for roles: self, interpersonal, professional, extended
* projects can be of two types: ones with a definite end, or on-going
* What to name on-going projects?  duty, responsibility, assignment, undertaking, activity
* lots of different lists:
    * next for tasks that should be done soon
    * maybe-week for weekly review
    * maybe-month for monthly review
    * maybe-3mth
    * maybe-year
* schedule/calendar
    * tasks for a specific day or time
    * tasks due by a specific day or time
    * some tasks shouldn't be shown until a certain day/time, because we don't need to plan ahead for them
    * tasks can have a partial order (i.e., assign 'before' property)
    * "next" list for the week
    * have templates from which items can be added to the schedule
* outcome (multi-step task), projects (complex outcomes?), responsibilities (housework), activities (taiji, fitness), roles (father), topics (medical, health)
* arbitrary time segmentation
    * time can be segmented into years, seasons, months, weeks, days
    * days can be segmented, and tasks can be assigned to the segments, e.g.:
    * generic sometime (tasks which are not assigned to a specific segment)
    * getting ready in the morning
    * work
    * evening
    * before bed
* goal tracking (ala Beeminder)
* time tracking
* might be nice to make it multi-user
* item properties:
    * UUID
    * description
    * type (realm, role, context, user, list, task, (bug) ticket, goal, outcome, project, activity, external reference, event)
    * isTemplate?
    * importance/reward [H/M/L/null]
    * critical/penalty [H/M/L/null]
    * urgency [H/M/L/null]
    * description
    * after/dependsOn (UUID)
    * status (open, closed, deleted)
    * stage/heading/class/classification/category/basket/group/grouping/actionstatus/horizon/frame/scope/zone/dominion/sphere/aspect/realm/span/range/timespan/stretch/timeframe
    * grouping: queue, calendar, review, reference, null
    * stage (new, external, maybe, incubator/standby/hold/limbo/gestation/backlog, queue, today, now, done)
    * stage (new, external, review, queue, today, now, done)
    * close reason (for tickets/bugs: fixed, won't fix, not a bug, ...)
    * close time
    * due
    * defer/ignore/forget/hide/wait/postpone/delay/tickler till date
    * readiness: ready/actionable, waiting/external, needs to be decomposed into smaller steps
    * original estimate of time required
    * current estimate of time required
    * number of times re-estimated
    * actual time used so far
    * estimated time remaining
    * maybe we should estimate both time and effort?  Or effort, time, and estimated delivery?
    * start time (especially for events)
    * end time (especially for events)
    * tags
    * contexts
    * order in parent, so that the user can manually arrange the items
    * severity (for bugs)
    * flagged: the item has been flagged by some event, such as an external item coming back or a timer
    * reminder: date when the item should be automatically placed in "today"
    * acceptance: new/accepted/rejected; when an item is automatically placed in "inbox" or "today", it is new.  The item should then be accepted or rejected, optionally with a comment
* item property interactions
    * there are lots of interactions between some of the variables (stage, status, close reason, close time); figure this out better
    * some properties are only relevant for some item types.  E.g., stage generally isn't needed for lists
    * what about typical bug status, such as 'accepted'?  I suppose that's a stage?
* item relations:
    * parent/container/folder (realm, role, context, outcome, etc)
    * tag
    * user
    * list
    * relative priorities? For indicating that one task is more important than another?
* special tags:
    * routine
    * review-week, review-month, review-quarter, review-year
    * decide, for things that need to be decided as opposed to acted on
* folders, tags, lists, items, projects
    * a folder is a hierarchical way to organize all the stuff
    * lists are an ordered sequence of items, but where the items may be modified from their original
    * lists are in a folder, but they can contain items from any folder
    * list contents could either be a text file which gets modified or a sequence of items representing the lines of the list
* tasks vs ideas
    * some lists never need to be reviewed regularly -- they're just there for when you want to reference them
    * some items may not need to be reviewed regularly -- they're just there for idea generation
    * each projects should have such lists: brainstorm, reference, tasks
    * so two dimensions:
         * purpose: brainstorm (idea), reference (note), task
         * review: only when needed, year, quarter, month, week, on a specific date, after a specific date, certain intervals
         * for tasks: they may be past the review stage
* revisions:
    * need to think about how to handle revisions.  Maybe need to introduce branches and not just use command times.
    * we might not always want to save fine-grained changes when a user changes ordering on a list, for exampe
    * maybe an "compress" command which would compress commands down, perhaps since previous sync
    * maybe an "apply diff" which would apply a data diff instead of commands
* more features
    * items somehow inherit properties from their parents, if the properies are not set on the items
    * when an item has a due date and a time estimate, show it on the daily list once things start getting tight
    * for tasks with effort estimates and due dates, maybe we can show some useful graph?
    * let user schedule time slots and pomodoros for various days, both today and in advance
    * link files and URLs to items
    * use as an RSS reader and for tagging other websites to read or that are noteworthy
    * scheduled reminders
    * show marginal costs and benefits for comparison of activities
    * team functionality: should maybe use UUIDs for tags/roles/etc so that users can also tag shared items privately; or perhaps we should add a user/group field to the property table.
    * like Anki for scheduled review?
    * Shopping lists
        * recurring and one-time items
        * recurring items can be selected when they start to run low
        * the qualitity to purchase can be selected
        * you can display the list by store
        * check-off while shopping
        * keep track of amounts purchased over time
    * recipes, connected with shopping lists
    * Travel lists
        * Lists of things to pack and take care of
        * Items can be tagged by weather, destination, duration, occasion, participants, etc
        * Should be able to organize according to where item was packed
        * General lists and lists for concrete past/future trips
        * Make it easy to create a new list for a new trip by picking items from the past and/or starting with an existing list as a template
    * calendar for both one-time events and general schedule, where the schedule should be easily modified for deviations on a given day/week/month
* plausibility checks
    * make plausibility checks when setting a date to hide and item till, because hiding is kind of dangerous
    * when a task is marked as done which has incomplete sub-tasks
* software names:
    * Vivia Activity
    * Vivia Activities
    * Vivia Activity Organizer (VAO)
    * Vivia Organizer
    * Vivia Planner
    * Vivia GTD
    * Vivia Todo
    * Vivia Task Organizer
    * Vivia Task Manager
    * Vivia Task
    * Vivia Taskman
    * Vivia Tasker
    * Vivia OnTop (vot)
    * Vivia OnTopOfThings

## GUI Ideas

* Queues: inbox, today, next, scheduled, deferred
* Reviews: adaptive, week, month, quarter, year
* Folders
    * Cabinets (different repositories that can be synced independently; e.g. Private, Family, Work, TeamA)
    * Roles
    * Projects: arbitrary hierarchy of lists and documents
    * References: arbitrary hierarchy of lists and documents
* Calendar, Agenda, Schedule
* Time Management
* Active lists: convenient selection of currently active roles/projects/lists

Also want team functionality, so have the items above for teams too.
Allow for sharing of information between personal and team accounts.
A repository can be bound anywhere in the folder hierarchy, allowing the user
to keep personal information separate from team information.
Item can be moved among repositories.

Want to track age in queue, and age overdue for review.

## Stages

   - it should be possible for the user to define custom stages
   - standard stages are as follows:
   - queues: inbox, today, next
   - calendars: due, schedule
   - review: deferred, adaptive, week, month, quarter, year
   - if something is on the calendar, it needs a date
   - if something is deferred, it needs a date
   - might want to track the duration that an item has spent in a queue
   - an item which has a defer date automatically gets staged in "deferred", but should still show up in grey-out color on the due or schedule calendar, if it has the relevant dates
   - an item which has a due date automatically becomes a project, and the user should say how long beforehand the project should show up as an active project
   - an item which has a start date (but no due date) automatically goes on the schedule
   - probably need to have 'date' and 'time' fields instead of just 'time' fields in Item, since we need to distinguish being an item being scheduled for a given day vs being scheduled at midnight on a given day

## Packing/shopping list

Packing and shopping lists differ from task lists significantly.
They may be based on a database of items which you select among for your next trip.
Each item can be given a trip-specific quantity and other specifications.
For shopping items, you might want to specify which store(s) they can be bought at.
For packing items, you might want to specify which piece of luggage they go in.
You might want to record the prices of shopping items for each store in the database,
and since prices change over time, that would need to be handled appropriately somehow.

When a user looks at a shopping folder, they should see a list of all non-deleted
shopping items, as well as non-closed template lists and special-occasion lists from the past.

Travel lists will include:
* Things to pack
* Things to take care of at various stages before leaving
* Things to do while traveling
So a trip can be thought of as a project with subprojects and due dates.

When something is checked off on the pack or shop list, it should not be hidden.

## Flow from command line to database

* Options: command line options and arguments, current parsed by CmdArgs
* CommandRecord: command line options stored on a file, currently using Aeson JSON
* Command: command record stored in the database, currently using Persistent Sqlite

When creating a new item from the commandline:

* Options
* Options are validated and processed
* Options convert to CommandRecord
* CommandRecord saved to a temporary file
* CommandRecord read in from file
* Verify that CommandRecords are equal
* CommandRecord converted to Command
* Command saved to DB
* Command loaded from DB
* Verify that Commands are equal
* Command converted to a CommandRecord
* Verify that CommandRecords are equal
* CommandRecord converted to Options
* Verify that Options are equal
* Options are validated and processed for modification of DB 'item' and 'property' tables
* items and properties are updated
* if anything went wrong, remove the Command entry and rebuild
* if everything went well, move the CommandRecord file to the appropriate path

When building DB from files:

* CommandRecord read in from file
* CommandRecord converted to Command
* Command saved to DB
* Do that for all CommandRecord files
* Then for all Commands:
* Command loaded from DB
* Command converted to a CommandRecord
* CommandRecord converted to Options
* Options are validated and processed for modification of DB 'item' and 'property' tables
* items and properties are updated

## Command Records

For now I'm saving Change Records, but it might be better to save Command Records instead.

```
{
 format:1, time:"2013-12-28T09:27:34.909Z", user:"default", cmd:"add", args: ["type=item", "title=improve my repos", "tag+repos"]
}
```

Or in card format:

```
F [FORMAT]
T [TIME]
U [USER]
I [UUID FOR THIS DIFF]
P [UUID OF PREVIOUS DIFF]
D [DIFFTYPE=item]
K [KEY=UUID of item]
= type item
+ tag repos
= longtext
  MY LONG TEXT
  AND MORE OF IT
  AND SO ON
```

```
F [FORMAT]
T [TIME]
U [USER]
C [COMMAND]
= type item
+ tag repos
= longtext
  MY LONG TEXT
  AND MORE OF IT
  AND SO ON
```

The above format is inspired by the fossil file format: <http://www.fossil-scm.org/fossil/doc/trunk/www/fileformat.wiki>

## Diff format

Perhaps instead of command records, use diff records.

```
--- /path/to/original ''timestamp''
+++ /path/to/new ''timestamp''
status:
-new
+queue
content:
@@ -22,3 +22,7 @@
 this paragraph needs to
 be changed. Things can
 be added after it.
+
+This paragraph contains
+important new additions
+to this document.
```

## Reports

Sample Report of pending stuff:

Admin (List)
- backup stuff
- 2013-12-25 backup some more
- backup everything (due:2014-01-01)

Kids (List)
- visit museum

Sample schedule:

Today (Phase)
- ...
Next (Phase)


Today report:

Load all tasks and events that meet any of these criteria:
* stage=inbox, waiting, ready, next, or today and defer date is less than tomorrow
* completion date was today
* any events that run today (start/end time overlap this day)
* any items marked as urgent and whose defer date is unset or less than tomorrow
* any items that are due today

Then:
* For all those tickets, also load their parents recursively.
* For all items, recursively find the name of the list
* Sort the tickets 

## Command Line Options

Right now I'm using CmdArgs, which produces nice help output, but doesn't accommodate much of what I'd like.
Search for the library that helps you write your own command option parser.

Ideally, I'd like this:

        otot [root flags] cmd [args and flags]
        otot [root flags] [ID cmd [flags]]

The first line executes a single command which may or may not modify items.
The second time can be used to edit multiple items on a single command line.

Might want to look here for a few ideas about extra commands: <http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/>

## Using fossil

I considered using fossil's ticket system, but abadoned the attempt due to the inability to
effectively create list fields, such as a list of tags or contexts for a task.  Below
are my notes from when I was working on the idea.

Want to create a `ft` script.

Commands:
* add [--<type>]
* log -- add task and mark as done
* find
* modify
* [lists|projects|tasks|events|...]
* show [listname]
* sort by age, priority, context
* report for a day, part of the day, week, month

Consider using node.js to write the script, and optimist as a plugin for parsing options.
Or write it in scala or haskell or C++ for better parsing of times and other shorthand notations.

Put these tasks into icescrum?

- [ ] `add --list "title"`
- [ ] `add "title"`
- [ ] `add "title" --parent XXX`
- [ ] `show [listname|tagname|contextname]`
- [ ] `show`
- [ ] `report today`
- [ ] `close`

```
CREATE TABLE ticket(
  -- Do not change any column that begins with tkt_
  tkt_id INTEGER PRIMARY KEY,
  tkt_uuid TEXT UNIQUE,
  tkt_mtime DATE,
  tkt_ctime DATE,
  -- Add as many fields as required below this line
  type TEXT,
  status TEXT,
  subsystem TEXT,
  priority TEXT,
  severity TEXT,
  foundin TEXT,
  private_contact TEXT,
  resolution TEXT,
  title TEXT,
  comment TEXT,
  -- Added by ellis
  parent TEXT,
  stage TEXT,
  due_date DATE, -- is start date for events
  end_date DATE,
  completion_date DATE,
  defer_date DATE,
  after TEXT, -- this needs to be a list
  estimate INTEGER -- this would be better as a decimal value
);
CREATE TABLE ticketchng(
  -- Do not change any column that begins with tkt_
  tkt_id INTEGER REFERENCES ticket,
  tkt_rid INTEGER REFERENCES blob,
  tkt_mtime DATE,
  -- Add as many fields as required below this line
  login TEXT,
  username TEXT,
  mimetype TEXT,
  icomment TEXT
);
CREATE INDEX ticketchng_idx1 ON ticketchng(tkt_id, tkt_mtime);
```

```
$ fossil ticket add type list title admin
ticket add succeeded for 4da9970d7cd9470b6d4da5979a77e8022f2b8bbd
$ fossil ticket add type task parent 4da9970d7cd9470b6d4da5979a77e8022f2b8bbd title "customize myrepos better"
ticket add succeeded for 801f1657f103a171ecb8f1de325e1aab1d567ea3
$ fossil sqlite3 ../db.fossil 'select * from ticket;' -header
```


## References

* Look at ``fossil`` to see how they manage file formats and ticket changes (<https://www.sqlite.org/debug1/doc/trunk/www/fileformat.wiki>).
* plancake.com looks quite nice; made a google account with the usual password.  Doesn't have hierarchical tasks or any project management.

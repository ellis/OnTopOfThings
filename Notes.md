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
    * after (UUID)
    * stage (inbox, external, pending, next, today, now, completed)
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
* item property interactions
    * 
* item relations:
    * parent (realm, role, context, outcome, etc)
    * tag
    * user
    * list
* more features
    * items somehow inherit properties from their parents, if the properies are not set on the items
    * when an item has a due date and a time estimate, show it on the daily list once things start getting tight
    * for tasks with effort estimates and due dates, maybe we can show some useful graph?
    * let user schedule time slots and pomodoros for various days, both today and in advance
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

## Command Records

For now I'm saving Change Records, but it might be better to save Command Records instead.

```
{
 format:1, time:"2013-12-28T09:27:34.909Z", user:"default", cmd:"add", args: ["type=item", "title=improve my repos", "tag+repos"]
}
```

```
F [FORMAT]
T [TIME]
U [USER]
C [COMMAND]
= type item
+ tag repos
L= title [LEN]
[TEXT OF LENGTH `LEN`]
```

## Change Records

See the fossil file format: <http://www.fossil-scm.org/fossil/doc/trunk/www/fileformat.wiki>

### JSON Change Record

Example of changes to a single entity:

```
{
 format: 1,
 time: "2013-12...",
 id: "UUID",
 properties: ["type=item", "title=improve my repos", "tag+repos"]
}
```

Example of changes to two entities:

```
{
 format: 1,
 time: "2013-12...",
 list: [
  {
   id: "UUID",
   changes: ["type=item", "title=improve my repos", "tag+repos"]
  },
  {
   id: "UUID2",
   changes: [...]
 ]
}
```

### YAML

```
format: 1
time: 2013-12...
id: UUID
properties:
- type = list
- title = admin
---
format: 1
time: 2013-12...
id: UUID
properties:
- type=item
- title=improve my repos
- tag+repos
- context+office
- parent=UUID
---
format: 1
time: 2013-12...
list:
- id: UUID
  properties:
  - something = else
- id: UUID
  ...
```

### Text format

This would be an efficient text format for the change record:

```
F [FORMAT]
U [USER]
T [TIME]
B [TABLE]
I [ID]
= type item
+ tag repos
L= title [LEN]
[TEXT OF LENGTH `LEN`]
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

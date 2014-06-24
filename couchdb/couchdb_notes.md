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

    {"id": "01", "type": "activity", "user": "ellis", "folder": ["eth"], "name": "planning", "status": "open"} 
    {"id": "02", "type": "project", "user": "ellis", "folder": ["eth", "lrn"], "name": "research", "status": "open"} 
    {"id": "03", "type": "task", "user": "ellis", "folder": ["eth", "lrn", "research"], "title": "load peoples summaries of logic", "status": "closed"}
    {"id": "04", "type": "task", "user": "ellis", "folder": ["eth", "lrn", "research"], "title": "copy my logic notes into an text file", "status": "closed"}
    {"id": "05", "type": "project", "user": "ellis", "folder": ["per", "lrn"], "name": "android", "status": "open"} 
    {"id": "06", "type": "task", "user": "ellis", "folder": ["per", "lrn", "android"], "title": "evaluate week 4", "status": "closed"}
    {"id": "07", "type": "project", "user": "ellis", "folder": ["eth", "lrn"], "name": "algo", "status": "open"} 
    {"id": "08", "type": "task", "user": "ellis", "folder": ["eth", "lrn", "algo"], "title": "programming 6 due", "status": "closed"}
    {"id": "09", "type": "project", "user": "ellis", "folder": ["per"], "name": "otot", "status": "open"} 

    {"id": "10", "type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 1, "time": [6, 0], "itemId": "01", "pomodoros": [1], "pomodoroStatus": ["x"]}
    {"id": "11", "type": "scheduleItem", "user": "ellis", "date": "2014-06-22", "index": 2, "time": [6, 25], "itemId": "02", "pomodoros": [1], "pomodoroStatus": ["x"]}
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

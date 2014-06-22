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

    {'user': 'ellis', 'date': '2014-06-22', 'index': 1, 'time': [6, 0], 'itemId': '/work/planning', 'pomodoros': [1], 'pomodoroStatus': ['x']}
    {'user': 'ellis', 'date': '2014-06-22', 'index': 2, 'time': [6, 25], 'itemId': '/work/lrn/research', 'pomodoros': [1], 'pomodoroStatus': ['x']}
    {'user': 'ellis', 'date': '2014-06-22', 'parentId': '...', 'index': 1, 'itemId': '(TASK ID)'}
    {'user': 'ellis', 'date': '2014-06-22', 'parentId': '...', 'index': 2, 'itemId': '(TASK ID)'}
    {'user': 'ellis', 'date': '2014-06-22', 'index': 3, 'time': [7, 15], 'itemId': '(TASK ID)', 'pomodoros': [1]}
    {'user': 'ellis', 'date': '2014-06-22', 'index': 4, 'time': [6, 25], 'text': '@Train'}


Figures I haven't thought about yet:
- storing large binary files
- git versioned files
- contacts

Want to figure out way to handle Schedule.wiki-like interaction
Maybe have a list, and separate list items so that if different clients update items, they can be more easily merged
How to mark an item as done?
How to mark time spent on an item?
How to assign a projected start time to an item?
How to allocate pomodoros to an item?

A list for each day

Difficult features:
- block together shorter tasks for a single pomodoro

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

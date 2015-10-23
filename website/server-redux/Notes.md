# TODOs

* [ ] consider combining action files together into .lst files:
        ``for file in 20150907_161654857-741b3b13bf185b3e048935f0a437b37e.json 20150907_162005502-2c648d243cb1beedc2e19b737bc2c165.json; do cat $file; echo; done``
* [ ] history file needs `ver` parameter for history items
* [ ] in history file, the keys of history item data should be sorted
* [ ] reformat data:
    * [ ] remove `id` from history items
    * [ ] change tag array to tag map
    * [ ] change deleted items to set `closed` time and `closedType: discarded`
    * [ ] temporarily `_.omit(historyData, 'content')` to get rid of obsolete field
* [ ] consider automatically adding field id, creator, created when merging the first history data item
* [ ] allow for history (.hst) files in data dir, output of `printHistoryStream()`
    * [ ] figure out how to load `.hst` file in as newline-separated JSON files
    * [ ] ignore any .json files with earlier dates than the last .hst file
* [ ] try to figure out what searches can be done with JMESPath

Future

* [ ] merge two .hst files
* [ ] consider using a git merge hook to maintain a single JSON file instead of many (see <http://blog.joshhaas.com/2014/06/how-to-merge-json-files-using-git/>)

# File types

* json: contains a single action
* acts: a sorted list of actions (new-line separated) for archival purposes
* hst1: a sorted list of item histories (new-line separated) (format 1)
* dat1: a sorted list of items data with histories (format 1), used for export to another program
* data: JSON export of item data and histories

Lists are sorted by item creation time.

# Terminology

## Problems

I have a problem with the terminology for "hidden" items and "archives".

* hidden: hide, archive, dismiss, close
* archives: archive, detach, store, vault

Currently (2015-10-10), I'm thinking:

* `closed: true` for items which shouldn't be in "active" views
  (what I used to call "archived").
* `status: "null|done|discarded"`


Closed/hidden/dismissed items: items can either be open/active, closed/done/finished/completed, cancelled/deleted/discarded/trashed.
We could also enter a "closing note", such as "decided against", "no longer relevant",
"deadline passed", "not interested", etc.

Here's a big problem with hiding items: it's a very user-specific kind of decision,
and I'm not sure how to handle it when sharing lists among users.

status: open, accepted, rejected, inwork, complete, deleted, concluded

Could have an 'awaiting: {username: ISODATE}' property to signal when a user is requested to view the item.
I don't see how to conveniently clear the flag, however.  Probably needs an extra user interface,
where the user can click standard things like [acknowledge] [close] [close&hide] [re-open], which would
also remove their name from the `awaiting` field.

Could also have item sub-types like in "issue" systems, such as bug, improvement, idea.

# Data storage

"Compress", and "Archive".

As a general rule, keep action histories indefinitely.

The "Compress" procedure does the following:
* for all actions older than a certain time (say a month):
    * create a new `.acts` file and move it to a storage folder
    * delete the `.json` files
    * create a new `.dat1` file corresponding to the actions in the `.acts` file
    * delete older `.dat1` files

"Export":
* for all items meeting some given criteria (e.g. hidden items from 2013, or everything in a specific folder):
    * create a `.data` file for the corresponding actions

"Archive":
* for all items meeting some given criteria (e.g. hidden items from 2013, or everything in a specific folder):
    * make sure that "Compress" was executed up to the date of the modified item,
      because otherwise we won't be able to get rid of the actions that affect
      items that are both in the archive set and outside of it.
    * export the items
    * remove the items

# New format/protocol thoughts

Old:

```json
{"created":"2015-09-20T18:17:49+00:00","creator":"default","folder":["fam","dora"],"horizon":"week","id":"6d1a7e12-d24a-4146-bf37-b50bebda013e","title":"Figure out how to let Dora download youtube videos","type":"task"}
```

New:

The idea is to have history in each item which merely consists of data to `_.merge()` together.

```yaml
id: ...
history:
- user: default
  time: ...
  data: {...}
  note: ...
```

tags need to be objects, and null values lead to removal (since 'undefined' can be used in json)

```yaml
history:
- data:
    tag:
      mustdo: true

{"created":"2015-09-20T18:17:49+00:00","creator":"default","folder":["fam","dora"],"horizon":"week","id":"6d1a7e12-d24a-4146-bf37-b50bebda013e","title":"Figure out how to let Dora download youtube videos","type":"task"}
```


'closed', 'closedType' = 'done|discarded', 'closedReason'

# Parent/child relations

We can either have a parent reference or child references.

## Parent references

Nice an unambiguous.

## Child references

Instead of storing a parent reference, keep a map of children: id -> index:

```yaml
id: ...
type: project
children:
  ABCDEFG: 1
  SDLKFNQ: 2
```

Very convenient.  Allows us to treat folders and lists the same.
It can get messy, though.  For example, if we move old tasks into another
file for storage, do we remove those ids from the child list?  Because if
we do, it'll be harder to view old data.

# JMESPath

Find items in folder '/foo/bar':

        [?folder == `["foo", "bar"]`]

Find items in or below folder '/foo/bar':

        [?folder[:2] == `["foo", "bar"]`]

For horizon <= week

        [?contains(`["today", "next", "week"]`, horizon)]

For tag 'mustdo':

        [?type(tag.mustdo) != 'null']

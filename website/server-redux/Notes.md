# TODOs

* [ ] allow for history (.hst) files in data dir, output of `printHistoryStream()`
* [ ] history file needs `ver` parameter for history items
* [ ] in history file, keys of history item data should be sorted
* [ ] remove `id` from history items
* [ ] reformat data:
    * [ ] temporarily `_.omit(historyData, 'content')` to get rid of obsolete field
    * [ ] change deleted items to set `closed` time and `closedType: discarded`
* [ ] consider automatically adding field id, creator, created when merging the first history data item
* [ ] figure out how to load `.hst` file in as newline-separated JSON files

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


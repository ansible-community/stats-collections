# Data sources

## collections_list.csv

This list specifies the Collections index - these repositories will be crawled
for data, which is consumed by the dashboard. The format is a CSV file, with
these fields:

| *Field* | *Purpose* |
|---------|-----------|
| *Site*  | The base URL for the Git repo
| *Org*   | The organisation (optional)
| *Repo*  | The repository
|* *NewCollection* | If `TRUE` then don't try to find the history of this collection in `ansible/ansible` |
| *Regex* | Used to find (roughly) where the Collection used to live in `ansible/ansible |
| *MergeKey* | If present, combines several repos into a single graph |

Pull requests are welcome to extend the list of watched Collections

## 2021-11-29 

### CSV file import

Use the 'import' button on the 'clonks' view to pull in a csv file.

Files must be in a 3 column format containing task, from datetiem, and to datetime.

Extra columns are ignored.  You have to have a header row as in the following example:


```
task,from,to
spinning up on automato meeting, 2021-08-25 17:48:08, 2021-08-25 18:57:09, 1.15, 1.15, 1.15
spinning up on automato meeting 2, 2021-09-05 12:09:44, 2021-09-05 12:49:57, 0.67, 0.67, 0
automato hello world, 2021-09-07 13:00:17, 2021-09-07 13:07:38, 0.12, 0, 
automato hello world, 2021-09-07 17:06:45, 2021-09-07 17:27:13, 0.34, , 
automato hello world, 2021-09-07 20:36:22, 2021-09-07 21:18:05, 0.7, 1.16, 
```


## 2021-11-23

### ctrl-s for save

Use a ctrl-s shortcut for saving on ProjectTime and ProjectEdit pages.


### revert

added a revert button to discard changes that haven't been saved yet.

### ignore rows

you can ignore rows in your timelog so that they don't count towards your total hours.  This is
for a workflow where you log everything by default, then go back and ignore things that you don't want to count as
paid hours.

### checked rows.

you can check rows in pay and clonk modes to delete them (and ignore in clonk mode).

### duration editing

instead of editing the end date, you can enter a duration number.

## 2021-11-21

### better payment/time editing

Editing the start date caused the timelog row to jump around and lose focus.
Added an OK button before accepting start date edit.

## 2021-11-21

Default to last task for 'current task' string.


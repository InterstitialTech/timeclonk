## 2021-12-19

Copied the elm-ui checkbox 'icon' function and darkened it up a bit to make it more visible.

## 2021-12-17

Replaced hardcoded spacing with a default spacing var in TcCommon.  Increased spacing a bit.

Tweaked flake.nix to include cargo-watch, which is needed by server/watch-run.sh.

Various other style changes for headers.

## 2021-12-08

The old payments view is now "distributions", which is modeled on the confusing spreadsheet I was using before this.

There's a new payments view, which is just payments, and not payments + worked time together.

## 2021-12-07

route support for projectedit, projectime, and the 3 current modes of projecttime.  Also projecttime
no longer resets to clonks mode when you save.


## 2021-12-06

better error message for bad login uid or pwd.  was 'no rows'.

## 2021-12-02

added allocation CSV import.  uses the same import button as the clonks one.  format is:

```
description,date,hours
Pareto Security,2020-03-12 14:42:00,44
Gasper Vozel,2020-03-12 15:09:00,5.5
Daniel Rafaj,2020-03-18 11:12:00,1
Amine Chikhaoui,2020-03-18 12:31:00,1.5
```

## 2021-12-01

### Allocations

Allocations.  These indicate that a block of hours is allotted for working.   The idea with allocations is that
you note the number of hours in the current contract, or the number of hours that
have been funded, or similar.

So now there are Clonks, Payments, and Allocations views on the main project page.

Allocations are entered and viewed on the Allocations page.  They also
appear in the totals on the Clonks and Payments views, to make it easy to track how many hours are left to work.

## 2021-11-30 

### CSV file export

Pretty straightforward.  Check rows in the clonks view, hit export, get file named 'timeclonk.csv'.

## 2021-11-29 

### CSV file import

Use the 'import' button on the 'clonks' view to pull in a csv file.

Files must be in a 3 column format containing task, 'from' datetime, and 'to' datetime.

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

## 2021-11-22

### better payment/time editing

Editing the start date caused the timelog row to jump around and lose focus.
Added an OK button before accepting start date edit.

## 2021-11-21

Default to last task for 'current task' string.



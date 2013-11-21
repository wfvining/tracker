\Tracker
=======

Tracker is a simple task tracker system.

With tracker you can create tasks and incrementally track progress to
completion. You do this by specifying how many sub-tasks are required
for completion, and advancing the number of completed tasks as you
complete them. 

For example the judge sentences you to 50 hours of community
service. Simply create a task.

```
$ tracker add community-service 50
```

When you get home from working on the litter crew:

```
$ tracker advance community-service 4
```

Check your standing.

```
$ tracker view community-service
```

(Idea: add tracker to your init file so every time you log in you are
reminded of your debt to society!)

Tasks reside in particular directories, so you can create tasks for
your projects that you don't see unless you are in the project
directory and don't interfer with your personal tasks in your home
directory.

# Setup

```
$ ghc --make tracker.hs
$ mv tracker $SOMEWHERE_IN_MY_PATH
```

# Usage

The basic Tracker commands are 'add', 'ammend', 'advance', 'view', and 'remove'.

## Add a New Task

```
add <task-name> <sub-tasks> : create a new task
```

`<task-name>` is a the name of the new task.

`<sub-tasks>` is an integer giving the number of sub-tasks for completion.

NOTE: It is an error to provide anything other than and integer;
however, Tracker will still create the task (this is a bug). When
you try to advance such a task everything will blow up in your
face...

## Change the Number of Sub-Tasks

```
 ammend <task-name> <new-sub-task-count> : change the number of sub-tasks
```

## Note Progress

```
 advance <task-name> <amount-to-advance> [optional comment]
```
 
`<task-name>` is the name of the task. duh.

`<amount-to-advance>` an integer, the number of subtasks to 'tick off'


An optional comment may be provided to be stored with the amount
of the advance, describing what you did and how you feel about it.

## View Progress

```
view [<task-name>]
```

Show stats about a task, the percent completed and the number of
remaining sub-tasks. When called without a task name the status of all available tasks is displayed.

## Remove a Task

```
remove [-q] <task-name>
```

## List

To get a list of available tasks simply run tracker with no arguments.

```
$ tracker
Available tasks:
  foo
  bar
  ...
```

Delete a task. If the -q option is not provided will prompt user
for confirmation.

# To Do

This is version 0.1 (maybe lower). Much remains to be done including:

- [ ] Pretty printing and progress bars for view.
- [x] List all tasks.
- [ ] ~~Fix that bug in 'add'~~
- [x] Consider allowing fractional tasks to track time instead of fixing that bug.
- [x] view all tasks in a directory and their status.


  

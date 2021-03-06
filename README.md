Tracker
=======

Tracker is a simple task tracker system.

With tracker you can create tasks and incrementally track progress to
completion. You do this by specifying how many sub-tasks are required
for completion, and advancing the number of completed tasks as you
complete them. 

For example the judge sentences you to 50 hours of community
service. Simply create a task.

```
$ tracker add penance 50
```

When you get home from working on the litter crew:

```
$ tracker advance penance 4
```

Check your standing.

```
$ tracker view penance
8.0% complete with 46.0 tasks remaining.
```

Let's say you loose it one day and start throwing litter at the passing 
cars. For this the judge gives you an additional 10 hours. You can
amend your penance task to accomodate this.

```
$ tracker amend penance +10
```

Subsequently you see the error in your ways and a kinder judge decides
you were sentenced too harshly, removing 5 hours from you sentence.

```
$ tracker amend penance -5
```

Tasks reside in particular directories, so you can create tasks for
your projects that you don't see unless you are in the project
directory and don't interfer with your personal tasks in your home
directory.

## Setup

```
$ ghc --make tracker.hs
$ mv tracker $SOMEWHERE_IN_MY_PATH
```

## Usage

The basic Tracker commands are 'add', 'amend', 'advance', 'view', and 'remove'.

### Add a New Task

```
add <task-name> <sub-tasks>
```

`<task-name>` is a the name of the new task.

`<sub-tasks>` is the number of sub-tasks required for completion.

### Change the Number of Sub-Tasks

```
amend <task-name> [+|-]<new-sub-task-count>
```

If `<new-sub-task-count>` is prefixed by `+` then the number of sub-tasks
will be incremented by the specified amount. A `-` prefix will decrement
the number of sub-tasks. If neither is specified then `<new-sub-task-count>`
will replalce the current sub-task-count.

### Note Progress

```
advance <task-name> <amount-to-advance> [optional comment]
```
 
`<task-name>` is the name of the task. duh.

`<amount-to-advance>` the number of subtasks to 'tick off'


An optional comment may be provided to be stored with the amount
of the advance, describing what you did and how you feel about it.

### View Progress

```
view [<task-name>]
```

Show stats about a task, the percent completed and the number of
remaining sub-tasks. When called without a task name the status of all available tasks is displayed.

### Remove a Task

```
remove [-q] <task-name>
```

Delete a task, prompting the user for confirmation. The `-q` flag will suppress this behavior,
deleting the task without any interaction.

### List

To get a list of available tasks simply run tracker with no arguments.

```
$ tracker
Available tasks:
  foo 25% complete with 3.0 tasks remaining.
  bar 99% complete with 0.5 tasks remaining.
  ...
```


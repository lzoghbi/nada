# nada

## Installing and running

Clone this repository

```bash
$ git clone https://github.com/lzoghbi/nada.git
```

Navigate to `nada` and build using `stack`.

```bash
$ stack build
```

Execute using `stack`.

```bash
$ stack exec nada
```

## Project description

Our project is an implementation of a to-do list on the command line. There are
two components that we are considering for the project: a CLI component for
simple commands such as “add task” or “complete task” as well as a
fully-featured TUI component utilizing brick. The latter will involve most of
the features, which we hope to incrementally develop. Our goal for this
application is to provide an intuitive and useful interface for users to
indicate tasks, priorities, completion dates, etc. that will also help users
stay on top of their work. We intend to draw inspiration from other to-do list
applications such as Microsoft To Do or Emacs’s org-mode.

Below are some of the features we wish to support. As this project lends itself
well to incremental development, we are explicitly not setting out to support
every single feature, but instead as many as we can. At a minimum, we will
support creating, editing, and displaying to-dos, although we expect to support
much more than this.

### Planned Features
- To-do lists
  - Categories
    - Each task can be part of a specific category
    - Or also we can have a tag system (multiple tags for a single task)
  - Task can cross reference
  - Progress indicator
    - Not started, in progress, done
  - Nested tasks
  - Due date
    - Integrates with calendar
  - Priority
    - UI indicator for this
- Calendar
  - Multiple views
    - Day, week, month, …
  - Tasks show up in calendar when they have a due date
- Habit tracker
  - Streaks show in calendar
  - Habit stats
- UI
  - Multipane/multimodal views
  - Keyboard shortcuts + mouse support
  - Searching/fuzzy searching among tasks/task files
- CLI
  - Shell prompt widgets
  - Commands
    - Add task
    - Next task
    - Task done
    - Shell completion / fuzzy search

# arr[^1]

![arr](https://raw.githubusercontent.com/cl-sdk/arr/refs/heads/main/assets/github-banner.png "arr")

Common lisp background workers, eventually a task queue and task orchestration (coordination).

---

## Usage

> NOTE: Your application must handle process termination signals to stop the application.

### `arr.background-worker`

There are 2 function to start and stop the background worker:

#### Start

```lisp
(arr.background-worker:start-application &key (number-of-workers 1))
```

#### Stop

```lisp
(arr.background-worker:stop-application app)
```

### `arr.global-background-worker`

Same as `arr.background-worker`, but the state is controlled internally
on this package.

#### Start

```lisp
(arr.global-background-worker:start-application &key (number-of-workers 1))
```

#### Stop

```lisp
(arr.global-background-worker:stop-application)
```

### Custom implementation

All the generics are available in other to build your worker.

#### Task implementation

`task` is a generic definition and can be executed by all workers available.

It is (supposed) a stateless function that is fire and forget.

```lisp
(defgeneric task (task-name task-data &key time &allow-other-keys))
```

#### Scheduler, enqueueing, dequeueing and runner worker function

`enqueue-task` and `dequeue-task` especified by the data store implementation.

```lisp
(defgeneric enqueue-task (data-source task &key app &allow-other-keys))
```

```lisp
(defgeneric dequeue-task (data-source &key app &allow-other-keys))
```

`task-runner` is the engine of the dispatcher worker. At this moment, all tasks, immediate or schedules, are ready to dispatch.

```lisp
(defgeneric task-runner (app &key &allow-other-keys))
```

Just like `task-runner`, `task-scheduler` is the engine of the scheduling and queueing tasks when they are ready to be dispatched.

```lisp
(defgeneric task-scheduler (app &key &allow-other-keys))
```

`schedule-task` receives the "task plan":

- How it should be scheduled
- Task data

```lisp
(defgeneric schedule-task (data-source scheduled-time task &key app &allow-other-keys))
```

##### Enqueueing tasks

```lisp
(defgeneric execute-task (app task-name &optional data))
```

```lisp
(defgeneric execute-task-at (app time task-name &optional data))
```

---

## License

[Unlicense](https://github.com/cl-sdk/arr/blob/main/license)

[^1]: `arr` is the name of the function of the haskell's `Control.Arrows` package, that lifts a pure function into a computation.
